open ExtList
open Printf

open Format
open Sexplib
open Sexp
open Conv
open Misc

open Ekey
open Ekey_conv_aux

let is_v3_expired pubkey_info =
  match pubkey_info.Packet.pk_expiration with
    | Some d -> 
	let today = Stats.round_up_to_day (Unix.gettimeofday ()) in
	let ctime = Int64.to_float pubkey_info.Packet.pk_ctime in
	let valid = (float_of_int d) *. 24. *. 3600. in
	  ctime +. valid < today
    | None -> 
	false

let is_signature_expired siginfo =
  let today = Stats.round_up_to_day (Unix.gettimeofday ()) in
    match siginfo.Index.sig_creation_time with
      | Some ctime ->
	  begin
	    match siginfo.Index.sig_expiration_time with
	      | Some exptime ->
		  if compare (Int64.add ctime exptime) (Int64.of_float today) <= 0 then
		    true
		  else
		    false
	      | None ->
		  false
	  end
      | None -> false

let is_key_expired ctime siginfo =
  let today = Stats.round_up_to_day (Unix.gettimeofday ()) in
    match siginfo.Index.key_expiration_time with
      | Some exptime ->
	  if compare (Int64.add ctime exptime) (Int64.of_float today) <= 0 then
	    true
	  else
	    false
      | None ->
	  false
      
let sig_is_revok siginfo =
  match siginfo.Index.sigtype with
    | 0x20 | 0x28 | 0x30 -> true
    | _ -> false

let is_revoked pkey = 
  let selfsigs = pkey.KeyMerge.selfsigs in
    List.exists (fun sign -> 
		   sig_is_revok (Index.sig_to_siginfo sign)
                )
      selfsigs

let is_revoked_pkey_siginfo k =
  List.exists sig_is_revok k.info_selfsigs

let siginfo_to_esiginfo siginfo =
  { sig_puid_signed = false; 
    sig_level = siginfo.Index.sigtype; 
    sig_hash_alg = siginfo.Index.siginfo_hash_alg;
    sig_pk_alg = siginfo.Index.siginfo_pk_alg;
    sig_ctime = i64_to_float_option siginfo.Index.sig_creation_time;
    sig_exptime = i64_to_float_option siginfo.Index.sig_expiration_time;
    sig_revoktime = None;
  }

(* returns true if the key (for v4 keys) or signature is expired *)
let check_expired ctime signature =
  if is_signature_expired signature then
    raise (Skip_uid "most recent self-signature expired")
  else
    if is_key_expired ctime signature then
      raise (Skip_key (Expired, "key expired"))
    else
      ()

(* return true if this is the puid, false otherwise *)
let handle_self_sig pubkey_info ignore_issuers signature issuer_keyid =
  match signature.Index.sigtype with
    | 0x20 ->
	(* key is revoked - can this appear in a uid list? *)
	raise (Skip_key (Revoked, "key revoked (0x20)"))
    | 0x30 ->
	(* uid is revoked *)
	raise (Skip_uid "uid is revoked (0x30)")
    | 0x10 | 0x11 | 0x12 | 0x13 ->
	check_expired pubkey_info.Packet.pk_ctime signature;
	let exptime = i64_to_float_option signature.Index.key_expiration_time in
	  Some ((signature.Index.is_primary_uid, exptime))
    | 0x18 | 0x28 ->
	None
    | _ as t -> 
	printf "handle_self_sig: unexpected signature type 0x%x\n" t;
	None

(* return Some esig if this signature is valid, None otherwise *)
let handle_foreign_sig signature issuer_keyid =
  match signature.Index.sigtype with
    | 0x30 ->
	(* sig is revoked -> don't consider this issuer for further sigs *)
	None
    | 0x10 | 0x11 | 0x12 | 0x13 ->
	if is_signature_expired signature then
	  (* sig is expired -> don't consider this issuer for further sigs *)
	  None
	else
	  Some (issuer_keyid, siginfo_to_esiginfo signature)
    | t ->
	(* skip unexpected/irrelevant sig type *)
	None

let extract_sigs keyid siglist pubkey_info =
  let handle_signature (esigs, ignore_issuers, is_puid, valid_selfsig, keyexptime) signature =
    if is_signature_valid signature then
      let issuer_keyid = Option.get signature.Index.keyid in
	if Keyid_set.mem issuer_keyid ignore_issuers then
	  (esigs, ignore_issuers, is_puid, valid_selfsig, keyexptime)
	else
	  if keyid = issuer_keyid then
	    match handle_self_sig pubkey_info ignore_issuers signature issuer_keyid with
	      | Some (is_puid, new_exptime) ->
		  let ignore_issuers = Keyid_set.add issuer_keyid ignore_issuers in
		    (esigs, ignore_issuers, is_puid, true, new_exptime)
	      | None ->
		  (esigs, ignore_issuers, is_puid, valid_selfsig, keyexptime)
	  else 
	    match handle_foreign_sig signature issuer_keyid with
	      | Some esig -> (Signature_set.add esig esigs, ignore_issuers, is_puid, valid_selfsig, keyexptime)
	      | None -> (esigs, ignore_issuers, is_puid, valid_selfsig, keyexptime)
    else
      (esigs, ignore_issuers, is_puid, valid_selfsig, keyexptime)
  in
  let siglist_reverse = sort_reverse_siginfo_list siglist in
  let (esigs, _, is_puid, valid_selfsig, keyexptime) = 
    let start = (Signature_set.empty, Keyid_set.empty, false, false, None) in
      List.fold_left handle_signature  start siglist_reverse in
    (esigs, is_puid, valid_selfsig, keyexptime)

let handle_uid pkey pubkey_info (sigs, puid, uids, valid_selfsig, exptime) (uid_packet, siglist) =
  match uid_packet.Packet.packet_type with
    | Packet.User_ID_Packet ->
	(try 
	  let uid = uid_packet.Packet.packet_body in
	  let keyid = Fingerprint.keyid_from_packet pkey.KeyMerge.key in
	  let (new_sigs, puid_flag, valid_selfsig, keyexptime) = 
	    extract_sigs keyid siglist pubkey_info in
	  let sigs = Signature_set.union sigs new_sigs in
	  let uids = uid :: uids in
	  let puid = 
	    if puid_flag then
	      match puid with
		| Some puid -> Some uid
		| None -> Some uid
	    else
	      puid
	  in
	    (sigs, puid, uids, valid_selfsig, keyexptime)
	with Skip_uid s -> (sigs, puid, uids, valid_selfsig, None))
    | Packet.User_Attribute_Packet ->
	(sigs, puid, uids, valid_selfsig, exptime)
    | _ ->
	failwith "key_to_ekey: unexpected packet type in uid list"

let key_to_ekey key =
  try
    let pkey = KeyMerge.parse_keystr (KeyMerge.key_to_stream key) in
    let keyid = Fingerprint.keyid_from_packet pkey.KeyMerge.key in
    let pubkey_info = ParsePGP.parse_pubkey_info pkey.KeyMerge.key in
    let handle_uid = handle_uid pkey pubkey_info in
    let sig_pkey = pkey_to_pkey_siginfo pkey in
      if is_v3_expired pubkey_info || is_revoked_pkey_siginfo sig_pkey then
	raise (Skip_key (Expired, "key expired"))
      else if is_revoked_pkey_siginfo sig_pkey then
	raise (Skip_key (Revoked, "key revoked"))
      else
	let start = (Signature_set.empty, None, [], false, None) in
	let (sigs, puid_option, uids, valid_selfsig, exptime) = 
	  List.fold_left handle_uid start sig_pkey.info_uids in
	  if valid_selfsig then
	    let puid = 
	      match puid_option with
		| None -> 
		    List.hd uids
		| Some uid ->
		    uid
	    in
	    let exptime =
	      match pubkey_info.Packet.pk_version with
		| 4 -> exptime
		| 2 | 3 -> int_to_float_option pubkey_info.Packet.pk_expiration
		| _ -> raise (Skip_key (Unparseable, "Unparseable: unknown version"))
	    in
	    let siglist = Signature_set.elements sigs in
	    let algo = pubkey_info.Packet.pk_alg in
	    let keylen = pubkey_info.Packet.pk_keylen in
	    let ctime = Int64.to_float pubkey_info.Packet.pk_ctime in
	    let pki = 
	      { key_keyid = keyid;
		key_version = pubkey_info.Packet.pk_version;
		key_puid = puid; 
		key_alg = algo; 
		key_len = keylen;
		key_ctime = ctime;
		key_all_uids = uids;
		key_exptime = exptime;
		key_revoktime = None;
	      }
	    in
	      { pki = pki; signatures = siglist }
	  else
	    raise (Skip_key (No_valid_selfsig, "key_to_ekey: found no valid selfsignature -> skip key"))
  with
    | Skip_key (reason, s) ->
	let keyid = Fingerprint.keyid_from_packet (List.hd key) in
	  raise (Skipped_key (reason, keyid))
    | ParsePGP.Overlong_mpi 
    | Unparseable_signature_packet 
    | Signature_without_creation_time ->
	let keyid = Fingerprint.keyid_from_packet (List.hd key) in
	  raise (Skipped_key (Unparseable, keyid))
