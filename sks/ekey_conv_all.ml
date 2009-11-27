open ExtList
open Printf

open Format
open Sexplib
open Sexp
open Conv
open Misc
open Ekey_conv_aux
open Ekey

(* v3 keys specify expiration time as number of days relative to the creation time *)
let v3_absolute_expire_date pubkeyinfo =
  match pubkeyinfo.Packet.pk_expiration with
    | Some d ->
	let ctime = Int64.to_float pubkeyinfo.Packet.pk_ctime in
	let valid = (float_of_int d) *. 24. *. 3600. in
	  Some (ctime +. valid)
    | None -> None

let get_revocation_date selfsigs =
  let selfsigs = sort_reverse_siginfo_list selfsigs in
  let pred siginfo =
    siginfo.Index.sigtype = 0x20
  in
    try
      let revocation_sig = List.find_exc pred Not_found selfsigs in
	i64_to_float_option revocation_sig.Index.sig_creation_time
    with Not_found -> None

type sig_status = 
    Valid_foreignsig of esiginfo
  | Valid_selfsig of bool * (float option) 
  | Revoked of float
  | Irrelevant_sigtype

(* return true if this is the puid, false otherwise *)
let get_self_sig_status siginfo keyid =
  match siginfo.Index.sigtype with
    | 0x30 ->
	(* uid is revoked *)
	Revoked (Option.get (i64_to_float_option siginfo.Index.sig_creation_time))
    | 0x10 | 0x11 | 0x12 | 0x13 ->
	let key_exptime = i64_to_float_option siginfo.Index.key_expiration_time in
	  Valid_selfsig ((siginfo.Index.is_primary_uid, key_exptime))
    | 0x18 | 0x28 ->
	(* subkeys -> ignore *)
	Irrelevant_sigtype
    | _ as t -> 
	printf "handle_self_sig: unexpected signature type 0x%x\n" t;
	Irrelevant_sigtype

(* return Some esig if this signature is valid, None otherwise *)
let get_foreign_sig_status siginfo =
  match siginfo.Index.sigtype with
    | 0x30 ->
	(* sig is revoked -> don't consider this issuer for further sigs *)
	Revoked (Option.get (i64_to_float_option siginfo.Index.sig_creation_time))
    | 0x10 | 0x11 | 0x12 | 0x13 ->
	(* include expired signatures *)
	Valid_foreignsig (siginfo_to_esiginfo siginfo)
    | t ->
	(* skip unexpected/irrelevant sig type *)
	Irrelevant_sigtype

(* if we encounter a cert revocation, search for the revoked sig and include it
   together with the revoke time. *)
let handle_foreign_sig own_keyid current_sig remaining_sigs ignore_set =
  let issuer_keyid = Option.get current_sig.Index.keyid in
    match get_foreign_sig_status current_sig with
      | Valid_foreignsig esiginfo -> ((Keyid_set.add issuer_keyid ignore_set), (Some esiginfo))
      | Revoked time ->
	  (* search backwards in time for the revoked signature *)
	  (try 
	    let pred s = ((Option.get s.Index.keyid) = issuer_keyid) in
	    let revoked_sig = List.find_exc pred Not_found remaining_sigs in
	    let sigtype = revoked_sig.Index.sigtype in
	      if sigtype >= 0x10 && sigtype <= 0x13 then
		let esiginfo = 
		  Some ({ (siginfo_to_esiginfo revoked_sig) with sig_revoktime = Some time }) in
		  ((Keyid_set.add issuer_keyid ignore_set), esiginfo)
	      else (
		printf "%s found no sig for sig revocation\n" (keyid_to_string own_keyid);
		(ignore_set, None))
	  with Not_found -> 
	    printf "%s found no sig for sig revocation\n" (keyid_to_string own_keyid);
	    ((Keyid_set.add issuer_keyid ignore_set), None))
      | Irrelevant_sigtype -> 
	  (ignore_set, None)
      | Valid_selfsig _ -> 
	  failwith "encountered Valid_selfsig in foreign sig -> WTF?"

let extract_sigs keyid siglist pubkey_info =
  let rec handle results l =
    let (esigs, ignore_set, is_puid, valid_selfsig, keyexptime) = results in
      match l with
	| siginfo :: tl -> 
	    if is_signature_valid siginfo then
	      let issuer_keyid = Option.get siginfo.Index.keyid in
		if Keyid_set.mem issuer_keyid ignore_set then
		  handle (esigs, ignore_set, is_puid, valid_selfsig, keyexptime) tl
		else
		  if keyid = issuer_keyid then
		    match get_self_sig_status siginfo keyid with
		      | Revoked _ -> raise (Skip_uid "uid revoked")
		      | Irrelevant_sigtype -> 
			  handle results tl
		      | Valid_foreignsig _ ->
			  failwith "encountered Valid_foreignsig in foreign sig -> WTF?"
		      | Valid_selfsig (is_puid, keyexptime) ->
			  let ignore_set = Keyid_set.add keyid ignore_set in
			    handle (esigs, ignore_set, is_puid, true, keyexptime) tl
		  else
		    let (ignore_set, sig_option) = handle_foreign_sig keyid siginfo tl ignore_set in
		      match sig_option with
			| Some esiginfo ->
			    let esigs = Signature_set.add (issuer_keyid, esiginfo) esigs in
			      handle (esigs, ignore_set, is_puid, valid_selfsig, keyexptime) tl
			| None ->
			    handle (esigs, ignore_set, is_puid, valid_selfsig, keyexptime) tl
			    
	    else
	      handle (esigs, ignore_set, is_puid, valid_selfsig, keyexptime) tl
	| [] -> (esigs, is_puid, valid_selfsig, keyexptime)
  in
  let siglist_reverse = sort_reverse_siginfo_list siglist in
  let start = (Signature_set.empty, Keyid_set.empty, false, false, None) in
  let (esigs, is_puid, valid_selfsig, keyexptime) = handle start siglist_reverse in
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
    let v3_expiry_date = v3_absolute_expire_date pubkey_info in
    let revocation_time = get_revocation_date sig_pkey.info_selfsigs in
    let start = (Signature_set.empty, None, [], false, None) in
    let (sigs, puid_option, uids, valid_selfsig, exptime) = 
      List.fold_left handle_uid start sig_pkey.info_uids 
    in
      if valid_selfsig then
	let puid = 
	  match puid_option with
	    | None -> 
		(* has a valid selfsig but no puid -> choose one *)
		List.hd uids
	    | Some uid ->
		uid
	in
	let exptime =
	  match pubkey_info.Packet.pk_version with
	    | 4 -> exptime
	    | 2 | 3 -> v3_expiry_date
	    | _ -> raise (Skip_key (Unparseable, "Unparseable: unknown version"))
	in
	let siglist = Signature_set.elements sigs in
	let algo = pubkey_info.Packet.pk_alg in
	let keylen = pubkey_info.Packet.pk_keylen in
	let ctime = Int64.to_float pubkey_info.Packet.pk_ctime in
	let pki = 
	  { key_keyid = keyid; 
	    key_puid = puid; 
	    key_alg = algo; 
	    key_len = keylen;
	    key_ctime = ctime;
	    key_all_uids = uids;
	    key_exptime = exptime;
	    key_revoktime = revocation_time;
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
