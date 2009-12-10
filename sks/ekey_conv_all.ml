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
let get_self_sig_status siginfo own_keyid =
  match siginfo.Index.sigtype with
    | 0x30 ->
	(* uid is revoked *)
	(try
	  Revoked (Option.get (i64_to_float_option siginfo.Index.sig_creation_time))
	with Option.No_value ->
	  failwith "get_self_sig_status: sig without creation time")
    | 0x10 | 0x11 | 0x12 | 0x13 ->
	let key_exptime = i64_to_float_option siginfo.Index.key_expiration_time in
	  Valid_selfsig ((siginfo.Index.is_primary_uid, key_exptime))
    | 0x18 | 0x28 ->
	(* subkeys -> ignore *)
	Irrelevant_sigtype
    | _ as t -> 
	printf "handle_self_sig: %s unexpected signature type 0x%x\n" (keyid_to_string own_keyid) t;
	Irrelevant_sigtype

(* return Some esig if this signature is valid, None otherwise *)
let get_foreign_sig_status siginfo =
  match siginfo.Index.sigtype with
    | 0x30 ->
	(* sig is revoked -> don't consider this issuer for further sigs *)
	(try
	  Revoked (Option.get (i64_to_float_option siginfo.Index.sig_creation_time))
	with Option.No_value ->
	  failwith "get_foreign_sig_status: sig without creation time")
    | 0x10 | 0x11 | 0x12 | 0x13 ->
	(* include expired signatures *)
	Valid_foreignsig (siginfo_to_esiginfo siginfo)
    | t ->
	(* skip unexpected/irrelevant sig type *)
	Irrelevant_sigtype

let search_self_sig own_keyid siglist =
  let rec loop siglist =
    match siglist with
      | signature :: tl ->
	  if is_signature_valid signature then
	    let issuer_keyid = Option.get signature.Index.keyid in
	      if own_keyid = issuer_keyid then
		(* this is a selfsig *)
		match get_self_sig_status signature own_keyid with
		  | Revoked revoktime -> None
		  | Valid_selfsig (puid_flag, exptime) -> Some (puid_flag, exptime)
		  | Irrelevant_sigtype -> loop tl
		  | Valid_foreignsig _ -> failwith (sprintf "%s encountered foreign sig with own keyid" (keyid_to_string own_keyid))
	      else
		(* not a selfsig *)
		loop tl
	  else
	    loop tl
      | [] -> 
	  printf "%s siglist length %d\n" (keyid_to_string own_keyid) (List.length siglist);
	  None
  in
    loop siglist

(* if we encounter a cert revocation, search for the revoked sig and include it
   together with the revoke time. *)
let handle_foreign_sig own_keyid current_sig remaining_sigs ignore_set =
  let issuer_keyid = 
    try 
      Option.get current_sig.Index.keyid 
    with Option.No_value -> 
      failwith "handle_foreign_sig: sig without keyid"
  in
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
	   with 
	       Not_found -> 
		 printf "%s found no sig for sig revocation\n" (keyid_to_string own_keyid);
		 ((Keyid_set.add issuer_keyid ignore_set), None)
	     | Option.No_value ->
		 printf "%s has sig without keyid \n" (keyid_to_string own_keyid);
		 (ignore_set, None))
      | Irrelevant_sigtype -> 
	  (ignore_set, None)
      | Valid_selfsig _ -> 
	  failwith "encountered Valid_selfsig in foreign sig -> WTF?"

let collect_foreign_sigs own_keyid siglist =
  let rec loop raw_siglist collected_sigs ignore_set =
    match raw_siglist with
      | signature :: tl ->
	  if is_signature_valid signature then
	    (* sigs without keyid are excluded via is_signature_valid *)
	    let issuer_keyid = Option.get signature.Index.keyid in
	      if Keyid_set.mem issuer_keyid ignore_set || issuer_keyid = own_keyid then
		loop tl collected_sigs ignore_set
	      else
		(match handle_foreign_sig own_keyid signature tl ignore_set with
		   | (updated_ignore_set, Some esig) ->
		       (* found a foreign sig *)
		       loop tl (Signature_set.add (issuer_keyid, esig) collected_sigs) updated_ignore_set
		   | (updated_ignore_set, None) ->
		       (* found no useable foreign sig *)
		       loop tl collected_sigs updated_ignore_set)
	  else
	    loop tl collected_sigs ignore_set
      | [] ->
	  collected_sigs
  in
    loop siglist Signature_set.empty Keyid_set.empty

let update_puid puid_old_option uid puid_flag =
    match (puid_old_option, puid_flag) with
      |	((Some puid_old) as old, _)  -> old
      | (None, true) -> Some uid
      | (None, false) -> None

let update_exptime exptime_old exptime_new =
  match (exptime_old, exptime_new) with
    | (Some e1, Some e2) -> Some e2
    | (None, None) -> None
    | (Some e, None) -> Some e
    | (None, Some e) -> Some e

let handle_uid pkey pubkey_info result (uid_packet, siglist) =
  match uid_packet.Packet.packet_type with
    | Packet.User_ID_Packet ->
	let (sigs, puid, uids, valid_selfsig, exptime) = result in
	let uid = uid_packet.Packet.packet_body in
 	let own_keyid = Fingerprint.keyid_from_packet pkey.KeyMerge.key in
	let siglist = sort_reverse_siginfo_list siglist in
	  ( match search_self_sig own_keyid siglist with
	    | None -> result
	    | Some (puid_flag_new, exptime_new) ->
		let puid = update_puid puid uid puid_flag_new in
		let exptime = update_exptime exptime exptime_new in
		let new_sigs = collect_foreign_sigs own_keyid siglist in
		let total_sigs = Signature_set.union sigs new_sigs in
		let total_uids = uid :: uids in
		  (total_sigs, puid, total_uids, true, exptime) )
    | Packet.User_Attribute_Packet ->
	result
   | _ ->
	failwith "key_to_ekey: unexpected packet type in uid list"

let get_subkey_sig_status siginfo own_keyid =
  match siginfo.Index.sigtype with
    | 0x18 -> Valid_selfsig (true, None)
    | 0x28 -> Revoked (Option.get (i64_to_float_option siginfo.Index.sig_creation_time))
    | _ as t -> 
	printf "handle_self_sig: %s unexpected signature type 0x%x\n" (keyid_to_string own_keyid) t;
	Irrelevant_sigtype

(* how to handle revoked subkeys and signatures made by revoked subkeys *)
let handle_subkey pkey pubkey_info result (subkey_packet, siglist) =
  match subkey_packet.Packet.packet_type with
    | Packet.Public_Subkey_Packet ->
	let (subkey_ids, foreign_sigs) = result in
	let own_keyid = Fingerprint.keyid_from_packet pkey.KeyMerge.key in
	let subkey_keyid = Fingerprint.keyid_from_packet subkey_packet in
	let siglist = sort_reverse_siginfo_list siglist in
	let new_sigs = collect_foreign_sigs own_keyid siglist in
	  (subkey_keyid :: subkey_ids, (Signature_set.union new_sigs foreign_sigs))
    | _ ->
	failwith "key_to_ekey: unexpected packet type in subkey list"

let key_to_ekey key =
  try
    let pkey = KeyMerge.parse_keystr (KeyMerge.key_to_stream key) in
    let keyid = Fingerprint.keyid_from_packet pkey.KeyMerge.key in
    let pubkey_info = ParsePGP.parse_pubkey_info pkey.KeyMerge.key in
    let handle_uid = handle_uid pkey pubkey_info in
    let handle_subkey = handle_subkey pkey pubkey_info in
    let sig_pkey = pkey_to_pkey_siginfo pkey in
    let v3_expiry_date = v3_absolute_expire_date pubkey_info in
    let revocation_time = get_revocation_date sig_pkey.info_selfsigs in
    let start = (Signature_set.empty, None, [], false, None) in
    let pk_version = pubkey_info.Packet.pk_version in
    let (sigs, puid_option, uids, valid_selfsig, exptime) = 
      List.fold_left handle_uid start sig_pkey.info_uids
    in
    let (subkey_keyids, subkey_sigs) = 
      List.fold_left handle_subkey ([], Signature_set.empty) sig_pkey.info_subkeys 
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
	  match pk_version with
	    | 4 -> exptime
	    | 2 | 3 -> v3_expiry_date
	    | _ -> raise (Skip_key (Unparseable, "Unparseable: unknown version"))
	in
	let algo = pubkey_info.Packet.pk_alg in
	let keylen = pubkey_info.Packet.pk_keylen in
	let ctime = Int64.to_float pubkey_info.Packet.pk_ctime in
	let foreign_sigs = Signature_set.union sigs subkey_sigs in
	let siglist = Signature_set.elements foreign_sigs in
	let pki = 
	  { key_keyid = keyid; 
	    key_version = pk_version;
	    key_puid = puid; 
	    key_alg = algo; 
	    key_len = keylen;
	    key_ctime = ctime;
	    key_all_uids = uids;
	    key_exptime = exptime;
	    key_revoktime = revocation_time;
	  }
	in
	    (subkey_keyids, { pki = pki; signatures = siglist })
      else
	let msg = sprintf "key_to_ekey: no valid selfsignature -> skip key (version %d) %s" pk_version (keyid_to_string keyid) in
	  raise (Skip_key (No_valid_selfsig, msg))
  with
    | Skip_key (reason, s) ->
	print_endline s;
	let keyid = Fingerprint.keyid_from_packet (List.hd key) in
	  raise (Skipped_key (reason, keyid))
    | ParsePGP.Overlong_mpi 
    | Unparseable_signature_packet 
    | Signature_without_creation_time ->
	let keyid = Fingerprint.keyid_from_packet (List.hd key) in
	  raise (Skipped_key (Unparseable, keyid))
