open ExtList
open Ekey
open Misc

module Keyid_set = Set.Make(String)

module Signature_set = Set.Make(struct
				  type t = esignature
				  let compare = compare_esignature
				end)

type skip_reason = Expired | Revoked | No_valid_selfsig | Unknown_key_version | Unparseable

let string_of_skip_reason = function
  | Expired -> "Expired"
  | Revoked -> "Revoked"
  | No_valid_selfsig -> "No_valid_selfsig"
  | Unparseable -> "Unparseable"
  | Unknown_key_version -> "Unknown_key_version"

exception Unparseable_signature_packet
exception Signature_without_creation_time
exception Skip_key of skip_reason * string
exception Skip_uid of string
exception Skipped_key of skip_reason * string

type sigpair_siginfo = Packet.packet * Index.siginfo list

type pkey_siginfo = { info_key : Packet.packet;
		      info_selfsigs: Index.siginfo list;
		      info_uids: sigpair_siginfo list;
		      info_subkeys: sigpair_siginfo list
		    }

(* transform list of packets to a list of siginfos and skip unparseable 
   signatures *)
let parse_sigs siglist =
  let parse l sig_packet =
    try
      let siginfo = Index.sig_to_siginfo sig_packet in
	siginfo :: l
    with
	_ -> 
	  print_endline "skip unparseable signature";
	  l
  in
    List.fold_left parse [] siglist
      
let pkey_to_pkey_siginfo k = 
  let s = List.map Index.sig_to_siginfo k.KeyMerge.selfsigs in
  let uids = 
    List.map 
      (fun pair -> 
	 (fst pair, parse_sigs (snd pair)))
      k.KeyMerge.uids
  in
  let subkeys = 
    List.map
      (fun pair ->
	 (fst pair, parse_sigs (snd pair)))
      k.KeyMerge.subkeys
  in
    { info_key = k.KeyMerge.key; info_selfsigs = s; info_uids = uids; info_subkeys = subkeys }

let key_to_pkey key =
  let stream = KeyMerge.key_to_stream key in
    try Some (KeyMerge.parse_keystr stream) with
	KeyMerge.Unparseable_packet_sequence -> None


let sort_reverse_siginfo_list siglist =
  let compare_ctime_reverse sig1 sig2 =
    try
      let ctime1 = Option.get sig1.Index.sig_creation_time in
      let ctime2 = Option.get sig2.Index.sig_creation_time in
	match Int64.to_int (Int64.sub ctime1 ctime2) with
	  | 0 -> 0
	  | d when d > 0 -> -1
	  | d -> 1
    with Option.No_value -> raise Signature_without_creation_time
  in
    List.sort ~cmp:compare_ctime_reverse siglist

let is_signature_valid siginfo =
  match siginfo.Index.keyid with
    | Some keyid -> true
    | None -> false

(* v4 keys specify expiration time as the number of seconds after creation time
   for which the key is valid *)
let v4_absolute_expire_date exptime ctime =
  match exptime with
    | Some t when t = 0L -> None
    | Some t -> Some (Int64.to_float (Int64.add (Option.get ctime) t))
    | None -> None

(* v3 keys specify expiration time as number of days relative to the creation time *)
let v3_absolute_expire_date pubkeyinfo =
  match pubkeyinfo.Packet.pk_expiration with
    | Some d ->
	let ctime = Int64.to_float pubkeyinfo.Packet.pk_ctime in
	let valid = (float_of_int d) *. 24. *. 3600. in
	  Some (ctime +. valid)
    | None -> None

let siginfo_to_esiginfo siginfo =
  (* according to RFC 4880 v3 sigs don't contain a expiration time so this should be safe. *)
  let exptime = v4_absolute_expire_date siginfo.Index.sig_expiration_time siginfo.Index.sig_creation_time in
    { sig_puid_signed = false; 
      sig_level = siginfo.Index.sigtype; 
      sig_hash_alg = siginfo.Index.siginfo_hash_alg;
      sig_pk_alg = siginfo.Index.siginfo_pk_alg;
      sig_ctime = i64_to_float_option siginfo.Index.sig_creation_time;
      sig_exptime = exptime;
      sig_revoktime = None;
    }
