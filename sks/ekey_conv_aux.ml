open ExtList
open Ekey
open Misc

module Keyid_set = Set.Make(String)

module Signature_set = Set.Make(struct
				  type t = esignature
				  let compare = compare_esignature
				end)

type skip_reason = Expired | Revoked | No_valid_selfsig | Unparseable

exception Unparseable_signature_packet
exception Signature_without_creation_time
exception Skip_key of skip_reason * string
exception Skip_uid of string
exception Skipped_key of skip_reason * string

type sigpair_siginfo = Packet.packet * Index.siginfo list

type pkey_siginfo = { info_key : Packet.packet;
		      info_selfsigs: Index.siginfo list;
		      info_uids: sigpair_siginfo list
		    }

let pkey_to_pkey_siginfo k = 
  try
    let s = List.map Index.sig_to_siginfo k.KeyMerge.selfsigs in
    let uids = 
      List.map 
	(fun pair -> 
	   (fst pair, List.map Index.sig_to_siginfo (snd pair)))
	k.KeyMerge.uids
    in
      { info_key = k.KeyMerge.key; info_selfsigs = s; info_uids = uids }
  with
    | _ -> raise Unparseable_signature_packet

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
