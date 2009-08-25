open ExtList
open Option
open Printf

open Format
open Sexplib
open Sexp
open Conv

open Ekey

module Keyid_set = Set.Make(String)

exception Unparseable_signature_packet
exception Signature_without_creation_time
exception Skip_key of string
exception Skip_uid of string
exception Skipped_key of string


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

let sort_reverse_siginfo_list siglist =
  let compare_ctime_reverse sig1 sig2 =
    try
      let ctime1 = Option.get sig1.Index.sig_creation_time in
      let ctime2 = Option.get sig2.Index.sig_creation_time in
	match Int64.to_int (Int64.sub ctime1 ctime2) with
	  | 0 -> 0
	  | d when d > 0 -> -1
	  | d -> 1
    with No_value -> raise Signature_without_creation_time
  in
    List.sort ~cmp:compare_ctime_reverse siglist
      
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

let siginfo_to_esignature issuer siginfo =
  let esiginfo = 
    { sig_puid_signed = false; 
      sig_level = siginfo.Index.sigtype; 
      sig_hash_alg = siginfo.Index.siginfo_hash_alg;
      sig_pk_alg = siginfo.Index.siginfo_pk_alg;
      sig_ctime = match siginfo.Index.sig_creation_time with 
	| Some time -> Int64.to_float time
	| None -> Int64.to_float 0L;
    }
  in
    (issuer, esiginfo)

let check_expired ctime signature =
  if is_signature_expired signature then
    raise (Skip_uid "most recent self-signature expired")
  else
    if is_key_expired ctime signature then
      raise (Skip_key "key expired")
    else
      ()

let is_signature_valid siginfo =
  match siginfo.Index.keyid with
    | Some time -> true
    | None -> false

let iter_sigs keyid uid_packet siglist sig_accumulator puid pubkey_info =
  let sigs_so_far = ref Keyid_set.empty in
  let siglist_descending = sort_reverse_siginfo_list siglist in
  let rec iter l =
    match l with
      | signature :: tl when not (is_signature_valid signature) ->
	  iter tl
      | signature :: tl ->
	  begin
	    let issuer_keyid = get signature.Index.keyid in
	      if not (Keyid_set.mem issuer_keyid !sigs_so_far) then
		(* issuer was not handled so far *)
		begin
		  if keyid = issuer_keyid then
		    begin
		      (* handle self-signature *)
		      match signature.Index.sigtype with
			| 0x20 ->
			    (* key is revoked - can this appear in a uid list? *)
			    raise (Skip_key "key revoked (0x20)")
			| 0x30 ->
			    (* uid is revoked *)
			    raise (Skip_uid "uid is revoked (0x30)")
			| 0x10 | 0x11 | 0x12 | 0x13 ->
			    begin
			      check_expired pubkey_info.Packet.pk_ctime signature;
			      if is_none !puid then
				puid := Some uid_packet.Packet.packet_body
			      else
				if signature.Index.is_primary_uid then
				  (* user attributes should be skipped, so this must be a User ID *)
				  puid := Some uid_packet.Packet.packet_body
				else
				  ()
			      ;
			      sigs_so_far := Keyid_set.add issuer_keyid !sigs_so_far;
			      iter tl
			    end
			| _ ->
			    (* skip unexpected/irrelevant sig type *)
			    iter tl
		    end
		  else
		    (* handle signature by another key *)
		    begin
		      match signature.Index.sigtype with
			| 0x30 ->
			    (* sig is revoked -> don't consider this issuer for further sigs *)
			    sigs_so_far := Keyid_set.add issuer_keyid !sigs_so_far;
			    iter tl
			| 0x10 | 0x11 | 0x12 | 0x13 ->
			    if is_signature_expired signature then
			      begin
				(* sig is expired -> don't consider this issuer for further sigs *)
				sigs_so_far := Keyid_set.add issuer_keyid !sigs_so_far;
				iter tl
			      end
			    else
			      begin
				sig_accumulator := Signature_set.add (siginfo_to_esignature issuer_keyid signature) !sig_accumulator;
				sigs_so_far := Keyid_set.add issuer_keyid !sigs_so_far;
				iter tl
			      end
			| t ->
			    (* skip unexpected/irrelevant sig type *)
			    iter tl
		    end
		end
	      else
		begin
		  (* issuer was already handled -> skip *)
		  iter tl
		end
	  end
      | [] ->
	  ()
  in
    iter siglist_descending
      
let key_to_ekey key =
  try 
    let pkey = KeyMerge.parse_keystr (KeyMerge.key_to_stream key) in
    let pubkey_info = ParsePGP.parse_pubkey_info pkey.KeyMerge.key in
    let sig_pkey = pkey_to_pkey_siginfo pkey in
      if is_v3_expired pubkey_info || is_revoked_pkey_siginfo sig_pkey then	
	raise (Skip_key "key is expired (v3) or revoked")
      else
	let keyid = Fingerprint.keyid_from_packet pkey.KeyMerge.key in
	let sig_accu = ref Signature_set.empty in
	let puid = ref None in
	let iter_uids = 
	  (fun (uid_packet, siglist) ->
	     match uid_packet.Packet.packet_type with
	       | Packet.User_ID_Packet ->
		   begin
		     let this_uid_sigs = ref Signature_set.empty in
		       try
			 begin
			   iter_sigs keyid uid_packet siglist this_uid_sigs puid pubkey_info;
			   sig_accu := Signature_set.union !this_uid_sigs !sig_accu
			 end
		       with
			 | Skip_uid s-> 
			     ()
		   end
	       | Packet.User_Attribute_Packet ->
		   (* attribute packet can only contain a photo id at the moment -> skip*)
		   ()
	       | _ -> 
		   failwith "key_to_ekey: unexpected packet type in uid list"
	  )
	in
	  List.iter iter_uids sig_pkey.info_uids;
	  match !puid with
	    | None -> 
		let keyid = Fingerprint.keyid_from_packet (List.hd key) in
		  raise (Skipped_key keyid)
	    | Some s -> 
		let siglist = Signature_set.elements !sig_accu in 
		let algo = pubkey_info.Packet.pk_alg in
		let keylen = pubkey_info.Packet.pk_keylen in
		let ctime = Int64.to_float pubkey_info.Packet.pk_ctime in
		  { key_keyid = keyid; 
		    key_puid = s; 
		    key_signatures = siglist; 
		    key_alg = algo; 
		    key_len = keylen;
		    key_ctime = ctime ;
		  }
  with
    | Skip_key s -> 
	(* print_endline ("skip key: " ^ s); *)
	let keyid = Fingerprint.keyid_from_packet (List.hd key) in
	  raise (Skipped_key keyid)
    | ParsePGP.Overlong_mpi | Unparseable_signature_packet | Signature_without_creation_time ->
	let keyid = Fingerprint.keyid_from_packet (List.hd key) in
	  raise (Skipped_key keyid)
