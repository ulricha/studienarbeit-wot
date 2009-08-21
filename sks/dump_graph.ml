(************************************************************************)
(* This file is part of SKS.  SKS is free software; you can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA *)
(***********************************************************************)

(** takes content of SKS keyserver and creates a wotsap dump file from that *)

module F(M:sig end) = 
struct
  open ExtList
  open Option

  let settings = {
    Keydb.withtxn = !Settings.transactions;
    Keydb.cache_bytes = !Settings.cache_bytes;
    Keydb.pagesize = !Settings.pagesize;
    Keydb.dbdir = Lazy.force Settings.dbdir;
    Keydb.dumpdir = Lazy.force Settings.dumpdir;
  }

  module Keydb = Keydb.Unsafe

  module Keyid_set = Set.Make(String)

  exception Unparseable_signature_packet
  exception Signature_without_creation_time

  type cert_level = Generic | Persona | Casual | Positive

  type signature = { mutable sig_puid_signed: bool;
		     sig_level: int;
		     sig_issuer: string;
		     sig_ctime: float;
		     sig_hash_alg: int;
		     sig_pk_alg: int;
		   }

  type key = { key_keyid: string;
	       key_puid: string;
	       key_ctime: float;
	       key_alg: int;
	       key_len: int;
	       key_signatures: signature list
	     }

  let compare_signature s1 s2 = compare s1.sig_issuer s2.sig_issuer

  module Signature_set = Set.Make(struct
				    type t = signature
				    let compare = 
				      (fun s1 s2 -> 
					 Pervasives.compare s1.sig_issuer s2.sig_issuer)
				  end)

  module Key_set = Set.Make(struct
			      type t = key
			      let compare =
				(fun k1 k2 ->
				   Pervasives.compare k1.key_keyid k2.key_keyid)
			    end)

  type sigpair_siginfo = Packet.packet * Index.siginfo list

  type pkey_siginfo = { info_key : Packet.packet;
			info_selfsigs: Index.siginfo list; (* revocations only in v3 keys *)
			info_uids: sigpair_siginfo list
		      }

  let string_of_key_struct ks =
    let out = Buffer.create 70 in
    let keyid_string = Fingerprint.keyid_to_string ks.key_keyid in
      Buffer.add_string out keyid_string;
      Buffer.add_char out ' ';
      Buffer.add_string out ks.key_puid;
      Buffer.add_char out ' ';
      Buffer.add_string out (Printf.sprintf "(%s - %d bit)" (Packet.pubkey_algorithm_string ks.key_alg) ks.key_len);
      Buffer.add_char out ' ';
      Buffer.add_string out "signed by ";
      List.iter (fun signature ->
		   Buffer.add_string out (Fingerprint.keyid_to_string signature.sig_issuer);
		   let s =  (Printf.sprintf " (pk %s %d bit h %s) " 
			       (Packet.pubkey_algorithm_string signature.sig_pk_alg) 
			       ks.key_len
			       (Packet.hash_algorithm_string signature.sig_hash_alg)) in
		     Buffer.add_string out s;
		)
	ks.key_signatures;
      Buffer.contents out

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

  let get_keys_by_keyid keyid =
    let keyid_length = String.length keyid in
    let short_keyid = String.sub keyid (keyid_length - 4) 4 in
    let keys = Keydb.get_by_short_subkeyid short_keyid in
      match keyid_length with
	| 4 -> (* 32-bit keyid.  No further filtering required. *)
	    keys
	| 8 -> (* 64-bit keyid *) 
	    List.filter (fun key -> (Fingerprint.from_key key).Fingerprint.keyid = keyid ) keys
	| 20 -> (* 160-bit v. 4 fingerprint *)
	    List.filter (fun key -> keyid = (Fingerprint.from_key key).Fingerprint.fp ) keys
	| 16 -> (* 128-bit v3 fingerprint.  Not supported *)
	    failwith "128-bit v3 fingerprints not implemented"
	| _ -> failwith "unknown keyid type"

  let key_to_pkey key =
    let stream = KeyMerge.key_to_stream key in
    try Some (KeyMerge.parse_keystr stream) with
      | KeyMerge.Unparseable_packet_sequence -> None

  let discard a =
    ()

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

  let is_v4_key_expired siginfo =
    let today = Stats.round_up_to_day (Unix.gettimeofday ()) in
      match siginfo.Index.sig_creation_time with
	| Some ctime ->
	    begin
	      match siginfo.Index.key_expiration_time with
		| Some exptime ->
		    if compare (Int64.add ctime exptime) (Int64.of_float today) <= 0 then
		      true
		    else
		      false
		| None ->
		    false
	    end
	| None -> false

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
    
  let is_uid_expired keyid sigpair =
    let (uid, sig_list) = sigpair in
      (* get list of selfsigs *)
    let siginfo_list = List.map Index.sig_to_siginfo sig_list in
    let selfsigs = List.filter (fun siginfo -> Index.is_selfsig ~keyid:keyid siginfo) siginfo_list in
    let selfsigs_sorted_reverse = sort_reverse_siginfo_list selfsigs in
    let rec iter_selfsigs l =
      match l with
	| selfsig :: tl ->
	    begin
	      match selfsig.Index.sigtype with
		| 0x10 | 0x11 | 0x12 | 0x13 -> 
		    if is_signature_expired selfsig then
		      true
		    else
		      let res = is_v4_key_expired selfsig in
			res
		| 0x30 -> true
		| _ -> iter_selfsigs tl
	    end	
	| [] -> false
    in
      iter_selfsigs selfsigs_sorted_reverse

  let is_v4_expired keyid pkey pubkeyinfo =
    let expired_list = List.map (fun sigpair -> is_uid_expired keyid sigpair) pkey.KeyMerge.uids in
      List.exists (fun a -> a = true) expired_list

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

  exception Skip_key of string
  exception Skip_uid of string
  exception Skipped_key of string

  let cert_level_of_int d =
    match d with
      | 0x10 -> Generic
      | 0x11 -> Persona
      | 0x12 -> Casual
      | 0x13 -> Positive
      | _ -> failwith (Printf.sprintf "cert_level_of_int: unexpected vale %d" d)

  let string_of_cert_level l =
      match l with
	| Generic -> "Generic"
	| Persona -> "Persona"
	| Casual -> "Casual"
	| Positive -> "Positive"

  let siginfo_to_signature_struct issuer siginfo =
    { sig_puid_signed = false; 
      sig_level = siginfo.Index.sigtype; 
      sig_issuer = issuer;
      sig_hash_alg = siginfo.Index.siginfo_hash_alg;
      sig_pk_alg = siginfo.Index.siginfo_pk_alg;
      sig_ctime = match siginfo.Index.sig_creation_time with 
	| Some time -> Int64.to_float time
	| None -> Int64.to_float 0L;
    }

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
				  (* sig_accumulator := (siginfo_to_signature_struct issuer_keyid signature) :: !sig_accumulator; *)
				  sig_accumulator := Signature_set.add (siginfo_to_signature_struct issuer_keyid signature) !sig_accumulator;
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
	
  let key_to_key_struct key =
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
	    List.iter (fun (uid_packet, siglist) ->
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
					 (* print_endline ("uid " ^ uid_packet.Packet.packet_body ^ " skipped: " ^ s) *)
					 ()
			       end
			   | Packet.User_Attribute_Packet ->
			       (* attribute packet can only contain a photo id at the moment -> skip*)
			       ()
			   | _ -> 
			       failwith "key_to_key_struct: unexpected packet type in uid list"
		      )
	      sig_pkey.info_uids;
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


  let count_iterations cnt =
    if !cnt mod 10000 = 0 then
      begin
	print_endline (string_of_int !cnt);
	incr cnt
      end
    else
      incr cnt

  let fetch_missing_keys skipped_keyids keyids_so_far keys_so_far =
    let missing_keyids = ref Keyid_set.empty in
    let add_if_missing signature =
	if Keyid_set.mem signature.sig_issuer keyids_so_far then
	  ()
	else
	  if not (Keyid_set.mem signature.sig_issuer skipped_keyids) then
	    missing_keyids := Keyid_set.add signature.sig_issuer !missing_keyids
	  else
	    ()
    in  
      List.iter 
	(fun ks ->
	   List.iter add_if_missing ks.key_signatures)
	keys_so_far
      ;
      Keyid_set.elements !missing_keyids

  let test_key_extraction () =
    let key_cnt = ref 0 in
    let skipped_cnt = ref 0 in
    let unsigned_cnt = ref 0 in
    let relevant_keyids = ref Keyid_set.empty in
    let skipped_keyids = ref Keyid_set.empty in
    let relevant_keys = ref [] in
    let extract_key ~hash ~key =
      try 
	let key_struct = key_to_key_struct key in
	  begin
	    count_iterations key_cnt;
	    (* print_endline (string_of_key_struct key_struct) *)
	    match key_struct.key_signatures with
	      | [] -> 
		  incr unsigned_cnt
	      | _ -> 
		  begin
		    relevant_keys := key_struct :: !relevant_keys;
		    relevant_keyids := Keyid_set.add key_struct.key_keyid !relevant_keyids
		  end
	  end
      with
	| Skipped_key keyid ->
	    begin
	      incr skipped_cnt;
	      count_iterations key_cnt;
	      skipped_keyids := Keyid_set.add keyid !skipped_keyids
	    end
    in
      begin
	Keydb.iter ~f:extract_key;
	Printf.printf "skipped %d\n" !skipped_cnt;
	Printf.printf "unsigned %d\n" !unsigned_cnt;
	Printf.printf "relevant keys in list %d\n" (List.length !relevant_keys);
	let missing_list = fetch_missing_keys !skipped_keyids !relevant_keyids !relevant_keys in
	  begin
	    Printf.printf "missing keys %d\n" (List.length missing_list);
	    let rec iter l i =
	      if i > 20 then
		()
	      else
		match l with
		  | keyid :: tl -> 
		      begin
			print_endline (Fingerprint.keyid_to_string keyid);
			iter tl (i+1)
		      end
		  | [] ->
		      ()
	    in
	      iter missing_list 0
	  end
      end
(*
  let test_key_struct () =
    let keyid = Fingerprint.keyid_of_string "0x94660424" in
    let keys = get_keys_by_keyid keyid in
      print_endline ("nr keys " ^ (string_of_int (List.length keys)));
      List.iter	(fun key -> 
		   match key_to_key_struct key with
		     | None ->
			 begin
			   print_endline "no key returned (why?)"
			 end
		     | Some key_struct ->
			 begin
			   print_endline (string_of_key_struct key_struct)
			 end
		)
	keys
*)

  let run () =
    Keydb.open_dbs settings;
    let t1 = Unix.time () in
      begin
	test_key_extraction ();
	let t2 = Unix.time () in
	  print_endline ("time " ^ (string_of_float (t2 -. t1)))
      end
end
