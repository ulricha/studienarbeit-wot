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

  exception Unparseable_key

  type cert_level = Generic | Persona | Casual | Positive

  type signature = { mutable sig_puid_signed : bool;
		     sig_level : cert_level;
		     sig_issuer : string
		   }

  type key = { key_keyid : string;
	       key_puid : string;
	       key_signatures : signature list
	     }

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
      Buffer.add_string out "signed by ";
      List.iter (fun signature ->
		   Buffer.add_string out (Fingerprint.keyid_to_string signature.sig_issuer);
		   Buffer.add_char out ' ')
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
      | _ -> raise Unparseable_key

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

  let rec print_multiple_key_structure keyid =
    let rec print_single_key_structure key = 
      match key with
	| packet :: tl when packet.Packet.packet_type = Packet.Signature_Packet -> 
	    let s = Index.sig_to_siginfo packet in
	      if s.Index.is_primary_uid then
		begin
		  Packet.print_packet packet;
		  print_endline "is primary uid";
		  print_single_key_structure tl;
		end
	      else
		begin
		  Packet.print_packet packet;
		  print_single_key_structure tl;
		end
	| packet :: tl ->
	    Packet.print_packet packet;
	    print_single_key_structure tl
	| [] ->
	    ()
    in
    let keys = get_keys_by_keyid keyid in
      print_endline (string_of_int (List.length keys));
      match keys with
	| key :: tl -> print_single_key_structure key
	| [] -> ()

  let string_of_siginfo s = 
    let out = ref "" in
      if is_some s.Index.keyid then
	out := !out ^ "keyid " ^ (Fingerprint.keyid_to_string (Option.get s.Index.keyid))
      else
	out := !out ^ "no keyid (wtf?)"
      ;
(*      if is_some s.keyid then
	out := !out ^ " keyid " ^ (keyid_to_string (get s.keyid))
      else
	() *)
      !out
  

  let print_packet_verbose packet =
    match packet.Packet.packet_type with
      | Packet.Signature_Packet -> 
	  let info = Index.sig_to_siginfo packet in
	    print_endline (string_of_siginfo info)
      | _ -> Packet.print_packet packet

  let print_uid_verbose uid =
    let (uid_packet, sigpacket_list) = uid in
      print_endline "uid";
      print_packet_verbose uid_packet;
      print_endline "signatures";
      List.iter print_packet_verbose sigpacket_list

  let print_pkey p =
      begin
	print_endline "key";
	Packet.print_packet p.KeyMerge.key;
	print_endline ("selfsigs len " ^ (string_of_int (List.length p.KeyMerge.selfsigs)));
	List.iter print_packet_verbose p.KeyMerge.selfsigs;
	print_endline ("uids len " ^ (string_of_int (List.length p.KeyMerge.uids)));
	List.iter print_uid_verbose p.KeyMerge.uids 
      end

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
      with No_value -> failwith "sort_reverse_siginfo_list: signature does not contain creation time"
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

  let is_expired pkey =
    try 
    let info = ParsePGP.parse_pubkey_info pkey.KeyMerge.key in
      match info.Packet.pk_version with
	| 2 | 3 -> 
	    is_v3_expired info
	| 4 -> 
	    let keyid = Fingerprint.keyid_from_packet pkey.KeyMerge.key in
	      is_v4_expired keyid pkey info
	| x -> 
	    failwith ("unexpected pk_version field " ^ (string_of_int x))
    with 
      | ParsePGP.Overlong_mpi -> 
	  begin
	    print_endline "overlong mpi";
	    false
	  end
      | Failure s ->
	  begin
	    print_endline ("Failure "^ s);
	    false
	  end
      | _ -> 
	  begin
	    print_endline "unexpected exception";
	    false
	  end

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

  exception Skip_key
  exception Skip_uid

  let siginfo_to_signature_struct issuer siginfo =
    let cert_level = match siginfo.Index.sigtype with
      | 0x10 -> Generic
      | 0x11 -> Persona
      | 0x12 -> Casual
      | 0x13 -> Positive
      | _ -> failwith ("siginfo_to_signature_struct: unexpected signature type" ^ 
			 (string_of_int siginfo.Index.sigtype))
    in
      { sig_puid_signed = false; sig_level = cert_level; sig_issuer = issuer }

  let iter_sigs keyid uid_packet siglist sig_accumulator puid =
    let sigs_so_far = ref Keyid_set.empty in
    let siglist_descending = sort_reverse_siginfo_list siglist in
    let rec iter l =
      match l with
	| signature :: tl ->
	    begin
	      let issuer_keyid = get signature.Index.keyid in
		if Keyid_set.mem issuer_keyid !sigs_so_far then
		  (* issuer was not handled so far *)
		  begin
		    if Index.is_selfsig keyid signature then
		      (* handle self-signature *)
		      match signature.Index.sigtype with
			| 0x20 ->
			    (* key is revoked - can this appear in a uid list? *)
			    raise Skip_key
			| 0x30 ->
			    (* uid is revoked *)
			    raise Skip_uid
			| 0x10 | 0x11 | 0x12 | 0x13 ->
			    if is_signature_expired signature then
			      raise Skip_key
			    else
			      if signature.Index.is_primary_uid then
				begin
				  (* user attributes should be skipped, so this must be a User ID *)
				  puid := Some uid_packet.Packet.packet_body;
				  sigs_so_far := Keyid_set.add issuer_keyid !sigs_so_far;
				  iter tl
				end
			| _ ->
			    (* skip unexpected/irrelevant sig type *)
			    iter tl
		    else
		      (* handle signature by another key *)
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
				sig_accumulator := (siginfo_to_signature_struct issuer_keyid signature) :: !sig_accumulator;
				sigs_so_far := Keyid_set.add issuer_keyid !sigs_so_far;
				iter tl
			      end
			| _ ->
			    (* skip unexpected/irrelevant sig type *)
			    iter tl
		  end
		else
		  (* issuer was already handled -> skip *)
		  iter tl
	    end
	| [] ->
	    (* TODO: what? *)
	    ()
    in
      iter siglist_descending
	
  let key_to_key_struct key =
    try 
      let pkey =  KeyMerge.parse_keystr (KeyMerge.key_to_stream key) in
	
      let sig_pkey = pkey_to_pkey_siginfo pkey in
      let pubkey_info = ParsePGP.parse_pubkey_info pkey.KeyMerge.key in
      let keyid = Fingerprint.keyid_from_packet pkey.KeyMerge.key in
      ()
    with
      | _ -> print_endline "skip key"
      

  let count_iterations cnt =
    if !cnt mod 10000 = 0 then
      begin
	incr cnt;
	print_endline (string_of_int !cnt)
      end
    else
      incr cnt

  let count_expired_revoked () =
    let key_cnt = ref 0 in
    let revoked_cnt = ref 0 in
    let expired_cnt = ref 0 in
    let count_revoked ~hash ~key =
      let pkey = KeyMerge.parse_keystr (KeyMerge.key_to_stream key) in
      begin
	if is_revoked pkey then
	  incr revoked_cnt
	else
	  ()
	;
	if is_expired pkey then
	  incr expired_cnt
	else
	  ()
	;
	count_iterations key_cnt
      end
    in
      begin
	Keydb.iter ~f:count_revoked;
	print_endline ("revoked " ^ (string_of_int !revoked_cnt));
	print_endline ("expired " ^ (string_of_int !expired_cnt))
      end

  let test_expired () =
    let keyid = Fingerprint.keyid_of_string "0x3EF281DA" in
    let keys = get_keys_by_keyid keyid in
    let pkeys = List.map key_to_pkey keys in
      print_endline ("nr keys " ^ (string_of_int (List.length keys)));
      List.iter
	(function
	   | Some p ->
	       if is_expired p then
		 print_endline "is expired"
	       else
		 print_endline "not expired"
	   | None -> print_endline "unparseable packet sequence"
	)
	pkeys

  let run () =
    Keydb.open_dbs settings;
    let t1 = Unix.time () in
      begin
	test_expired ();
	count_expired_revoked ();
	let t2 = Unix.time () in
	  print_endline ("time " ^ (string_of_float (t2 -. t1)))
      end
end
