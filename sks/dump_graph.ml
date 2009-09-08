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
  open ExtHashtbl
  open Option
  open Printf

  open Format
  open Sexplib
  open Sexp
  open Conv

  open Ekey
  open Ekey_conv
  open Misc

  let settings = {
    Keydb.withtxn = !Settings.transactions;
    Keydb.cache_bytes = !Settings.cache_bytes;
    Keydb.pagesize = !Settings.pagesize;
    Keydb.dbdir = Lazy.force Settings.dbdir;
    Keydb.dumpdir = Lazy.force Settings.dumpdir;
  }

  module Keydb = Keydb.Unsafe

  module Keyid_set = Set.Make(String)

  let lookup_key_index_in_array array keyid =
    let cmp keyid epki = compare keyid epki.key_keyid in
    let rec search low high =
      if high < low then
	raise Not_found
      else
	let mid = low + ((high-low) / 2) in
	  match cmp keyid (Array.get array mid)  with
	    | a when a < 0 -> search low (mid-1)
	    | b when b > 0 -> search (mid+1) high
	    | _ -> 
		assert ((cmp keyid (Array.get array mid)) = 0);
		mid
    in
      search 0 ((Array.length array) - 1)

  let ekey_list_to_sexp_graph ekey_list =
    print_endline "ekey_list_to_sexp_graph";
    let key_cnt = ref 0 in
    let sig_cnt = ref 0 in
    let vlist = List.map (fun ekey -> ekey.pki) ekey_list in
    let edge_list = RefList.empty () in
    let one_key_signatures ekey =
      let signee_id = ekey.pki.key_keyid in
      let signer_list = List.fold_left 
	(fun l esig ->
	   display_iterations sig_cnt "sigs";
	   let (signer_id, siginfo) = esig in
	     (signer_id, siginfo) :: l)
	[]
	ekey.signatures
      in
	(signee_id, signer_list)
    in
      List.iter
	(fun ekey -> 
	   display_iterations key_cnt "keys";
	   RefList.push edge_list (one_key_signatures ekey)
	)
	ekey_list
      ;
      (vlist, (RefList.to_list edge_list))

  let dump_sexp_graph_to_file vertex_filename edge_filename g =
    let (vertex_list, edge_list) = g in
    let v_channel = open_out vertex_filename in
    let e_channel = open_out edge_filename in
      List.iter
	(fun v ->
	   let s = sexp_of_epki v in
	     output_mach v_channel s;	
	     output_char v_channel '\n'
	)
	vertex_list
      ;
      List.iter
	(fun e ->
	   let s = sexp_of_sig_list_per_signee e in
	     output_mach e_channel s;
	     output_char e_channel '\n'
	)
	edge_list

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

  let fetch_missing_keys skipped_keyids keyids_so_far keys_so_far =
    let missing_keyids = ref Keyid_set.empty in
    let add_if_missing (issuer, _) =
	if Keyid_set.mem issuer keyids_so_far then
	  ()
	else
	  if not (Keyid_set.mem issuer skipped_keyids) then
	    missing_keyids := Keyid_set.add issuer !missing_keyids
	  else
	    ()
    in  
      List.iter 
	(fun ks ->
	   List.iter add_if_missing ks.signatures)
	keys_so_far
      ;
      Keyid_set.elements !missing_keyids

  let filter_signatures_to_missing_keys keys keyids =
    let filtered_keys = ref 0 in
    let filtered_sigs = ref 0 in
    let filter_list siglist =
      List.filter (fun (issuer, _) -> Hashtbl.mem keyids issuer) siglist
    in
    let rec filter_keys keylist filtered_keys =
      match keylist with
	| key :: tl ->
	    begin
	      let before = List.length key.signatures in
		key.signatures <- filter_list key.signatures;
		let diff = before - (List.length key.signatures) in
		  filtered_sigs := !filtered_sigs + diff;
		  if key.signatures = [] then 
		    filter_keys tl filtered_keys
		  else
		    filter_keys tl (key :: filtered_keys)
	    end
	| [] -> 
	    filtered_keys
    in
    let l = filter_keys keys [] in
      printf "filtered %d signatures %d keys\n" !filtered_sigs !filtered_keys;
      l

  let decide_who_stays ekey1 ekey2 = 
    let ctime1 = ekey1.pki.key_ctime in
    let ctime2 = ekey2.pki.key_ctime in
      if ctime1 > ctime2 then
	ekey1
      else if ctime2 > ctime1 then
	ekey2
      else
	let nr_sigs1 = List.length ekey1.signatures in
	let nr_sigs2 = List.length ekey2.signatures in
	  if nr_sigs1 >= nr_sigs2 then
	    ekey1
	  else
	    ekey2

  let fetch_keys () =
    let key_cnt = ref 0 in
    let skipped_cnt = ref 0 in
    let unsigned_cnt = ref 0 in
    let skipped_keyids = ref Keyid_set.empty in
    let relevant_keys = Hashtbl.create 320000 in
    let extract_key ~hash ~key =
      try 
	let key_struct = key_to_ekey key in
	  begin
	    display_iterations key_cnt "fetch_keys";
	    match key_struct.signatures with
	      | [] -> 
		  incr unsigned_cnt
	      | _ -> 
		  begin
		    try
		      let dupe = Hashtbl.find relevant_keys key_struct.pki.key_keyid in
			print_endline "DUPE!";
			print_endline (string_of_ekey dupe);
			print_endline (string_of_ekey key_struct);
			Hashtbl.add relevant_keys key_struct.pki.key_keyid (decide_who_stays key_struct dupe)
		    with Not_found -> Hashtbl.add relevant_keys key_struct.pki.key_keyid key_struct
		  end
	  end
      with
	| Skipped_key keyid ->
	    begin
	      incr skipped_cnt;
	      display_iterations key_cnt "fetch_keys";
	      skipped_keyids := Keyid_set.add keyid !skipped_keyids
	    end
    in
      Keydb.iter ~f:extract_key;
      printf "skipped %d\n" !skipped_cnt;
      printf "unsigned %d\n" !unsigned_cnt;
      printf "relevant keys in list %d\n" (Hashtbl.length relevant_keys);
      let keylist = List.of_enum (Hashtbl.values relevant_keys) in
	filter_signatures_to_missing_keys keylist relevant_keys

  let fetch_single_key keyid =
    match get_keys_by_keyid keyid with
      | key :: tl ->
	  begin
	    try
	      Some (key_to_ekey key)
	    with Skipped_key keyid -> None
	  end
      | [] -> None

  let dump_sexp_file file ekey_list =
    let chan = open_out file in
      List.iter 
	(fun ekey ->
	   let s = sexp_of_ekey ekey in
	     output_mach chan s;
	     output_char chan '\n'
	)
	ekey_list
      ;
      close_out chan
	  
  let run () =
    Keydb.open_dbs settings;
    begin
      let keys = time_evaluation fetch_keys "fetch_keys" in
      let vertexf = "vertex.sexp" in
      let edgef = "edge.sexp" in
	dump_sexp_graph_to_file vertexf edgef (ekey_list_to_sexp_graph keys)
    end
end
