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
      List.filter (fun (issuer, _) -> Keyid_set.mem issuer keyids) siglist
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

  let fetch_keys () =
    let key_cnt = ref 0 in
    let skipped_cnt = ref 0 in
    let unsigned_cnt = ref 0 in
    let relevant_keyids = ref Keyid_set.empty in
    let skipped_keyids = ref Keyid_set.empty in
    let relevant_keys = Hashtbl.create 320000 in
    let relevant_keys = ref [] in
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
		    if Keyid_set.mem key_struct.pki.key_keyid !relevant_keyids then
		      let dupe = List.find (fun ekey -> ekey.pki.key_keyid = key_struct.pki.key_keyid) !relevant_keys in
			print_endline "DUPE!";
			print_endline (string_of_ekey dupe);
			print_endline (string_of_ekey key_struct)
		    else
		      begin
			relevant_keys := key_struct :: !relevant_keys;
			relevant_keyids := Keyid_set.add key_struct.pki.key_keyid !relevant_keyids
		      end
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
      printf "relevant keys in list %d\n" (List.length !relevant_keys);
      filter_signatures_to_missing_keys !relevant_keys !relevant_keyids

  exception No_difference

  let decide_who_stays l = 
    printf "decide on list with len %d\n" (List.length l);
    let find_newest keylist =
      let cmp_ctime = (fun k1 k2 -> compare k1.pki.key_ctime k2.pki.key_ctime) in
      let sorted = List.sort ~cmp:cmp_ctime keylist
      in
      let last = List.last sorted in
      let first = List.hd sorted in
	if last.pki.key_ctime = first.pki.key_ctime then
	  raise No_difference
	else
	  last
    in
    let find_most_ids keylist =
      let cmp_sigcnt = 	
	(fun k1 k2 -> 
	   compare (List.length k1.signatures) (List.length k2.signatures)) in
      let sorted = List.sort ~cmp:cmp_sigcnt keylist in
      let last = List.last sorted in
      let first = List.hd sorted in
	if (List.length last.signatures) = (List.length first.signatures) then
	  raise No_difference
	else
	  last
    in
      try 
	[find_newest l] 
      with No_difference ->
	try
	  [find_most_ids l]
	with No_difference ->
	  [List.hd l]

  let filter_keys_with_duplicate_keyids keylist =
    let grplist = Misc.group compare_ekey keylist in
    let filtered_grplist = List.map 
      (function 
	 | key :: [] as l -> l
	 | l ->  decide_who_stays l
      )
      grplist 
    in
      List.flatten filtered_grplist
	
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
      let keys = time_evaluation fetch_keys in
      let filename = "sks_dump.sexp" in
	dump_sexp_file filename keys
    end
end
