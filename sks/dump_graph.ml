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

  let filter_signatures_to_missing_keys keys keyids =
    let filter_list siglist =
      List.filter (fun s -> Keyid_set.mem s.sig_issuer keyids) siglist
    in
    let rec iter l =
      match l with
	| key :: tl ->
	    begin
	      key.key_signatures <- filter_list key.key_signatures;
	      iter tl
	    end
	| [] -> 
	    ()
    in
      iter !keys

  let fetch_keys () =
    let key_cnt = ref 0 in
    let skipped_cnt = ref 0 in
    let unsigned_cnt = ref 0 in
    let relevant_keyids = ref Keyid_set.empty in
    let skipped_keyids = ref Keyid_set.empty in
    let relevant_keys = ref [] in
    let extract_key ~hash ~key =
      try 
	let key_struct = key_to_ekey key in
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
	printf "skipped %d\n" !skipped_cnt;
	printf "unsigned %d\n" !unsigned_cnt;
	printf "relevant keys in list %d\n" (List.length !relevant_keys);
	filter_signatures_to_missing_keys relevant_keys !relevant_keyids;
      end
      ;
      !relevant_keys
	
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
    let t1 = Unix.time () in
      begin
	let keys = fetch_keys () in
	let t2 = Unix.time () in
	  begin
	    print_endline ("time " ^ (string_of_float (t2 -. t1)));
	    Gc.full_major ();
	    let filename = "sks_dump.sexp" in
	      dump_sexp_file filename keys
	  end
      end
end
