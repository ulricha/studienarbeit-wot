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
  open Ekey_conv_aux
  open Ekey_conv_all
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

  let dump_ekeys_to_file ekeys_enum filename =
    let out_chan = open_out filename in
    let write ekey =
	   let s = sexp_of_ekey ekey in
	     output_mach out_chan s;
	     output_char out_chan '\n'
    in
      Enum.iter write ekeys_enum;
      close_out out_chan

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

  let add_key_without_duplicate keys_so_far newkey =
    try
      let dupe = Hashtbl.find keys_so_far newkey.pki.key_keyid in
	print_endline "DUPE!";
	print_endline (string_of_ekey dupe);
	print_endline (string_of_ekey newkey);
	Hashtbl.add keys_so_far newkey.pki.key_keyid (decide_who_stays newkey dupe)
    with Not_found -> Hashtbl.add keys_so_far newkey.pki.key_keyid newkey

  let filter_sigs_to_missing_keys tbl =
    let bench = time_iterations "filter_sigs" 10000 in
    let c = ref 0 in
    let filter_single_key ekey =
      bench ();
      let before = List.length ekey.signatures in
      let rec loop siglist result =
	match siglist with
	  | ((issuer, _) as s) :: tl -> 
	      if Hashtbl.mem tbl issuer then
		loop tl (s :: result)
	      else
		loop tl result
	  | [] -> result
      in
      let filtered = loop ekey.signatures [] in
	ekey.signatures <- filtered;
	let after = List.length ekey.signatures in
	  if before <> after then (
	    c := !c + (before - after);
	    printf "filtered %d sigs\n" (before - after)
	  )
    in
      Enum.iter filter_single_key (Hashtbl.values tbl);
      printf "filtered %d sigs altogether\n" !c;
      flush stdout
	
  let replace_subkeyids_in_sigs ekey_tbl subkey_tbl =
    print_endline "replace_subkeyids_in_sigs";
    let replace ekey =
      let replaced = ref 0 in
      let replace_subkeyid (signer, esiginfo) =
	try 
	  let new_signer = Hashtbl.find subkey_tbl signer in
	    incr replaced;
	    (new_signer, esiginfo)
	with Not_found -> (signer, esiginfo)
      in
      let replaced_sigs = List.map replace_subkeyid ekey.signatures in
	ekey.signatures <- replaced_sigs;
	if !replaced <> 0 then
	  begin
	    printf "replaced %d subkeyids in signatures\n" !replaced;
	    flush stdout
	  end;
    in
      Enum.iter replace (Hashtbl.values ekey_tbl)

  let fetch_keys () =
    let key_cnt = ref 0 in
    let skipped_cnt = ref 0 in
    let keys_so_far = Hashtbl.create 3000000 in
    let subkeyids = Hashtbl.create 3000000 in
    let extract_key ~hash ~key =
      try 
	let (subkey_ids, ekey) = key_to_ekey key in
	  List.iter (fun sk_id -> Hashtbl.add subkeyids sk_id ekey.pki.key_keyid) subkey_ids;
	  display_iterations key_cnt "fetch_keys" 10000;
	  add_key_without_duplicate keys_so_far ekey
      with
	| Skipped_key (reason, msg) ->
	    display_iterations skipped_cnt "skipped" 10000;
	    display_iterations key_cnt "fetch_keys" 10000;
	    match reason with
	      | Unparseable
	      | No_valid_selfsig -> 
		  print_endline msg
	      | _ -> failwith "Expired and revoked keys should be ok"
    in
      Keydb.iter ~f:extract_key;
      print_endline (sprintf "keys altogether %d skipped %d" !key_cnt !skipped_cnt);
      (keys_so_far, subkeyids)
	  
  let run () =
    Keydb.open_dbs settings;
    let (keys, subkeyids) = time_eval fetch_keys "fetch_keys" in
      printf "number of bindings in subkeyids %d\n" (Hashtbl.length subkeyids);
      flush stdout;
      replace_subkeyids_in_sigs keys subkeyids;
      filter_sigs_to_missing_keys keys;
      dump_ekeys_to_file (Hashtbl.values keys) "ekeys_all.sexp"
end
