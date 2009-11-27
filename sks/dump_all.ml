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
    let filter_single_key ekey =
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
      let after = List.length filtered in
	if before <> after then 
	  printf "filtered %d sigs\n" (before - after);
	ekey.signatures <- filtered
    in
      Enum.iter filter_single_key (Hashtbl.values tbl)

  let fetch_keys () =
    let key_cnt = ref 0 in
    let skipped_cnt = ref 0 in
    let keys_so_far = Hashtbl.create 3000000 in
    let extract_key ~hash ~key =
      try 
	let ekey = key_to_ekey key in
	  display_iterations key_cnt "fetch_keys" 100;
	  add_key_without_duplicate keys_so_far ekey
      with
	| Skipped_key (reason, keyid) ->
	    display_iterations skipped_cnt "skipped" 100;
	    display_iterations key_cnt "fetch_keys" 100;
	    match reason with
	      | Unparseable ->
		  print_endline (sprintf "skipped unparseable key %s" (keyid_to_string keyid))
	      | No_valid_selfsig ->
		  print_endline (sprintf "skipped key without selfsig %s" (keyid_to_string keyid))
	      | _ -> failwith "Expired and revoked keys should be ok"
    in
      Keydb.iter ~f:extract_key;
      print_endline (sprintf "keys altogether %d skipped %d" !key_cnt !skipped_cnt);
      keys_so_far
	  
  let run () =
    Keydb.open_dbs settings;
    let keys = time_eval fetch_keys "fetch_keys" in
      filter_sigs_to_missing_keys keys;
      dump_ekeys_to_file (Hashtbl.values keys) "ekeys_all.sexp"
end
