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
  open StdLabels
  open MoreLabels
  open Printf
  open Common
  open Packet
  open KeyMerge
  open Key
  open Fingerprint

  let settings = {
    Keydb.withtxn = !Settings.transactions;
    Keydb.cache_bytes = !Settings.cache_bytes;
    Keydb.pagesize = !Settings.pagesize;
    Keydb.dbdir = Lazy.force Settings.dbdir;
    Keydb.dumpdir = Lazy.force Settings.dumpdir;
  }

  module Keydb = Keydb.Unsafe

  let get_keys_by_keyid keyid =
    let keyid_length = String.length keyid in
    let short_keyid = String.sub ~pos:(keyid_length - 4) ~len:4 keyid in
    let keys = Keydb.get_by_short_subkeyid short_keyid in
    match keyid_length with
      | 4 -> (* 32-bit keyid.  No further filtering required. *)
	  keys

      | 8 -> (* 64-bit keyid *) 
	  List.filter keys
	  ~f:(fun key -> (Fingerprint.from_key key).Fingerprint.keyid = keyid )

      | 20 -> (* 160-bit v. 4 fingerprint *)
	  List.filter keys
	  ~f:(fun key -> keyid = (Fingerprint.from_key key).Fingerprint.fp )

      | 16 -> (* 128-bit v3 fingerprint.  Not supported *)
	  failwith "128-bit v3 fingerprints not implemented"

      | _ -> failwith "unknown keyid type"

  let itertest2 ~hash ~key =
    let rec print_uid key = 
      match key with
	| [] -> ()
	| packet :: tl -> 
	    if packet.packet_type = User_ID_Packet then
	      begin
		print_endline packet.packet_body;
		print_uid tl
	      end
	    else
	      print_uid tl
    in print_uid key

  let rec print_key_structure key = 
    match key with
      | packet :: tl -> 
	  print_endline (ptype_to_string packet.packet_type)
      | [] -> ()
  ;;

  let rec print_multiple_key_structure keyid =
    let rec print_single_key_structure key = 
      match key with
	| packet :: tl ->
	    print_endline (Packet.ptype_to_string packet.packet_type);
	    print_single_key_structure tl
	| [] ->
	    ()
    in
    let keys = get_keys_by_keyid keyid in
      print_endline (string_of_int (List.length keys));
      match keys with
	| key :: tl -> print_single_key_structure key
	| [] -> ()

  let run () = 
    Keydb.open_dbs settings;
    print_endline "foo";
    print_multiple_key_structure (keyid_of_string "0x9D6B4CE4")
    (* Keydb.iter itertest2 *)
end
