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
  open Index
  open ParsePGP

  let settings = {
    Keydb.withtxn = !Settings.transactions;
    Keydb.cache_bytes = !Settings.cache_bytes;
    Keydb.pagesize = !Settings.pagesize;
    Keydb.dbdir = Lazy.force Settings.dbdir;
    Keydb.dumpdir = Lazy.force Settings.dumpdir;
  }

  module Keydb = Keydb.Unsafe

  exception No_value

  let option_may f = function
    | None -> ()
    | Some v -> f v

  let option_map f = function
    | None -> None
    | Some v -> Some (f v)

  let option_default v = function
    | None -> v
    | Some v -> v

  let option_is_some = function
    | None -> false
    | _ -> true

  let option_is_none = function
    | None -> true
    | _ -> false

  let option_get = function
    | None -> raise No_value
    | Some v -> v

  let option_map_default f v = function
    | None -> v
    | Some v2 -> f v2

  type cert_level = Generic | Persona | Casual | Positive

  type signature = { sig_puid_signed : bool;
	       sig_level : cert_level;
	       sig_keyid : string
	     }

  type key = { key_keyid : string;
	       key_puid : string;
	       key_signatures : signature list
	       }

(*  
  let is_self_sig keyid sinfo =
    match sinfo.keyid with
      | Some id -> true
      | None -> false



  let is_puid siginfo =
    siginfo.is_primary_uid

*)
    
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

  let rec print_multiple_key_structure keyid =
    let rec print_single_key_structure key = 
      match key with
	| packet :: tl when packet.packet_type = Signature_Packet -> 
	    let s = sig_to_siginfo packet in
	      if s.is_primary_uid then
		begin
		  print_packet packet;
		  print_endline "is primary uid";
		  print_single_key_structure tl;
		end
	      else
		begin
		  print_packet packet;
		  print_single_key_structure tl;
		end
	| packet :: tl ->
	    print_packet packet;
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
      if option_is_some s.keyid then
	out := !out ^ "keyid " ^ (keyid_to_string (option_get s.keyid))
      else
	out := !out ^ "no keyid (wtf?)"
      ;
(*      if option_is_some s.keyid then
	out := !out ^ " keyid " ^ (keyid_to_string (option_get s.keyid))
      else
	() *)
      !out
  

  let print_packet_verbose packet =
    match packet.packet_type with
      | Signature_Packet -> 
	  let info = sig_to_siginfo packet in
	    print_endline (string_of_siginfo info)
      | _ -> print_packet packet



  let print_uid_verbose uid =
    let (uid_packet, sigpacket_list) = uid in
      print_endline "uid";
      print_packet_verbose uid_packet;
      print_endline "signatures";
      List.iter print_packet_verbose sigpacket_list

  let print_pkey p =
      begin
	print_endline "key";
	print_packet p.key;
	print_endline ("selfsigs len " ^ (string_of_int (List.length p.selfsigs)));
	List.iter print_packet_verbose p.selfsigs;
	print_endline ("uids len " ^ (string_of_int (List.length p.uids)));
	List.iter print_uid_verbose p.uids 
      end

  let key_to_pkey key =
    let stream = key_to_stream key in
    try Some (parse_keystr stream) with
      | Unparseable_packet_sequence -> None

  let run () = 
    Keydb.open_dbs settings;
    let keyid = keyid_of_string "0x9D6B4CE4" in
    let keys = get_keys_by_keyid keyid in
    let pkeys = List.map key_to_pkey keys in
      List.iter
	(function
	   | Some p -> print_pkey p
	   | None -> print_endline "unparseable packet sequence"
	)
	pkeys
      (* Keydb.iter itertest2 *)

  let discard a =
    ()

(*
  let run () =
    Keydb.open_dbs settings;
    let keylist = ref [] in
    let add_key ~hash ~key =
      keylist := key :: !keylist
    in
      begin
	print_endline (string_of_float (Unix.time ()));
	Keydb.iter ~f:add_key;
	print_endline (string_of_float (Unix.time ()));
	discard (read_line ())
      end
*)

  let is_v3_expired valid_days = function
      | Some d -> 
	  let today = Stats.round_up_to_day (Unix.gettimeofday ()) in
	  let ctime = Int64.to_float info.pk_ctime in
	  let valid = (float_of_int d) *. 24. *. 3600. in
	    ctime +. valid < today
      | None -> 
	  false

  let is_expired pkey =
    let info = parse_pubkey_info pkey.key in
    let keyid = shorten ~short (from_packet pkey.key).keyid in
      match info.pk_version with
	| 3 -> 
	    begin
	      match info.pk_expiration with
		| Some d -> 
		    let today = Stats.round_up_to_day (Unix.gettimeofday ()) in
		    let ctime = Int64.to_float info.pk_ctime in
		    let valid = (float_of_int d) *. 24. *. 3600. in
		      ctime +. valid < today
		| None -> 
		    false
	    end
	| 4 ->
	    List.map 
	      (fun (uid, sig_list) ->
		 let siginfo_list = List.map ~f:sig_to_siginfo sig_list in
		 let selfsigs = List.filter (fun siginfo -> is_selfsig ~keyid:keyid siginfo) siginfo_list in
		   

	| x -> failwith ("unexpected pk_version field " ^ (string_of_int x))

  let sig_is_revok siginfo =
    match siginfo.sigtype with
      | 0x20 | 0x28 | 0x30 -> true
      | _ -> false

  let is_revoked pkey = 
    let selfsigs = pkey.KeyMerge.selfsigs in
      List.exists ~f:(fun sign -> 
			sig_is_revok (sig_to_siginfo sign)
                     )
	selfsigs

  let run () =
    Keydb.open_dbs settings;
    let key_cnt = ref 0 in
    let revoked_cnt = ref 0 in
    let count_revoked ~hash ~key =
      let pkey = parse_keystr (key_to_stream key) in
      begin
	if is_revoked pkey then
	  incr revoked_cnt
	else
	  ()
	;
	if !key_cnt mod 10000 = 0 then
	  begin
	    incr key_cnt;
	    print_endline (string_of_int !key_cnt)
	  end
	else
	  incr key_cnt
      end
    in
      begin
	print_endline ("time " ^ (string_of_float (Unix.time ())));
	Keydb.iter ~f:count_revoked;
	print_endline ("time " ^ (string_of_float (Unix.time ())));
	print_endline ("revoked " ^ (string_of_int !revoked_cnt))
      end

    
	  
end
