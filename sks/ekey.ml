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


TYPE_CONV_PATH "Ekey"

open Format
open Sexplib
open Sexp
open Conv

type cert_level = Generic | Persona | Casual | Positive

let cert_level_of_int d =
  match d with
    | 0x10 -> Generic
    | 0x11 -> Persona
    | 0x12 -> Casual
    | 0x13 -> Positive
    | _ -> failwith (sprintf "cert_level_of_int: unexpected vale %d" d)

let string_of_cert_level l =
  match l with
    | Generic -> "Generic"
    | Persona -> "Persona"
    | Casual -> "Casual"
    | Positive -> "Positive"

type esignature = { mutable sig_puid_signed: bool;
		   sig_level: int;
		   sig_issuer: string;
		   sig_ctime: float;
		   sig_hash_alg: int;
		   sig_pk_alg: int;
		 } with sexp

type ekey = { key_keyid: string;
	      key_puid: string;
	      key_ctime: float;
	      key_alg: int;
	      key_len: int;
	      mutable key_signatures: esignature list
	    } with sexp

module Signature_set = Set.Make(struct
				  type t = esignature
				  let compare = 
				    (fun s1 s2 -> 
				       Pervasives.compare s1.sig_issuer s2.sig_issuer)
				end)

module Key_set = Set.Make(struct
			    type t = ekey
			    let compare =
			      (fun k1 k2 ->
				 Pervasives.compare k1.key_keyid k2.key_keyid)
			  end)
