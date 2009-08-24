open SExpr
open Printf

TYPE_CONV_PATH "Ekey"

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
