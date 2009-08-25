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

type esiginfo = { mutable sig_puid_signed: bool;
		   sig_level: int;
		   sig_ctime: float;
		   sig_hash_alg: int;
		   sig_pk_alg: int;
		 } with sexp

type esignature = (string * esiginfo) with sexp

type epki = { key_keyid: string;
		     key_puid: string;
		     key_ctime: float;
		     key_alg: int;
		     key_len: int;
		   } with sexp

type ekey = { pki: epki;
	      mutable signatures: esignature list;
	    } with sexp

module Signature_set = Set.Make(struct
				  type t = esignature
				  let compare = 
				    (fun (i1, si1) (i2, si2) -> 
				       Pervasives.compare i1 i2)
				end)

module Key_set = Set.Make(struct
			    type t = ekey
			    let compare =
			      (fun k1 k2 ->
				 Pervasives.compare k1.pki.key_keyid k2.pki.key_keyid)
			  end)

let hexstring digest = 
  let result = String.create (String.length digest * 2) in
  let hex = "0123456789ABCDEF" in
    for i = 0 to String.length digest - 1 do
      let c = Char.code digest.[i] in
	result.[2*i] <- hex.[c lsr 4];
	result.[2*i+1] <- hex.[c land 0xF]
    done;
    result

let keyid_to_string ?(short=true) keyid = 
  let hex = hexstring keyid in
  if short
  then String.sub hex (String.length hex - 8) 8
  else hex

let string_of_ekey ks =
  let out = Buffer.create 70 in
  let keyid_string = keyid_to_string ks.pki.key_keyid in
    Buffer.add_string out keyid_string;
    Buffer.add_char out ' ';
    Buffer.add_string out ks.pki.key_puid;
    Buffer.add_char out ' ';
    let algo_string = (Packet.pubkey_algorithm_string ks.pki.key_alg) in
      Buffer.add_string out (sprintf "(%s - %d bit)" algo_string ks.pki.key_len);
    Buffer.add_char out ' ';
    Buffer.add_string out "signed by ";
    List.iter (fun (issuer, esiginfo) ->
		 Buffer.add_string out (keyid_to_string issuer);
		 let s =  (sprintf " (pk %s %d bit h %s) " 
			     (Packet.pubkey_algorithm_string esiginfo.sig_pk_alg) 
			     ks.pki.key_len
			     (Packet.hash_algorithm_string esiginfo.sig_hash_alg)) in
		   Buffer.add_string out s;
	      )
      ks.signatures;
    Buffer.contents out

