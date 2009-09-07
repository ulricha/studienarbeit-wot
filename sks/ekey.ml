TYPE_CONV_PATH "Ekey"

open Format
open Sexplib
open Sexp
open Conv

open Misc

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

let compare_esignature esig1 esig2 =
  let (i1, _) = esig1 in
  let (i2, _) = esig2 in
    Pervasives.compare i1 i2

let compare_ekey k1 k2 = Pervasives.compare k1.pki.key_keyid k2.pki.key_keyid

let string_of_ekey ks =
  let out = Buffer.create 70 in
  let keyid_string = keyid_to_string ks.pki.key_keyid in
    Buffer.add_string out keyid_string;
    Buffer.add_char out ' ';
    Buffer.add_string out ks.pki.key_puid;
    Buffer.add_char out ' ';
    Buffer.add_string out ( sprintf "created %s "(string_of_float ks.pki.key_ctime));
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

