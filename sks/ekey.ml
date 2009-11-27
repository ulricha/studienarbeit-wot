TYPE_CONV_PATH "Ekey"

open Format
open Sexplib
open Sexp
open Conv
open Printf

open Misc

type esiginfo = { mutable sig_puid_signed: bool;
		  sig_level: int;
		  sig_ctime: float option;
		  sig_exptime: float option;
		  sig_revoktime: float option;
		  sig_hash_alg: int;
		  sig_pk_alg: int;
		} with sexp

type esignature = (string * esiginfo) with sexp

type epki = { key_keyid: string;
	      key_puid: string;
	      key_all_uids: string list;
	      key_ctime: float;
	      key_exptime: float option;
	      mutable key_revoktime: float option;
	      key_alg: int;
	      key_len: int;
	    } with sexp

type ekey = { pki: epki;
	      mutable signatures: esignature list;
	    } with sexp

type vertex_list = epki list with sexp
type sig_list_per_signee = string * ((string * esiginfo) list) with sexp
type edge_list = sig_list_per_signee list with sexp
type edgelist_per_vertex = string * (string list) with sexp
type vertex = string with sexp

let siginfo_to_esiginfo siginfo =
  { sig_puid_signed = false; 
    sig_level = siginfo.Index.sigtype; 
    sig_hash_alg = siginfo.Index.siginfo_hash_alg;
    sig_pk_alg = siginfo.Index.siginfo_pk_alg;
    sig_ctime = i64_to_float_option siginfo.Index.sig_creation_time;
    sig_exptime = i64_to_float_option siginfo.Index.sig_expiration_time;
    sig_revoktime = None;
  }

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

