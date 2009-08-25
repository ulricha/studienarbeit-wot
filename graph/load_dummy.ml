TYPE_CONV_PATH "Load_dummy"

open Batteries
open SExpr
open Printf
open Graph

open Ekey

let hash_keyid keyid =
  let x = 0 in
  let x = x lor (int_of_char keyid.[3] lsl 24) in
  let x = x lor (int_of_char keyid.[2] lsl 16) in
  let x = x lor (int_of_char keyid.[1] lsl 8) in
  let x = x lor (int_of_char keyid.[0]) in
    x

module V = struct
  type t = epki with sexp
  let compare = (fun k1 k2 -> compare k1.key_keyid k2.key_keyid)
  let hash = (fun k1 -> hash_keyid k1.key_keyid)
  let equal = (fun k1 k2 -> k1.key_keyid = k2.key_keyid)
end

module E = struct
  type t = V.t * V.t with sexp
  let compare = (fun (v1, v2) (v3, v4) ->
		   let k1 = v1.key_keyid ^ v2.key_keyid in
		   let k2 = v3.key_keyid ^ v2.key_keyid in
		     compare k1 k2)
end

type edge_with_label = (E.t * esiginfo) with sexp
type vertex_list = V.t list with sexp
type edge_list = edge_with_label list with sexp
type sexp_graph = vertex_list * edge_list with sexp
		   
module G = Imperative.Digraph.ConcreteBidirectional(V)

module Edge_siginfo_map = Map.Make(E)

let ekey_list_to_sexp_graph l =
  let vertex_list = List.fold_left 
    (fun vl ek -> ek.pki :: vl)
    []
    l
  in
  let single_key_signatures k =
    let destination = k.pki.key_keyid in
      List.fold_left 
	(fun siglist (source, info) -> ((source, destination), info) :: siglist)
	[]
	k.signatures
  in
  let edge_list = List.fold_left (fun el k -> (single_key_signatures k) @ el) in
    (vertex_list, edge_list)

let sexp_graph_to_graph g =
  ()

module Keyid_key_map = Map.Make(String)

let () =
  let filename = Sys.argv.(1) in
  let sexp_list = load_rev_sexps filename in
  let ekey_list = List.map ekey_of_sexp sexp_list in
    begin
      Gc.full_major ();
      printf "loaded %d ekeys from dump file\n" (List.length ekey_list);
      ignore (read_line ());
    end
    ;
    let tbl = Keyid_key_map.empty in
    let tbl = List.fold_left 
	      (fun tbl ekey -> Keyid_key_map.add ekey.pki.key_keyid ekey tbl)
	      tbl
	      ekey_list
    in
      ignore tbl
