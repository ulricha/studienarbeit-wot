TYPE_CONV_PATH "Load_dummy"

open Batteries
open SExpr
open Printf
open Graph

open Ekey
open Misc

module V = struct
  type t = epki with sexp
  let compare = (fun k1 k2 -> compare k1.key_keyid k2.key_keyid)
  let hash = (fun k1 -> hash_keyid k1.key_keyid)
  let equal = (fun k1 k2 -> k1.key_keyid = k2.key_keyid)
end

module E = struct
  type t = string * string with sexp
  let compare = (fun (v1, v2) (v3, v4) ->
		   compare (v1 ^ v2) (v3 ^ v4))
end

module G = Imperative.Digraph.ConcreteBidirectional(V)

module Edge_siginfo_map = Map.Make(E)

module Keyid_key_map = Map.Make(String)

(*
let sexp_graph_to_graph g =
  let (vertices, edges) = g in
    List.iter 
*)

let load_sexp_graph_from_files vertex_filename edge_filename =
  let vertices = List.map epki_of_sexp (load_sexps vertex_filename) in
  let edges = List.map sig_list_per_signee_of_sexp (load_rev_sexps edge_filename) in
    (vertices, edges)

let create_graph sexp_graph =
  let (vertices, edges) = sexp_graph in
  let g = G.create ~size:300000 () in
    List.iter (fun v -> G.add_vertex g v) vertices;
    g
      
(*
let () =
  print_endline "started";
  let filename = Sys.argv.(1) in
  let sexp_list = load_rev_sexps filename in
    print_endline "loaded sexps";
    let ekey_list = List.fast_sort compare_ekey (List.map ekey_of_sexp sexp_list) in
      begin
	printf "loaded %d ekeys from dump file\n" (List.length ekey_list);
	printf "%d %d\n"  (List.length (List.sort_unique compare_ekey ekey_list)) (List.length ekey_list);
	let (vl, el) as g = time_evaluation (fun () -> ekey_list_to_sexp_graph ekey_list) in
	  time_evaluation (fun () -> dump_sexp_graph_to_file "vertex_list.sexp" "edge_list.sexp" g)
      end
*)

let () =
  print_endline "started";
  let vertex_fname = Sys.argv.(1) in
  let edge_fname = Sys.argv.(2) in
  let sexp_g = load_sexp_graph_from_files vertex_fname edge_fname in
    ignore (create_graph sexp_g)

(*
    let f () = 
      let tbl = Keyid_key_map.empty in
	List.fold_left 
	  (fun tbl ekey -> Keyid_key_map.add ekey.pki.key_keyid ekey tbl)
	  tbl
	  ekey_list
    in
      ignore (time_evaluation f)
*)
	  
 
