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

let load_sexp_graph_from_files vertex_filename edge_filename =
  let vertices = List.map epki_of_sexp (load_sexps vertex_filename) in
  let edges = List.map sig_list_per_signee_of_sexp (load_rev_sexps edge_filename) in
    (vertices, edges)

let create_graph sexp_graph =
  let (vertices, edges) = sexp_graph in
  let g = G.create ~size:300000 () in
    List.iter (fun v -> G.add_vertex g v) vertices;
    g

let () =
  print_endline "started";
  let vertex_fname = Sys.argv.(1) in
  let edge_fname = Sys.argv.(2) in
  let sexp_g = time_evaluation (fun () -> load_sexp_graph_from_files vertex_fname edge_fname) "load_sexp_graph" in
  let g = time_evaluation (fun () -> create_graph sexp_g) "create_graph" in
    printf "vertices: %d edges: %d\n" (G.nb_vertex g) (G.nb_edges g) 
