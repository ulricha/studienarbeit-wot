open Batteries
open Printf
open Graph
open SExpr

open Ekey
open Misc

module V = struct
  type t = vertex
  let compare = Standard.compare
  let hash = (fun k1 -> hash_keyid k1)
  let equal = (=)
end

module E = struct
  type t = string * string
  let compare = (fun (v1, v2) (v3, v4) ->
		   compare (v1 ^ v2) (v3 ^ v4))
end

module G = Imperative.Digraph.ConcreteBidirectional(V)

(* undirected graph *)
module GU = Imperative.Graph.Concrete(V)

module Key_map = Map.Make(V)

let directed_to_undirected g =
  let gu = GU.create ~size:(G.nb_vertex g) () in
    G.iter_edges (fun u v -> GU.add_edge gu u v) g;
    gu

let load_structinfo_from_files vertex_filename edge_filename =
  let edges = List.map edgelist_per_vertex_of_sexp (load_rev_sexps edge_filename) in
    ([], edges)
	 
let add_edges g edge_list =
  let add_edges_from_one_vertex edges =
    let (signee, siglist) = edges in
      List.iter 
	(fun signer -> G.add_edge g signer signee)
	siglist
  in
    List.iter (fun edges -> add_edges_from_one_vertex edges) edge_list

let graph_from_structinfo structinfo =
  let (vertices, edges) = structinfo in
  let g = G.create ~size:300000 () in
    add_edges g edges;
    g
