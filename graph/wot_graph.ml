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

module Key_map = Map.Make(V)

let load_structinfo_from_files vertex_filename edge_filename =
  let vertices = List.map vertex_of_sexp (load_sexps vertex_filename) in
  let edges = List.map edgelist_per_vertex_of_sexp (load_rev_sexps edge_filename) in
    (vertices, edges)

let graph_to_structinfo g =
  let vertex_list = G.fold_vertex (fun v l -> v :: l) g [] in
  let edge_map = G.fold_edges
    (fun signer signee map -> 
       if Key_map.mem signee map then
	 let old_entry = Key_map.find signee map in
	 let (_, l) = old_entry in
	 let new_entry = (signee, signer :: l) in
	   Key_map.add signee new_entry map
       else
	 let entry = (signee, [signer]) in
	 Key_map.add signee entry map
    )
    g
    Key_map.empty
  in
    (vertex_list, List.of_enum (Key_map.values edge_map))
	 
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
    List.iter (fun v -> G.add_vertex g v) vertices;
    add_edges g edges;
    g
