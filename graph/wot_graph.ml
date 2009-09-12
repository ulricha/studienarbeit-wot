open Batteries
open Printf
open Graph

open Ekey
open Misc

module V = struct
  type t = epki
  let compare = (fun k1 k2 -> compare k1.key_keyid k2.key_keyid)
  let hash = (fun k1 -> hash_keyid k1.key_keyid)
  let equal = (fun k1 k2 -> k1.key_keyid = k2.key_keyid)
end

module E = struct
  type t = string * string
  let compare = (fun (v1, v2) (v3, v4) ->
		   compare (v1 ^ v2) (v3 ^ v4))
end

module G = Imperative.Digraph.ConcreteBidirectional(V)

let load_storeable_graph_from_files vertex_filename edge_filename =
  let v_inc = File.open_in vertex_filename in
  let e_inc = File.open_in edge_filename in
  let rec load_vertices vertices =
    try
      let v = Marshal.input v_inc in
	load_vertices (v :: vertices)
    with IO.No_more_input -> vertices
  in
  let rec load_edges edges =
    try
      let e = Marshal.input e_inc in
	load_edges (e :: edges)
    with IO.No_more_input -> edges
  in
  let vertices = load_vertices [] in
  let edges = load_edges [] in
    (vertices, edges)

  let dump_storeable_graph_to_file vertex_filename edge_filename g =
    let (vertex_list, edge_list) = g in
    let v_channel = open_out vertex_filename in
    let e_channel = open_out edge_filename in
      List.iter	(fun v -> Marshal.to_channel v_channel v []) vertex_list;
      List.iter	(fun e -> Marshal.to_channel e_channel e []) edge_list;
      close_out v_channel;
      close_out e_channel

let add_edges g vertex_tbl edge_list =
  let siginfos = Hashtbl.create 700000 in
  let add_edges_from_one_vertex edges =
    let (signee, siglist) = edges in
      try
	let signee_vertex = Hashtbl.find vertex_tbl signee in
	  List.iter 
	    (fun s ->
	       let (signer, siginfo) = s in
		 try
		   let signer_vertex = Hashtbl.find vertex_tbl signer in
		     Hashtbl.add siginfos (signer, signee) siginfo;
		     G.add_edge g signer_vertex signee_vertex
		 with Not_found -> print_endline ("key not in hashtbl: " ^ (keyid_to_string signer)))
	    siglist
      with Not_found -> ()
  in
    List.iter (fun edges -> add_edges_from_one_vertex edges) edge_list

let create_graph storeable_graph =
  let vertex_tbl = Hashtbl.create 320000 in
  let (vertices, edges) = storeable_graph in
  let g = G.create ~size:300000 () in
    List.iter (fun v -> G.add_vertex g v; Hashtbl.add vertex_tbl v.key_keyid v) vertices;
    add_edges g vertex_tbl edges;
    g

let () =
  print_endline "started";
  let vertex_fname = Sys.argv.(1) in
  let edge_fname = Sys.argv.(2) in
  let storeable_g = time_evaluation (fun () -> load_storeable_graph_from_files vertex_fname edge_fname) "load_storeable_graph" in
  let g = time_evaluation (fun () -> create_graph storeable_g) "create_graph" in
    printf "vertices: %d edges: %d\n" (G.nb_vertex g) (G.nb_edges g) 
