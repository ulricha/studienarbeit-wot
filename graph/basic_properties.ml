open Batteries

open Printf
open Graph
open Misc
open Ekey
open Wot_graph

module Wot_components = Components.Make(G)

let degree_distribution g in_output out_output =
  let (indeg_map, outdeg_map) = G.fold_vertex
      (fun v (in_map, out_map) -> 
	 let outdeg = G.out_degree g v in
	 let indeg = G.in_degree g v in
	 let out_map =
	   try
	     Map.IntMap.add outdeg ((Map.IntMap.find outdeg out_map) + 1) out_map
	   with Not_found -> Map.IntMap.add outdeg 1 out_map
	 in
	 let in_map =
	   try 
	     Map.IntMap.add indeg ((Map.IntMap.find indeg in_map) + 1) in_map
	   with Not_found -> Map.IntMap.add indeg 1 in_map
	 in
	   (in_map, out_map)
      )
      g
      (Map.IntMap.empty, Map.IntMap.empty)
  in
    Map.IntMap.iter (fun deg count -> fprintf out_output "%d %d\n" deg count) outdeg_map;
    Map.IntMap.iter (fun deg count -> fprintf in_output "%d %d\n" deg count) indeg_map

(* creating a new siginfo table for the new graph from the original one 
   is not necesarry because the old one can still be used *)
let graph_from_node_list nodes original_graph =
  let g = G.create () in
    List.iter (fun v -> G.add_vertex g v) nodes;
    let get_edge v1 v2 =
      if G.mem_edge original_graph v1 v2 then
	G.add_edge g v1 v2;
    in
      apply_all_pairs nodes nodes get_edge V.compare;
      g

let largest_component_as_graph scc_list original_graph =
  let compare_scc_length = 
    compare_reverse (fun l1 l2 -> compare (List.length l1) (List.length l2)) in
  let sorted_list = List.sort ~cmp:compare_scc_length scc_list in
  let largest = List.hd sorted_list in
    graph_from_node_list largest original_graph

let scc_properties scc_list =
  let l = List.map (fun scc -> List.length scc) scc_list in
  let size_list =List.sort ~cmp:(compare_reverse compare) l in
  let unique_size_list = List.sort_unique (compare_reverse compare) size_list in
    printf "largest component %d\n" (List.hd size_list);
    printf "number of components %d\n" (List.length scc_list);
    List.iter (fun size -> printf "%d " size) unique_size_list;
    print_endline "\n"

let scc_list_to_graph_list scc_list original_graph original_siginfo =
  List.map (fun scc -> graph_from_node_list scc original_graph) scc_list

(* mscc = maximum strongly connected component *)
let mscc_properties scc_list g =
  let mscc_g = largest_component_as_graph scc_list g in
  let mscc_nr_vertex = G.nb_vertex mscc_g in
  let mscc_nr_edges = G.nb_edges mscc_g in
    printf "mscc vertices %d mscc edges %d\n" mscc_nr_vertex mscc_nr_edges

let () =
  if (Array.length Sys.argv) <> 3 then
    begin
      print_endline "usage: basic_properties vertex.mar edges.mar";
      exit (-1)
    end
  else
    begin
      print_endline "compute basic properties of wot graph";
      let vertex_fname = Sys.argv.(1) in
      let edge_fname = Sys.argv.(2) in
      let storeable_g = time_evaluation (fun () -> load_storeable_graph_from_files vertex_fname edge_fname) "load_storeable_graph" in
      let g = time_evaluation (fun () -> create_graph storeable_g) "create_graph" in
      let indeg_output = File.open_out "indeg.plot" in
      let outdeg_output = File.open_out "outdeg.plot" in
      let scc_list = time_evaluation (fun () -> Wot_components.scc_list g) "scc_list" in
	degree_distribution g indeg_output outdeg_output;
	scc_properties scc_list;
	mscc_properties scc_list g 
    end
