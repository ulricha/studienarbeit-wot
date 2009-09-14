open Batteries

open Printf
open Misc

open Ekey
open Wot_graph

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

let () =
  print_endline "compute basic properties of wot graph";
  let vertex_fname = Sys.argv.(1) in
  let edge_fname = Sys.argv.(2) in
  let storeable_g = time_evaluation (fun () -> load_storeable_graph_from_files vertex_fname edge_fname) "load_storeable_graph" in
  let g = time_evaluation (fun () -> create_graph storeable_g) "create_graph" in
  let indeg_output = File.open_out "indeg.plot" in
  let outdeg_output = File.open_out "outdeg.plot" in
    degree_distribution g indeg_output outdeg_output
    

