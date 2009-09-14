open Batteries

open Printf
open Misc

open Ekey
open Wot_graph

let count_degrees g output =
  let deg_map = Map.IntMap.empty in
  let deg_map = G.fold_vertex
      (fun v m -> 
	 let deg = List.length (G.succ g v) in
	   if deg = 14642 then
	     print_endline (keyid_to_string v.key_keyid);
	   try
	     Map.IntMap.add deg ((Map.IntMap.find deg m) + 1) m
	   with Not_found -> Map.IntMap.add deg 1 m
      )
      g
      deg_map
  in
    Map.IntMap.iter (fun deg count -> fprintf output "%d %d\n" deg count) deg_map 

let () =
  print_endline "compute basic properties of wot graph";
  let vertex_fname = Sys.argv.(1) in
  let edge_fname = Sys.argv.(2) in
  let storeable_g = time_evaluation (fun () -> load_storeable_graph_from_files vertex_fname edge_fname) "load_storeable_graph" in
  let g = time_evaluation (fun () -> create_graph storeable_g) "create_graph" in
  let output = File.open_out "deg.plot" in
    count_degrees g output
    

