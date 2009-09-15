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

let scc_properties scc_list =
  let compare_reverse a b =
    match compare a b with 
      | 0 -> 0
      | x when x > 0 -> -1
      | x -> 1
  in
  let l = List.map (fun scc -> List.length scc) scc_list in
  let size_list =List.sort ~cmp:compare_reverse l in
  let unique_size_list = List.sort_unique compare_reverse size_list in
    printf "largest component %d\n" (List.hd size_list);
    printf "number of components %d\n" (List.length scc_list);
    List.iter (fun size -> printf "%d " size) unique_size_list;
    print_endline "\n"

let apply_all_pairs l1 l2 f ~cmp =
  let rec outer_loop l =
    match l with
      | v :: tl ->
	  begin
	    let rec inner_loop l =
	      match l with 
		| inner_v :: tl ->
		    if (cmp v inner_v) = 0 then
		      inner_loop tl
		    else 
		      begin
			f v inner_v;
			inner_loop tl
		      end
		| [] -> 
		    ()
	    in
	      inner_loop l2;
	      outer_loop tl
	  end
      | [] -> ()
  in
    outer_loop l1

let graph_from_node_list nodes original_graph original_siginfo =
  let g = G.create () in
  let siginfo_tbl = Hashtbl.create 1000 in
    List.iter (fun v -> G.add_vertex g v) nodes;
    let get_edge v1 v2 =
      if G.mem_edge original_graph v1 v2 then
	begin
	  G.add_edge g v1 v2;
	  let siginfo = Hashtbl.find original_siginfo (v1, v2) in
	    Hashtbl.add siginfo_tbl (v1, v2) siginfo
	end
    in
      apply_all_pairs nodes nodes get_edge

let scc_list_to_graph_list scc_list original_graph original_siginfo =
  List.map (fun scc -> graph_from_node_list scc original_graph original_siginfo) scc_list

let () =
  print_endline "compute basic properties of wot graph";
  let vertex_fname = Sys.argv.(1) in
  let edge_fname = Sys.argv.(2) in
  let storeable_g = time_evaluation (fun () -> load_storeable_graph_from_files vertex_fname edge_fname) "load_storeable_graph" in
  let g = time_evaluation (fun () -> create_graph storeable_g) "create_graph" in
  let indeg_output = File.open_out "indeg.plot" in
  let outdeg_output = File.open_out "outdeg.plot" in
  let scc_list = time_evaluation (fun () -> Wot_components.scc_list g) "scc_list" in
    degree_distribution g indeg_output outdeg_output;
    scc_properties scc_list
    

