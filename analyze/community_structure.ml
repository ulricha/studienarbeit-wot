open Batteries
open Printf
open Wot_graph
open Misc
open Community_helpers

module MG_make = Metagraph.Make(G)
module C = Component_helpers.Make(G)

let export_community_subgraphs g node_lists basename =
  let node_lists = Graph_misc.list_list_sort_reverse node_lists in
    let rec loop l =
      match l with
	| nodes :: tl when let l = List.length nodes in l <= 50 ->
	    ()
	| nodes :: tl when let l = List.length nodes in (l > 4000) || (l > 1300 && l < 1800) || (l > 50 && l < 110) ->
	    let l = List.length nodes in
	    let g_induced = C.graph_from_node_list nodes g in
	    let id = Component_helpers.canonical_component_name nodes in
	    let fname = sprintf "%s-%s-%d.igraph" basename id l in
	      Export_helpers.export_igraph_index g_induced fname;
	      loop tl
	| nodes :: tl ->
	    loop tl
	| [] -> 
	    ()
    in
      loop node_lists

let component_metagraph g communities =
  let minsize = int_of_string Sys.argv.(4) in
    printf "len communities %d\n" (List.length communities);
    printf "len communities %d\n" (List.length communities);
    let c = Metagraph.filter g communities in
      printf "filtered %d keys\n" c;
      let metagraph = (time_eval (fun () -> MG_make.metagraph g communities) "metagraph") in
      let metagraph = Metagraph.directed_to_undirected metagraph in
	print_endline (sprintf "vertices %d edges %d" (Metagraph.MGU.nb_vertex metagraph) (Metagraph.MGU.nb_edges metagraph));
	let basename = sprintf "metagraph-communities-%d" minsize in
	  Metagraph.export_umetagraph_cfinder metagraph (basename ^ ".cyto");
	  Metagraph.export_umetagraph_attributes metagraph (basename ^ "_attributes.cyto")

let _ =
  if (Array.length Sys.argv) <> 5 then (
    print_endline "usage: community_structure edge-file index-file community-file minsize";
    exit (-1))

let main () =
  print_endline "construct communities metagraph";
  let minsize = int_of_string Sys.argv.(4) in
  let cid_map = import_igraph_communities Sys.argv.(2) Sys.argv.(3) in
  let communities = Map.IntMap.fold (fun i c l -> c :: l) cid_map [] in
  let communities = List.filter (fun l -> List.length l >= minsize) communities in
  let edge_fname = Sys.argv.(1) in
  let g = load_graph_from_file edge_fname in 
    write_community_size_values cid_map "community_sizes.dat";
    component_metagraph g communities;
    export_community_subgraphs g communities "community_subgraph"

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)

