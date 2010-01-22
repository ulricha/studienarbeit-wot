open Batteries
open Graph
open Wot_graph
open Misc
open Graph_misc
open Printf
open Metagraph

module C = Component_helpers.Make(G)
module M = Make(G)
module Statistics = Network_statistics.Make(MG)

let export_community_subgraphs g node_lists basename =
  let node_lists = Graph_misc.list_list_sort_reverse node_lists in
    let rec loop l =
      match l with
	| nodes :: tl when let l = List.length nodes in l <= 90 ->
	    ()
	| nodes :: tl when let l = List.length nodes in (l > 1300 && l < 1800) || (l > 50 && l < 110) ->
	    let l = List.length nodes in
	    let g_induced = C.graph_from_node_list nodes g in
	    let fname = sprintf "%s-%d.igraph" basename l in
	      Export_helpers.export_igraph_index g_induced fname;
	      loop tl
	| nodes :: tl ->
	    loop tl
	| [] -> 
	    ()
    in
      loop node_lists

let _ =
  if (Array.length Sys.argv) <> 3 then (
      print_endline "usage: component_structure edge_file min_size";
      exit (-1))

let main () =
  print_endline "construct components metagraph";
  let edge_fname = Sys.argv.(1) in
  let (g, scc_list) = Component_helpers.load_scc_list edge_fname in
  let min_size = int_of_string Sys.argv.(2) in
  let large_components = remove_small_components min_size g scc_list in
  let c = filter g large_components in
    printf "filtered %d keys - WTF?\n" c;
    let metagraph = (time_eval (fun () -> M.metagraph g large_components) "metagraph") in
    let oc = Pervasives.open_out "metagraph.dot" in 
      print_endline (sprintf "vertices %d edges %d" (MG.nb_vertex metagraph) (MG.nb_edges metagraph));
      remove_singletons metagraph;
      print_endline (sprintf "vertices %d edges %d" (MG.nb_vertex metagraph) (MG.nb_edges metagraph));
      Dot.output_graph oc metagraph;
      let basename = Printf.sprintf "metagraph-components-%d" min_size in
	export_metagraph_cfinder metagraph (basename ^ ".cyto");
	export_metagraph_attributes metagraph (basename ^ "_attributes.cyto");
	Pervasives.close_out oc;
	export_community_subgraphs g large_components "component_subgraph"

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)

