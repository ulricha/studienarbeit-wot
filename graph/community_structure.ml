open Batteries
open Printf
open Wot_graph
open Misc
open Community_helpers

module MG_make = Metagraph.Make(G)

let _ =
  if (Array.length Sys.argv) <> 4 then (
    print_endline "usage: investigate_communities edge-file index-file community-file";
    exit (-1))

let component_metagraph g cid_map =
  let communities = Map.IntMap.fold (fun i c l -> c :: l) cid_map [] in
  let c = Metagraph.filter g communities in
    printf "filtered %d keys\n" c;
    let metagraph = (time_eval (fun () -> MG_make.metagraph g communities) "metagraph") in
      print_endline (sprintf "vertices %d edges %d" (Metagraph.MG.nb_vertex metagraph) (Metagraph.MG.nb_edges metagraph));
      let basename = "metagraph-communities" in
	Metagraph.export_metagraph_cfinder metagraph (basename ^ ".cyto");
	Metagraph.export_metagraph_attributes metagraph (basename ^ "_attributes.cyto")

let main () =
  print_endline "construct communities metagraph";
  let cid_map = import_igraph_communities Sys.argv.(3) Sys.argv.(4) in
  let edge_fname = Sys.argv.(1) in
  let g = load_graph_from_file edge_fname in 
    write_community_size_values cid_map "community_sizes.dat";
    component_metagraph g cid_map

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)

