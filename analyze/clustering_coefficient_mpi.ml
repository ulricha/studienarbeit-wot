open Batteries

open Printf
open Graph
open Misc
open Graph_misc
open Ekey
open Wot_graph

module C = Component_helpers.Make(G)
module CC = Clustering_coefficient.Make(G)

module Mpi_cc = Mpi_framework.Make(CC.Clustering_coefficient_job)

(* mscc = maximum strongly connected component *)
let _ =
  if (Array.length Sys.argv) <> 2 then
      print_endline "usage: basic_properties edge_file";
      exit (-1)

let main () =
  let rank = Mpi.comm_rank Mpi.comm_world in
  let (g, scc_list_sorted) = Component_helpers.load_scc_list Sys.argv.(1) in
  let mscc_nodelist = List.hd scc_list_sorted in
  let smaller_components_nodelists = List.tl scc_list_sorted in
  let mscc = C.graph_from_node_list mscc_nodelist g in
    if rank = 0 then
      begin
	print_endline "server started";
	let res = Mpi_cc.server 0 mscc in
	  print_endline "server finished";
	  let fname = sprintf "scc-%d_cc.values" (G.nb_vertex mscc) in
	    write_float_values_to_file (CC.M.values res) fname
      end
    else
      begin
	printf "worker %d started\n" rank;
	flush stdout;
	Mpi_cc.worker [mscc]
      end;
    Mpi.barrier Mpi.comm_world;
    if rank <> 0 then exit 0;
    let rec loop component_list =
      let bench = time_iterations "clustering_coefficient" 100 in
	match component_list with
	  | component_nodelist :: tl when (List.length component_nodelist) > 30 ->
	      let component = C.graph_from_node_list component_nodelist g in
	      let res = CC.clustering_coefficient_all_vertices component bench in
	      let component_size = G.nb_vertex component in
	      let fname = sprintf "scc-%d_cc.values" component_size in
		print_endline ("compute clustering coefficient for %s" ^ fname);
		write_float_values_to_file (CC.M.values res) fname;
		loop tl
	  | _ -> ()
    in
      loop smaller_components_nodelists

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)

