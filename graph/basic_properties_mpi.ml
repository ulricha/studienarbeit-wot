open Batteries

open Printf
open Graph
open Misc
open Graph_misc
open Ekey
open Wot_graph

module C = Component_helpers.Make(G)
module Statistics = Network_statistics.Make(G)
module M = Map.StringMap

module Mpi_statistics = Mpi_framework.Make(Statistics.Distance_statistics_job)

let print_basic_values (nr_vertex, nr_edges, indeg_map, outdeg_map, avg_indeg) graph_name =
  print_endline ("basic_network_statistics " ^ graph_name);
  printf "vertices %d edges %d\n" nr_vertex nr_edges;
  printf "average indegree = average outdegree %f\n" avg_indeg;
  write_int_values_to_file (Statistics.M.values indeg_map) (graph_name ^ "_indeg.values");
  write_int_values_to_file (Statistics.M.values outdeg_map) (graph_name ^ "_outdeg.values");
  print_endline ""

(* mscc = maximum strongly connected component *)
let () =
  if (Array.length Sys.argv) <> 3 then
    begin
      print_endline "usage: basic_properties vertex.sexp edges.sexp";
      exit (-1)
    end
  else
    let rank = Mpi.comm_rank Mpi.comm_world in
    let (g, scc_list_sorted) = Component_helpers.load_scc_list Sys.argv.(1) Sys.argv.(2) in
    let mscc_nodelist = List.hd scc_list_sorted in
    let smaller_components_nodelists = List.tl scc_list_sorted in
    let mscc = C.graph_from_node_list mscc_nodelist g in
      if rank = 0 then
	begin
	  printf "edges total %d oneway %d\n" (G.nb_edges g) (Statistics.count_oneway_edges mscc);
	  C.overall_component_properties scc_list_sorted;
	  let n = G.nb_vertex mscc in
	  let component_name = sprintf "scc-%d" n in
	  let res = Statistics.basic_network_statistics g in
	    print_basic_values res "whole_graph";
	    print_endline "server started";
	    let res = Mpi_statistics.server 0 mscc in
	      print_endline "server finished";
	      Statistics.analyze_and_print_results n component_name res
	end
      else
	begin
	  printf "worker %d started\n" rank;
	  flush stdout;
	  Mpi_statistics.worker [mscc]
	end;
      Mpi.barrier Mpi.comm_world;
      if rank <> 0 then exit 0;
      let fname = sprintf "scc-%d" (G.nb_vertex mscc) in
      let res = Statistics.basic_network_statistics mscc in
	print_basic_values res fname;
	let rec loop component_list =
	  match component_list with
	    | component_nodelist :: tl when (List.length component_nodelist) > 30 ->
		let component = C.graph_from_node_list component_nodelist g in
		let component_name = sprintf "scc-%d" (G.nb_vertex component) in
		let basic_res = Statistics.basic_network_statistics component in
		  print_basic_values basic_res component_name;
		  Statistics.distance_network_statistics_ser component component_name (fun () -> ());
		  loop tl
	    | _ -> ()
	in
	  loop smaller_components_nodelists

