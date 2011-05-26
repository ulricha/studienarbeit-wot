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

let _ =
  if (Array.length Sys.argv) <> 2 then
    begin
      print_endline "usage: basic_properties edge_file";
      exit (-1)
    end

let ca_keys = ["2BAE3CF6DAFFB000"; 
	       "DBD245FCB3B2A12C"; 
	       "44B8DDD6BB1D9F6D"; 
	       "77AE7F129E2BD1F2"; 
	       "D2BB0D0165D0FD58"; 
	       "31DE40D503484BD3"; 
	       "E7B7A9F1D24D8B7F"; 
	       "2D8CC79ABB62BBA7"; 
	       "F2B837A3FDCB1C33"; 
	       "B903D5D67A9D7B59"; 
	       "4E8A07C0F2D58DB1"; 
	       "7E2FEA2D890C0981"; 
	       "7EBA0D1DF7E87B9D"; 
	       "93EFA5FCFE93EAB9"; 
	       "A1E3AF9D35DBF565"; 
	       "C9A658256362BE8B"; 
	       "1E170E937282B245"]

let main () =
  print_endline "basic_properties";
  let rank = Mpi.comm_rank Mpi.comm_world in
    printf "rank %d\n" rank; flush stdout;
  let (g, scc_list_sorted) = Component_helpers.load_scc_list Sys.argv.(1) in
  let mscc_nodelist = List.hd scc_list_sorted in
  let mscc = C.graph_from_node_list mscc_nodelist g in
    printf "graph with cas: n = %d m = %d\n" (G.nb_vertex mscc) (G.nb_edges mscc);
    List.iter (G.remove_vertex mscc) ca_keys;
    let sccs = C.scc_list mscc in
    let mscc_nodelist = List.hd (list_list_sort_reverse sccs) in
    let mscc = C.graph_from_node_list mscc_nodelist mscc in
      printf "graph without cas: n = %d m = %d\n" (G.nb_vertex mscc) (G.nb_edges mscc);
      if rank = 0 then
	begin
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
      print_endline "workers finished"

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
