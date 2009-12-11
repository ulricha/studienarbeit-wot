open Batteries

open Printf
open Graph
open Misc
open Graph_misc
open Ekey
open Wot_graph

module C = Component_helpers.Make(G)
module B = Betweeness.Make(G)

module Mpi_betweeness = Mpi_framework.Make(B.Betweeness_job)

(* mscc = maximum strongly connected component *)
let _ =
  if (Array.length Sys.argv) <> 3 then
    print_endline "usage: betweeness_mpi edge_filename";
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
	let res = Mpi_betweeness.server 0 mscc in
	  print_endline "server finished";
	  let fname = sprintf "scc-%d_bet.values" (G.nb_vertex mscc) in
	    write_float_values_to_file (Map.StringMap.values res) fname
      end
    else
      begin
	printf "worker %d started\n" rank;
	flush stdout;
	Mpi_betweeness.worker [mscc]
      end;
    Mpi.barrier Mpi.comm_world;
    if rank <> 0 then exit 0;
    let rec loop component_list =
      match component_list with
	| component_nodelist :: tl when (List.length component_nodelist) > 30 ->
	    let component = C.graph_from_node_list component_nodelist g in
	    let res = B.betweeness_centrality_iterative g (fun () -> ()) in
	    let fname = sprintf "scc-%d_bet.values" (G.nb_vertex component) in
	      write_float_values_to_file (B.H.values res) fname;
	      loop tl
	| _ -> ()
    in
      loop smaller_components_nodelists

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
