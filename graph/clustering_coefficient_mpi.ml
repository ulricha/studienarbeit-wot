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

let write_values_to_file enum component_size =
  let write output =
    Enum.iter (fun (k, v) -> fprintf output "%s %f\n" (keyid_to_string k) v) enum
  in
  let fname = sprintf "cc-scc-%d.out" component_size in
    File.with_file_out fname write

(* mscc = maximum strongly connected component *)
let () =
  if (Array.length Sys.argv) <> 3 then
    begin
      print_endline "usage: basic_properties vertex.sexp edge.sexp";
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
	  print_endline "server started";
	  let res = Mpi_cc.server 0 mscc in
	    print_endline "server finished";
	    write_values_to_file (CC.M.enum res) (G.nb_vertex mscc)
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
	match component_list with
	  | component_nodelist :: tl when (List.length component_nodelist) > 30 ->
	      let component = C.graph_from_node_list component_nodelist g in
	      let res = CC.clustering_coefficient_all_vertices g in
		write_values_to_file (CC.M.enum res) (G.nb_vertex component);
		loop tl
	  | _ -> ()
      in
	loop smaller_components_nodelists
