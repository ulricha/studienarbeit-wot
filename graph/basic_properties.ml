open Batteries

open Printf
open Graph
open Misc
open Graph_misc
open Ekey
open Wot_graph

module C = Component_helpers.Make(G)
module Statistics = Network_statistics.Make(G)

(* mscc = maximum strongly connected component *)
let () =
  if (Array.length Sys.argv) <> 3 then
    begin
      print_endline "usage: basic_properties vertex.mar edges.mar";
      exit (-1)
    end
  else
    begin
      print_endline "compute basic properties of wot graph";
      let vertex_fname = Sys.argv.(1) in
      let edge_fname = Sys.argv.(2) in
      let l = fun () -> load_storeable_graph_from_files vertex_fname edge_fname in
      let storeable_g = time_evaluation l "load_storeable_graph" in
      let c = fun () -> graph_from_storeable_graph storeable_g in
      let g = time_evaluation c "graph_from_storeable_graph" in
      let scc_list = time_evaluation (fun () -> C.scc_list g) "scc_list" in
      let mscc = C.largest_component_as_graph scc_list g in
	Statistics.basic_network_statistics g "complete_graph";
	C.overall_component_properties scc_list;
	Statistics.complete_statistics mscc "mscc";
    end
