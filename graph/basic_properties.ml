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
      print_endline "usage: basic_properties vertex.sexp edges.sexp";
      exit (-1)
    end
  else
    begin
      print_endline "compute basic properties of wot graph";
      let (g, scc_list_sorted) = Component_helpers.load_scc_list Sys.argv.(1) Sys.argv.(2) in
      let bench = time_iterations "distance_statistics" 1000 in
	C.overall_component_properties scc_list_sorted;
	Statistics.basic_network_statistics g "complete_graph";
	let rec loop l =
	  match l with
	    | node_list :: tl ->
		let scc = C.graph_from_node_list node_list g in
		let n = G.nb_vertex scc in
		  if n <= 30 then
		    ()
		  else
		    begin
		      let name = "scc-" ^ (string_of_int n) in
			Statistics.complete_network_statistics scc name bench;
			loop tl
		    end
	    | [] -> 
		()
	in
	  loop scc_list_sorted
    end
