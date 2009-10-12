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
      let l = fun () -> load_structinfo_from_files vertex_fname edge_fname in
      let storeable_g = time_evaluation l "load_structinfo" in
      let c = fun () -> graph_from_structinfo storeable_g in
      let g = time_evaluation c "graph_from_storeable_graph" in
      let scc_list = time_evaluation (fun () -> C.scc_list g) "scc_list" in
      let scc_list_sorted = list_list_sort_reverse scc_list in
      let cnt = ref 0 in
	C.overall_component_properties scc_list;
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
			cnt := 0;
			Statistics.complete_network_statistics scc name cnt;
			loop tl
		    end
	    | [] -> 
		()
	in
	  loop scc_list_sorted
    end
