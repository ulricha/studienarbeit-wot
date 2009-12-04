open Batteries

open Printf
open Graph
open Misc
open Graph_misc
open Ekey
open Wot_graph

module C = Component_helpers.Make(G)
module B = Betweeness.Make(G)

let sort_alist_by_value l =
  let cmp (k1, v1) (k2, v2) = compare v1 v2 in
    List.sort ~cmp:(compare_reverse cmp) l

let betweeness_centrality betweeness_function g name cnt =
  let betweeness_values = betweeness_function g cnt in
  let sorted = List.of_enum (B.H.enum betweeness_values) in
  let (keyid, value) = List.hd sorted in
    printf "most central key in %s: %s (%f)\n" name keyid value

(* mscc = maximum strongly connected component *)
let () =
  if (Array.length Sys.argv) <> 3 then
    begin
      print_endline "usage: basic_properties vertex.sexp edge.sexp";
      exit (-1)
    end
  else
    begin
      print_endline "compute basic properties of wot graph";
      let vertex_fname = Sys.argv.(1) in
      let edge_fname = Sys.argv.(2) in
      let l = fun () -> load_structinfo_from_files vertex_fname edge_fname in
      let storeable_g = time_eval l "load_structinfo" in
      let c = fun () -> graph_from_structinfo storeable_g in
      let g = time_eval c "graph_from_storeable_graph" in
      let scc_list = time_eval (fun () -> C.scc_list g) "scc_list" in
      let scc_list_sorted = list_list_sort_reverse scc_list in
      let benchmark = time_iterations "betweeness_round" 100 in
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
			print_endline ("betweeness_centrality " ^ name);
			betweeness_centrality B.betweeness_centrality_iterative scc name benchmark;
			loop tl
		    end
	    | [] -> 
		()
	in
	  loop scc_list_sorted
    end
