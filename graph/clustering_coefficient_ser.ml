open Batteries

open Printf
open Graph
open Misc
open Graph_misc
open Ekey
open Wot_graph

module C = Component_helpers.Make(G)
module CC = Clustering_coefficient.Make(G)

let write_values_to_file enum component_size =
  let write output =
    Enum.iter (fun (k, v) -> fprintf output "%s %f\n" (keyid_to_string k) v) enum
  in
  let fname = sprintf "cc-scc-%d.out" component_size in
    File.with_file_out fname write

(* mscc = maximum strongly connected component *)
let _ =
  if (Array.length Sys.argv) <> 3 then
      print_endline "usage: basic_properties vertex.sexp edges.sexp";
      exit (-1)

let main () =
  print_endline "compute basic properties of wot graph";
  let (g, mscc_nodelist) = Component_helpers.load_mscc Sys.argv.(1) Sys.argv.(2) in
  let bench = time_iterations "clustering_coefficient" 1000 in
  let mscc = C.graph_from_node_list mscc_nodelist g in
  let res = CC.clustering_coefficient_all_vertices mscc bench in
    write_values_to_file (CC.M.enum res) (G.nb_vertex mscc)

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
