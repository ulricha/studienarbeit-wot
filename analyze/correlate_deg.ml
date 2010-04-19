open Batteries
open Wot_graph
open Community_helpers
open Misc
open Graph_misc

module C = Component_helpers.Make(G)

let s g =
  G.fold_edges
    (fun u v acc ->
       acc + ((G.out_degree g u) * (G.out_degree g v)))
    g
    0

let in_out_corr g =
  G.fold_vertex
    (fun v l ->
       ((float_of_int (G.out_degree g v)) /. (float_of_int (G.in_degree g v))) :: l)
    g
    []

let _ =
  let len = Array.length Sys.argv in
    if len <> 2 then (
      print_endline "usage: correlate_deg edges";
      exit (-1))

let main () =
  let (g, mscc) =  Component_helpers.load_mscc Sys.argv.(1)  in
  let mscc_g = C.graph_from_node_list mscc g in
  let c = in_out_corr mscc_g in
    write_float_values_to_file (List.enum c) "in-out-correlation.dat"
    

let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)

