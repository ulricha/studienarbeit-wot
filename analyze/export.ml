open Batteries
open Graph
open Wot_graph
open Misc
open Export_helpers

let _ = 
  if Array.length Sys.argv <> 3 then (
    print_endline "usage: export edge_file output_basename";
    exit 1)

module Components = Component_helpers.Make(G)

let main () =
  let (g, mscc) = Component_helpers.load_mscc Sys.argv.(1) in
  let mscc_g = Components.graph_from_node_list mscc g in
    export_cfinder mscc_g Sys.argv.(2);
    export_cfinder_undirected mscc_g Sys.argv.(2);
    export_igraph_index mscc_g Sys.argv.(2);
    export_pajek mscc_g Sys.argv.(2);
    export_pajek_undir mscc_g Sys.argv.(2)

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
