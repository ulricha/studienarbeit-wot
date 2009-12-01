open Batteries
open Graph
open Wot_graph
open Misc

let export_cfinder g fname =
  let write output =
    let write_edge u v =
      IO.nwrite output (Printf.sprintf "%s %s\n" (keyid_to_string u) (keyid_to_string v))
    in
      G.iter_edges write_edge g
  in
    File.with_file_out fname write

let _ = 
  if Array.length Sys.argv <> 4 then (
    print_endline "usage: simple_stats vertex.sexp edge.sexp output_file";
    exit 1)

module Components = Component_helpers.Make(G)

let main () =
  let (g, mscc) = Component_helpers.load_mscc Sys.argv.(1) Sys.argv.(2) in
  let mscc_g = Components.graph_from_node_list mscc g in
    export_cfinder mscc_g Sys.argv.(3)

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
