open Batteries
open Graph
open Wot_graph
open Misc

let export_cfinder g fname =
  let write output =
    let write_edge u v =
      IO.nwrite output (Printf.sprintf "%s %s\n" u v)
    in
      G.iter_edges write_edge g
  in
    File.with_file_out fname write

let export_cfinder_undirected g fname =
  let gu = directed_to_undirected g in
  let write output =
    let write_edge u v =
      IO.nwrite output (Printf.sprintf "%s %s\n" u v)
    in
      GU.iter_edges write_edge gu
  in
    File.with_file_out fname write

let keyid_to_int g =
  let f v (map, index) = (Map.StringMap.add v index map), (index + 1) in
  let (m, _) = G.fold_vertex f g (Map.StringMap.empty, 0) in
    m

let export_igraph_index g fname =
  let m = keyid_to_int g in
  let write output = 
    let write_edge u v =
      let u_id = Map.StringMap.find u m in
      let v_id = Map.StringMap.find v m in
	IO.nwrite output (Printf.sprintf "%d %d\n" u_id v_id)
    in
      G.iter_edges write_edge g
  in
  let write_index output =
    let write_entry keyid index =
      IO.nwrite output (Printf.sprintf "%s %d\n" keyid index)
    in
      Map.StringMap.iter write_entry m
  in
    File.with_file_out fname write;
    let fname = fname ^ "_index" in
      File.with_file_out fname write_index

let _ = 
  if Array.length Sys.argv <> 5 then (
    print_endline "usage: simple_stats edge_file output_file output_file_undirected igraph_directed";
    exit 1)

module Components = Component_helpers.Make(G)

let main () =
  let (g, mscc) = Component_helpers.load_mscc Sys.argv.(1) in
  let mscc_g = Components.graph_from_node_list mscc g in
    export_cfinder mscc_g Sys.argv.(2);
    export_cfinder_undirected mscc_g Sys.argv.(3);
    export_igraph_index mscc_g Sys.argv.(4)

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
