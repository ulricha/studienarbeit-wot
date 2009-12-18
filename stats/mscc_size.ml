open Batteries
open Ekey
open Unix
open Wot_graph
open Graph_misc

module C = Component_helpers.Make(G)

let _ =
  let len = Array.length Sys.argv in
    if len <> 2 then (
      print_endline "usage: graph_snapshot db";
      exit 1)

let today = Unix.time ()

let compute_sizes dbh start interval =
  let fname = "mscc-size" in
  let rec loop time interval_counter map =
    if time +. interval > today then
      write_distribution_to_file "%d %d\n" (Map.IntMap.enum map) fname
    else
      let gm = Unix.gmtime time in
	Printf.printf "compute mscc size for %d-%d\n" (gm.tm_mon + 1) (gm.tm_year + 1900);
	let edges = Db_interface.get_valid_sigs dbh time in
	let edges = List.map (fun (u, v) -> (Option.get u, Option.get v)) edges in
	let g = graph_from_edgelist edges in
	let scc_list = C.scc_list g in
	let size = List.length (List.hd (list_list_sort_reverse scc_list)) in
	let m = Map.IntMap.add interval_counter size map in
	  loop (time +. interval) (interval_counter + 1) m
  in
    loop start 1 Map.IntMap.empty

let main () = 
  let dbh = PGOCaml.connect ~database:Sys.argv.(1) () in
    compute_sizes dbh 1259751600. 2592000.

let _ = 
  try
    main ()
  with e -> prerr_endline (Printexc.to_string e)

