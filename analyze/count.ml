open Batteries
open Wot_graph
open Community_helpers

let _ =
  let len = Array.length Sys.argv in
    if len < 2 then (
      print_endline "usage: investigate_communities format index-file community-file";
      exit (-1))

let main () =
  print_endline "investigate_communities";
  if (Array.length Sys.argv) = 4 then
    let cid_map = 
      if Sys.argv.(1) = "copra" then
	import_copra_communities Sys.argv.(3)
      else if Sys.argv.(1) = "igraph" then
	import_igraph_communities Sys.argv.(2) Sys.argv.(3)
      else if Sys.argv.(1) = "infomap" then
	import_infomap_communities Sys.argv.(3)
      else if Sys.argv.(1) = "blondel" then
	import_blondel_communities Sys.argv.(2) Sys.argv.(3)
      else
	failwith "format = copra / igraph / infomap / blondel"
    in
      print_endline "imported communities";
      let members = 
	Map.IntMap.fold
	  (fun _id community members -> 
	     let len = List.length community in
	       if len >= 4 then
		 members + len
	       else
		 members)
	  cid_map
	  0
      in
	Printf.printf "communities > 4: %d members\n" members
  else
    let (g, scc_list_sorted) = Component_helpers.load_scc_list Sys.argv.(1)  in
      Printf.printf "vertices %d edges %d\n" (G.nb_vertex g) (G.nb_edges g)

let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)

