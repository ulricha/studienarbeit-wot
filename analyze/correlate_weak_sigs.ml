open Batteries
open Printf
open Wot_graph
open Graph_misc

module C = Component_helpers.Make(G)
module M = Map.StringMap

let _ =
  if Array.length Sys.argv <> 3 then (
    print_endline "usage: simple_stats db edgefile";
    exit 1)

let main () =
  let dbh = PGOCaml.connect ~database:Sys.argv.(1) () in
  let (g, mscc) = Component_helpers.load_mscc Sys.argv.(2) in
    print_endline "scc loaded";
  let g_mscc = C.graph_from_node_list mscc g in
  let records = Db_interface.divide_et_impera (Db_interface.get_key_records dbh) mscc in
    print_endline "records loaded";
  let records = 
    List.fold_left
      (fun m (keyid, _, _, _, alg, keylen) ->
	 if alg = 1l then
	   M.add keyid keylen m
	 else
	   m)
      M.empty
      records
  in
  let (out_512, out_768, out_1024) =
    List.fold_left
      (fun (out_512, out_768, out_1024) keyid ->
	 try
	   let keylen = M.find keyid records in
	   let deg = G.out_degree g_mscc keyid in
	     if keylen = 512l then
	       (deg :: out_512, out_768, out_1024)
	     else if keylen = 768l then
	       (out_512, deg :: out_768, out_1024)
	     else if keylen = 1024l then
	       (out_512, out_768, deg :: out_1024)
	     else
	       (out_512, out_768, out_1024)
	 with
	     Not_found -> 
	       print_endline "did not find key in db";
	       (out_512, out_768, out_1024))
      ([], [], [])
      mscc
  in
    write_int_values_to_file (List.enum out_512) "rsa-512-degs.dat";
    write_int_values_to_file (List.enum out_768) "rsa-768-degs.dat";
    write_int_values_to_file (List.enum out_1024) "rsa-1024-degs.dat"
      
let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
