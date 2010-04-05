open Batteries
open Graph
open Wot_graph
open Misc

let insert_component_id dbh (keyid, id) =
  PGSQL(dbh) "insert into component_ids (keyid, component_id) values ($keyid, $id)"

let _ =
  if Array.length Sys.argv <> 3 then (
    print_endline "usage: db_scc_information db edge_file";
    exit 1)

let main () =
  let dbh = PGOCaml.connect ~database:Sys.argv.(1) () in
    print_endline "connected to db";
    let (g, scc_list) = Component_helpers.load_scc_list Sys.argv.(2) in

      let sum = List.fold_left (fun sum c -> sum + c) 0 in
      let nodes = sum (List.map (fun c -> List.length c) scc_list) in
      Printf.printf "scc count %d graph nodes %d scc nodes %d\n" (List.length scc_list) (G.nb_vertex g) nodes;
    (* let scc_list = List.filter (fun c -> (List.length c) > 1) scc_list in *)
    let cids_per_key = 
      List.mapi (fun i c -> List.map (fun keyid -> (keyid, (Int32.of_int i))) c) 
	scc_list 
    in
    let cids_per_key = List.flatten cids_per_key in
      List.iter (insert_component_id dbh) cids_per_key

let _ = 
  try 
    main ()
  with e -> prerr_endline (Printexc.to_string e)
      
      

