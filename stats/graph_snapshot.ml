open Batteries
open Ekey

let _ =
  let len = Array.length Sys.argv in
    if len > 3 then (
      print_endline "usage: graph_snapshot db <time>";
      exit 1)

let append_sig map (signer, signee) =
  let (signer, signee) = (Option.get signer, Option.get signee) in
  try
    let old = Map.StringMap.find signee map in
      Map.StringMap.add signee (signer :: old) map
  with Not_found -> Map.StringMap.add signee [signer] map

let write_sigs_from_db basename sigs =
  let map = List.fold_left (fun m s -> append_sig m s) Map.StringMap.empty sigs in
  let output = File.open_out ("edge-" ^ basename ^ ".sexp") in
  let write output sigs =
    SExpr.output_mach output (sexp_of_edgelist_per_vertex sigs);
    IO.write output '\n'
  in
    Enum.iter (write output) (Map.StringMap.enum map);
    IO.close_out output

let write_keys_from_db basename keys =
  let output = File.open_out ("vertex-" ^ basename ^ ".sexp") in
  let write output key =
    SExpr.output_mach output (sexp_of_vertex key);
    IO.write output '\n'
  in
    List.iter (write output) keys;
    IO.close_out output

let main () = 
  let dbh = PGOCaml.connect ~database:Sys.argv.(1) () in
  let (basename, timestamp) = 
    if Array.length Sys.argv = 2 then
      ("today", Unix.time ())
    else
      (Sys.argv.(2), float_of_string Sys.argv.(2))
  in
  let keys = Db_interface.get_valid_signed_keys dbh timestamp in
  let sigs = Db_interface.get_valid_sigs dbh timestamp in
    Printf.printf "keys %d sigs %d\n" (List.length keys) (List.length sigs); 
    flush stdout;
    write_keys_from_db basename keys;
    write_sigs_from_db basename sigs

let _ = 
  try
    main ()
  with e -> prerr_endline (Printexc.to_string e)

