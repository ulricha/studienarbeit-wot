open Batteries
open Ekey

let _ =
  let len = Array.length Sys.argv in
    if len > 3 then (
      print_endline "usage: graph_snapshot db <time>";
      exit 1)

let write_sigs_from_db basename sigs =
  let output = File.open_out ("edge-" ^ basename ^ ".graph") in
  let write output (signer, signee) =
    Printf.fprintf output "%s %s\n" (Option.get signer) (Option.get signee)
  in
    List.iter (write output) sigs;
    IO.close_out output

let main () = 
  let dbh = PGOCaml.connect ~database:Sys.argv.(1) () in
  let (basename, timestamp) = 
    if Array.length Sys.argv = 2 then
      ("today", Unix.time ())
    else
      (Sys.argv.(2), float_of_string Sys.argv.(2))
  in
  let sigs = Db_interface.get_valid_sigs dbh timestamp in
    Printf.printf "sigs %d\n" (List.length sigs); 
    flush stdout;
    write_sigs_from_db basename sigs

let _ = 
  try
    main ()
  with e -> prerr_endline (Printexc.to_string e)

