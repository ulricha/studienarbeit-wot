open Batteries

let _ =
  if Array.length Sys.argv <> 3 then
    begin
      print_endline "usage: investigate_wotsap db wotsap-file";
      exit 1
    end

let search_in_keyids names l i short_keyid =
  let short_keyid_hex = Printf.sprintf "%08lX" short_keyid in
  let rec loop l =
    match l with
      | hd :: tl ->
	  if (String.sub hd 8 8) = short_keyid_hex then
	    ()
	  else
	    loop tl
      | [] ->
	  Printf.printf "%s (%s)\n" short_keyid_hex (Dyn_array.get names i)
  in
    loop l
      
let main () =
  let dbh = PGOCaml.connect ~database:Sys.argv.(1) () in
  let keyids = Db_interface.get_mscc_keys dbh in
    print_endline "fetched keyids from db";
  let (names, wotsap_keyids, _) = Wotsap_parser.read_wotsap_file Sys.argv.(2)  in
    Printf.printf "%d %d\n" (List.length keyids) (Dyn_array.length wotsap_keyids);
    Dyn_array.iteri (search_in_keyids names keyids) wotsap_keyids

let _ =
    try main () with e -> prerr_endline (Printexc.to_string e)
