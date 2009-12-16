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

let search_in_array warray keyid =
  let keyid_lower_part = String.sub keyid 8 8 in
  let compare short_keyid =
    let short_keyid_hex = Printf.sprintf "%08lX" short_keyid in
      keyid_lower_part = short_keyid_hex
  in
    try
      ignore (Dyn_array.index_of compare warray)
    with
	Not_found -> Printf.printf "%s\n" keyid

      
let main () =
  let dbh = PGOCaml.connect ~database:Sys.argv.(1) () in
  let keyids = Db_interface.get_mscc_keys dbh in
    print_endline "fetched keyids from db";
  let (names, wotsap_keyids, _) = Wotsap_parser.read_wotsap_file Sys.argv.(2)  in
    Printf.printf "%d %d\n" (List.length keyids) (Dyn_array.length wotsap_keyids);
    Dyn_array.iteri (search_in_keyids names keyids) wotsap_keyids;
    print_endline ">>> other way round";
    List.iter (search_in_array wotsap_keyids) keyids

let _ =
    try main () with e -> prerr_endline (Printexc.to_string e)
