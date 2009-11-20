open Batteries
open Unix

let regexp_email = Str.regexp ".*<\\(.*\\)>.*"
let regexp_tld = Str.regexp ".*@.+\\.\\([^\\.]+\\)$"
let regexp_sld = Str.regexp "[^\\.]+\\.[^\\.]+$"

let extract_regexp_group regexp strings =
  let extract l a =
    if Str.string_match regexp a 0 then
      (Str.matched_group 1 a) :: l
    else
      l
  in List.fold_left extract [] strings

let extract_sld strings =
  let extract l a =
    try
      let pos = String.index a '@' in
      let domain = Str.string_after a (pos + 1) in
	if Str.string_match regexp_sld domain 0 then (
	  print_endline "first match";
	  domain :: l)
	else 
	  let rec loop domain =
	    let pos = String.index domain '.' in
	    let maybe_sld = Str.string_after domain (pos + 1) in
	      Printf.printf "loop domain %s pos %d maybe_sld %s\n" domain pos maybe_sld;
	      if Str.string_match regexp_sld maybe_sld 0 then
		Some maybe_sld
	      else if maybe_sld <> "" then
		loop maybe_sld
	      else
		None
	  in
	    match loop domain with 
	      | Some sld -> sld :: l
	      | None -> l
    with _ -> l
  in List.fold_left extract [] strings

let format_time_option = function
  | Some t -> 
      let ts = gmtime t in
	Printf.sprintf "%d.%d.%d" ts.tm_mday ts.tm_mon (1900 + ts.tm_year)
  | None -> "void"

let print_key_records l =
  let print r =
    let (keyid, puid, ctime, exptime) = r in
    let ctime_string = format_time_option ctime in
    let exptime_string = format_time_option exptime in
    let s = Printf.sprintf 
      "%s %s %s %s" 
      keyid puid ctime_string exptime_string
    in
      print_endline s
  in
    List.iter print l

let creation_time dbh keyids =
  let ctimes = PGSQL(dbh) "select ctime from keys where keyid in $@keyids" in
  let ctimes = List.sort ctimes in
  let a = Array.of_list ctimes in
  let l = Array.length a in
  let median = Array.get a (l / 2) in
  let newest = Array.get a (l - 1) in
  let oldest = Array.get a 0 in
    (median, oldest, newest)

let sig_creation_times dbh keyids =
  let ctimes = PGSQL(dbh) "select ctime from sigs where signee in $@keyids and signer in $@keyids" in
    ctimes

let get_key_records dbh keyids =
  let keyids = List.map Misc.keyid_to_string keyids in
  PGSQL(dbh) "select keyid, puid, ctime, exptime from keys where keyid in $@keyids"

let _ =
  if Array.length Sys.argv <> 4 then (
    print_endline "investigate_components vertex.sexp edge.sexp min_size";
    exit 1)

let main () =
  let minsize = int_of_string Sys.argv.(3) in
    print_endline ("investigate smaller components down to size " ^ Sys.argv.(3));
    let dbh = PGOCaml.connect ~database:"wot" () in
    let (g, scc_list_sorted) = Component_helpers.load_scc_list Sys.argv.(1) Sys.argv.(2) in
    let rec loop l =
      match l with
	| hd :: tl when (List.length hd) > 30000 -> 
	    loop tl
	| hd :: tl when (List.length hd) > minsize -> 
	    let keyids = List.map Misc.keyid_to_string hd in
	    let records = get_key_records dbh keyids in
	      Printf.printf "\nmembers of scc %d\n" (List.length keyids);
	      print_key_records records;
	      loop tl
	| hd :: tl -> ()
	| [] -> ()
    in
      loop scc_list_sorted
      
let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
