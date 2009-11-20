open Batteries
open Unix

let regexp_email = Str.regexp ".*<\\(.*\\)>.*"
let regexp_tld = Str.regexp ".*@.+\\.\\([^\\.]+\\)$"
let regexp_sld = Str.regexp "[^\\.]+\\.[^\\.]+$"

let extract_regexp_group regexp strings =
  print_endline "extract_regexp_group";
  let extract l a =
    if Str.string_match regexp a 0 then
      (Str.matched_group 1 a) :: l
    else
      l
  in List.fold_left extract [] strings

let extract_slds strings =
  print_endline "extract_slds";
  let extract l a =
    try
      let pos = String.index a '@' in
      let domain = Str.string_after a (pos + 1) in
	if Str.string_match regexp_sld domain 0 then (
	  domain :: l)
	else 
	  let rec loop domain =
	    let pos = String.index domain '.' in
	    let maybe_sld = Str.string_after domain (pos + 1) in
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

let domain_distribution domains =
  let increase_or_add m k =
    try
      let old = Map.StringMap.find k m in
	Map.StringMap.add k (old + 1) m
    with Not_found -> Map.StringMap.add k 1 m
  in
  let map = List.fold_left increase_or_add Map.StringMap.empty domains in
    Enum.iter (fun (k, v) -> Printf.printf "%s %d\n" k v) (Map.StringMap.enum map)

let format_time_option = function
  | Some t -> 
      let ts = gmtime t in
	Printf.sprintf "%d.%d.%d" ts.tm_mday ts.tm_mon (1900 + ts.tm_year)
  | None -> "void"
      
let format_time t = 
  let ts = gmtime t in
    Printf.sprintf "%d.%d.%d" ts.tm_mday ts.tm_mon (1900 + ts.tm_year)

let print_key_records l =
  let print r =
    let (keyid, puid, ctime, exptime) = r in
    let ctime_string = format_time ctime in
    let exptime_string = format_time_option exptime in
    let s = Printf.sprintf 
      "%s %s %s %s" 
      keyid puid ctime_string exptime_string
    in
      print_endline s
  in
    List.iter print l

let creation_time ctimes =
  print_endline "creation_time";
  let ctimes = List.sort ctimes in
  let a = Array.of_list ctimes in
  let l = Array.length a in
    let median = Array.get a (l / 2) in
    let newest = Array.get a (l - 1) in
    let oldest = Array.get a 0 in
      (median, oldest, newest)

let print_statistics l =
  let uids = List.map (fun (_, uid, _, _) -> uid) l in
  let ctimes = List.map (fun (_, _, ctime, _) -> ctime ) l in
  let adresses = extract_regexp_group regexp_email uids in
  let tlds = extract_regexp_group regexp_tld adresses in
  let slds = extract_slds adresses in
  let (median, oldest, newest) = creation_time ctimes in
  let (median, oldest, newest) = (format_time median, format_time oldest, format_time newest) 
  in
    print_endline "\nCreation times of keys:";
    Printf.printf "median %s oldest %s newest %s\n" median oldest newest;
    print_endline "\nDistribution of Top-Level-Domains:";
    domain_distribution tlds;
    print_endline "\nDistribution of Second-Level-Domains:";
    domain_distribution slds

let sig_creation_times dbh keyids =
  let ctimes = PGSQL(dbh) "select ctime from sigs where signee in $@keyids and signer in $@keyids" in
    ctimes

let get_key_records dbh keyids =
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
	      assert (List.length records > 0);
	      Printf.printf "\nmembers of scc %d\n" (List.length keyids);
	      print_statistics records;
	      print_key_records records;
	      print_endline "";
	      loop tl
	| hd :: tl -> ()
	| [] -> ()
    in
      loop scc_list_sorted
      
let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
