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

let extract_slds strings =
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

let domain_distribution domains threshold =
  let increment_by_one = Graph_misc.stringmap_add_or_create 1 in
  let map = List.fold_left increment_by_one Map.StringMap.empty domains in
    Enum.iter 
      (fun (k, v) -> 
	 if v < threshold then 
	   ()
	 else
	   Printf.printf "%s %d\n" k v) 
      (Map.StringMap.enum map)

let format_time_option = function
  | Some t -> 
      let ts = gmtime t in
	Printf.sprintf "%d.%d.%d" ts.tm_mday (ts.tm_mon + 1) (1900 + ts.tm_year)
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

let characterize_times ctimes =
  let ctimes = List.sort ctimes in
  let a = Array.of_list ctimes in
  let l = Array.length a in
    let median = Array.get a (l / 2) in
    let newest = Array.get a (l - 1) in
    let oldest = Array.get a 0 in
      (median, oldest, newest)
