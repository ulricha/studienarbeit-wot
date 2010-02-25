open Batteries
open Unix
open Misc

let regexp_email = Str.regexp ".*<\\(.*\\)>.*"
let regexp_tld = Str.regexp ".*@.+\\.\\([^\\.]+\\)$"
let regexp_sld = Str.regexp "[^\\.]+\\.[^\\.]+$"

let percentage p o = ((float_of_int p) /. (float_of_int o)) *. 100.

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
      let domain_parts = String.nsplit domain "." in
      let len = List.length domain_parts in
	if len = 2 then
	  (String.concat "." domain_parts) :: l
	else if len >= 3 then
	  (String.concat "." (List.drop (len - 3) domain_parts)) :: l
	else
	  l
    with _ -> l
  in List.fold_left extract [] strings

let normalize_domain_list domains_nested extract_function =
  let extracted = List.map extract_function domains_nested in
  let unique = List.map (List.sort_unique Pervasives.compare) extracted in
    List.map lowercase (List.concat unique)

let check_social_assignment p =
  if p >= 80. then
    print_endline "SURE_ASS"
  else if p >= 50. then
    print_endline "MAYBE_ASS"
  else
    print_endline "NOT_ASS"

let domain_distribution members domains min_threshold dominate_threshold dominate_string sld =
  let increment_by_one = Graph_misc.stringmap_add_or_create 1 in
  let map = List.fold_left increment_by_one Map.StringMap.empty domains in
  let alist = List.of_enum (Map.StringMap.enum map) in
  let compare (a1, b1) (a2, b2) = compare b1 b2 in
  let alist = List.sort ~cmp:(compare_reverse compare) alist in
  let alist = List.map (fun (k, v) -> (k, v), (percentage v members)) alist in
  let candidates = List.filter (fun (_, p) -> p >= dominate_threshold) alist in
  let candidates_strings = List.map (fun ((domain, _), _) -> domain) candidates in
    if (List.length alist) > 0 then
      begin
	if (List.length candidates) > 0 then
	  begin
	    if sld then
	      begin
		let (_, max_p) = List.hd candidates in
		  check_social_assignment max_p
	      end;
	      Printf.printf "\n%s %s\n\n" dominate_string (String.concat " " candidates_strings);
	  end;
	List.iter 
	  (fun ((k, v), p) -> 
	     if p < min_threshold then 
	       ()
	     else
	       Printf.printf "%s %d (%.2f %%)\n" k v p)
	  alist
      end

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

let check_time_correlation median oldest newest =
  let month = (60. *. 60. *. 24. *. 30.) in
    if ((median -. oldest) < month) && ((newest -. median) < month)  then
      print_endline "IS_KSP_CANDIDATE"
    else
      print_endline "NO_KSP_CANDIDATE"
      
