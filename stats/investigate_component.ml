open Batteries
open Unix
open Wot_graph
open Db_interface
open Domain_time_statistics

let print_statistics key_records sig_ctimes =
  let uids = List.map (fun (_, uid, _, _) -> uid) key_records in
  let ctimes = List.map (fun (_, _, ctime, _) -> ctime ) key_records in
  let adresses = extract_regexp_group regexp_email uids in
  let tlds = extract_regexp_group regexp_tld adresses in
  let slds = extract_slds adresses in
  let (median, oldest, newest) = characterize_times ctimes in
  let (median, oldest, newest) = 
    (format_time median, format_time oldest, format_time newest) in
  let (median_sig, oldest_sig, newest_sig) = characterize_times sig_ctimes in
  let (median_sig, oldest_sig, newest_sig) = 
    (format_time median_sig, format_time oldest_sig, format_time newest_sig)
  in
    print_endline "\nCreation times of keys:";
    Printf.printf "median %s oldest %s newest %s\n" median oldest newest;
    print_endline "\nCreation times of signatures:";
    Printf.printf "median %s oldest %s newest %s\n" median_sig oldest_sig newest_sig;
    print_endline "\nDistribution of Top-Level-Domains:";
    domain_distribution tlds 1;
    print_endline "\nDistribution of Second-Level-Domains:";
    domain_distribution slds 1

let check_args () =
  if Array.length Sys.argv <> 4 then (
    print_endline "investigate_components db edge_file min_size";
    exit 1)

let main () =
  let minsize = int_of_string Sys.argv.(3) in
    print_endline ("investigate smaller components down to size " ^ Sys.argv.(3));
    let dbh = PGOCaml.connect ~database:Sys.argv.(1) () in
    let (g, scc_list_sorted) = Component_helpers.load_scc_list Sys.argv.(2) in
    let rec loop l =
      match l with
	| hd :: tl when (List.length hd) > 30000 -> 
	    loop tl
	| keyids :: tl when (List.length keyids) > minsize -> 
	    let records = get_key_records dbh keyids in
	    let sig_ctimes = sig_creation_times dbh keyids in
	      assert (List.length records > 0);
	      Printf.printf "\nmembers of scc %d\n" (List.length keyids);
	      print_statistics records sig_ctimes;
	      print_key_records records;
	      print_endline "";
	      loop tl
	| hd :: tl -> ()
	| [] -> ()
    in
      loop scc_list_sorted
      
let _ =
  try check_args (); main () with
    | e -> prerr_endline (Printexc.to_string e)
