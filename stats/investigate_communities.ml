open Batteries
open Graph
open Db_interface
open Domain_time_statistics
open Community_helpers

(* 
1. Groessenverteilung
2. Inhaltsanalyse a la investigate_components: domains (puids), erstellungszeit, domainverteilung
   ctimes auf keys und signaturen
3. Zeichnen a la metagraph
4. 
*)


let print_statistics key_records uids_nested sig_ctimes =
  let key_ctimes = List.map (fun (_, _, ctime, _) -> ctime) key_records in
  (* let puids = List.map (fun (_, uid, _, _) -> uid) key_records in *)
  let adresses = List.map (extract_regexp_group regexp_email) uids_nested in
  let normalize = normalize_domain_list adresses in
  let tlds = normalize (extract_regexp_group regexp_tld) in
  let slds = normalize extract_slds in
  let (median, oldest, newest) = characterize_times key_ctimes in
  let (median, oldest, newest) = (format_time median, format_time oldest, format_time newest) in
  let (median_sig, oldest_sig, newest_sig) = characterize_times sig_ctimes in
  let (median_sig, oldest_sig, newest_sig) = 
    (format_time median_sig, format_time oldest_sig, format_time newest_sig) in
    print_endline "\nCreation times of keys:";
    Printf.printf "median %s oldest %s newest %s\n" median oldest newest;
    print_endline "\nCreation times of signatures:";
    Printf.printf "median %s oldest %s newest %s\n" median_sig oldest_sig newest_sig;
    print_endline "\nDistribution of Top-Level-Domains:";
    domain_distribution tlds 2;
    print_endline "\nDistribution of Second-Level-Domains:";
    domain_distribution slds 3

let community_statistics db m =
  let minsize = int_of_string Sys.argv.(5) in
  let dbh = PGOCaml.connect ~database:db () in
  let community_list = Map.IntMap.fold (fun _ c l -> c :: l) m [] in
  let community_list = Graph_misc.list_list_sort_reverse community_list in
  let rec loop l =
    match l with
      | keyids :: tl when (List.length keyids) > minsize && (List.length keyids) > 100 ->
	  let records = divide_et_impera (get_key_records dbh) keyids in
	  let sig_ctimes = divide_et_impera (sig_creation_times dbh) keyids in
	  let uids = get_uids_per_key dbh keyids in
	    assert (List.length records > 0);
	    Printf.printf "\nmembers of community %d\n" (List.length keyids);
	    print_statistics records uids sig_ctimes;
	    print_endline "";
	    loop tl
      | keyids :: tl when (List.length keyids) > minsize ->
	  let records = get_key_records dbh keyids in
	  let sig_ctimes = sig_creation_times dbh keyids in
	  let uids = get_uids_per_key dbh keyids in
	    assert (List.length records > 0);
	    Printf.printf "\nmembers of community %d\n" (List.length keyids);
	    print_statistics records uids sig_ctimes;
	    print_endline "";
	    loop tl
      | hd :: tl -> ()
      | [] -> ()
  in
    loop community_list

(*
*)

let _ =
  if (Array.length Sys.argv) <> 6 then (
    print_endline "usage: investigate_communities db edge-file index-file community-file minsize";
    exit (-1))

let main () =
  print_endline "investigate_communities";
  let cid_map = import_igraph_communities Sys.argv.(3) Sys.argv.(4) in
    community_statistics Sys.argv.(1) cid_map

let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
