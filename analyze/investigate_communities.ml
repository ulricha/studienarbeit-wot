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
  let size = List.length key_records in
    (* let puids = List.map (fun (_, uid, _, _) -> uid) key_records in *)
  let adresses = List.map (extract_regexp_group regexp_email) uids_nested in
  let normalize = normalize_domain_list adresses in
  let tlds = normalize (extract_regexp_group regexp_tld) in
  let slds = normalize extract_slds in
  let (median, oldest, newest) = characterize_times key_ctimes in
  let (median, oldest, newest) = (format_time median, format_time oldest, format_time newest) in
  let (median_sig, oldest_sig, newest_sig) = characterize_times sig_ctimes in
    check_time_correlation median_sig oldest_sig newest_sig;
    let (median_sig, oldest_sig, newest_sig) = 
      (format_time median_sig, format_time oldest_sig, format_time newest_sig) in
      print_endline "\nCreation times of keys:";
      Printf.printf "median %s oldest %s newest %s\n" median oldest newest;
      print_endline "\nCreation times of signatures:";
      Printf.printf "median %s oldest %s newest %s\n" median_sig oldest_sig newest_sig;
      print_endline "\nDistribution of Top-Level-Domains:";
      domain_distribution size tlds 0.1 20. "DOM_CAND_TLD" false;
      print_endline "\nDistribution of Second-Level-Domains:";
      domain_distribution size slds 0.5 10. "DOM_CAND_SLD" true

let community_statistics db m =
  let minsize = int_of_string Sys.argv.(6) in
  let dbh = PGOCaml.connect ~database:db () in
  let community_list = Map.IntMap.fold (fun _ c l -> c :: l) m [] in
  let community_list = Graph_misc.list_list_sort_reverse community_list in
  let rec loop l =
    match l with
      | keyids :: tl when (List.length keyids) > minsize && (List.length keyids) > 100 ->
	  let id = Component_helpers.canonical_component_name keyids in
	  let records = divide_et_impera (get_key_records dbh) keyids in
	  let sig_ctimes = divide_et_impera (sig_creation_times dbh) keyids in
	  let uids = get_uids_per_key dbh keyids in
	    Printf.printf "stats community %s size %d\n" id (List.length keyids); flush stdout;
	    assert (List.length records > 0);
	    print_statistics records uids sig_ctimes;
	    print_endline "";
	    loop tl
      | keyids :: tl when (List.length keyids) > minsize ->
	  let id = Component_helpers.canonical_component_name keyids in
	  let records = get_key_records dbh keyids in
	  let sig_ctimes = sig_creation_times dbh keyids in
	  let uids = get_uids_per_key dbh keyids in
	    assert (List.length records > 0);
	    Printf.printf "stats community %s size %d\n" id (List.length keyids); flush stdout;
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
  if (Array.length Sys.argv) <> 7 then (
    print_endline "usage: investigate_communities db edge-file format index-file community-file minsize";
    exit (-1))

let main () =
  print_endline "investigate_communities";
  let cid_map = 
    if Sys.argv.(3) = "copra" then
      import_copra_communities Sys.argv.(5)
    else if Sys.argv.(3) = "igraph" then
      import_igraph_communities Sys.argv.(4) Sys.argv.(5)
    else
      failwith "format = copra / igraph"
  in
    print_endline "imported communities";
    community_statistics Sys.argv.(1) cid_map

let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
