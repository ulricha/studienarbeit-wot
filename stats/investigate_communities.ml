open Batteries
open Graph
open Db_interface
open Domain_time_statistics

(* 
1. Groessenverteilung
2. Inhaltsanalyse a la investigate_components: domains (puids), erstellungszeit, domainverteilung
   ctimes auf keys und signaturen
3. Zeichnen a la metagraph
4. 
*)

let read_index index_fname =
  let h = Hashtbl.create 45000 in
  let add l =
    let (keyid, numid) = String.split l " " in
      Hashtbl.add h (int_of_string numid) keyid
  in
    File.with_file_in index_fname (fun input -> Enum.iter add (IO.lines_of input));
    (fun numid -> Hashtbl.find h numid)

let add_map m k v =
  try
    let old = Map.IntMap.find k m in
      Map.IntMap.add k (v :: old) m
  with Not_found -> Map.IntMap.add k [v] m

(* return: cid->node, node->cid *)
let import_igraph_communities index_fname communities_fname =
  let numid_to_keyid = read_index index_fname in
  let add_line i l m =
    let cid = int_of_string l in
    let keyid = numid_to_keyid i in
      add_map m cid keyid
  in
  let fold_lines input =
    Enum.foldi add_line Map.IntMap.empty (IO.lines_of input)
  in
    File.with_file_in communities_fname fold_lines

let write_community_size_values m out_fname =
  let output = File.open_out out_fname in
  let write_size cid l =
    let len = List.length l in
    let line = Printf.sprintf "%d\n" len in
      IO.nwrite output line
  in
    Map.IntMap.iter write_size m;
    IO.close_out output

let print_statistics key_records uids sig_ctimes =
  let key_ctimes = List.map (fun (_, _, ctime, _) -> ctime) key_records in
  (* let puids = List.map (fun (_, uid, _, _) -> uid) key_records in *)
  let adresses = extract_regexp_group regexp_email uids in
  let tlds = extract_regexp_group regexp_tld adresses in
  let slds = extract_slds adresses in
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
    domain_distribution tlds 1;
    print_endline "\nDistribution of Second-Level-Domains:";
    domain_distribution slds 1

let community_statistics db m =
  let minsize = int_of_string Sys.argv.(4) in
  let dbh = PGOCaml.connect ~database:db () in
  let community_list = Map.IntMap.fold (fun _ c l -> c :: l) m [] in
  let community_list = Graph_misc.list_list_sort_reverse community_list in
  let rec loop l =
    match l with
      | keyids :: tl when (List.length keyids) > minsize ->
	  let records = get_key_records dbh keyids in
	  let sig_ctimes = sig_creation_times dbh keyids in
	  let uids = get_uids_per_key dbh keyids in
	    assert (List.length records > 0);
	    Printf.printf "\nmembers of community %d\n" (List.length keyids);
	    print_statistics records uids sig_ctimes;
	    print_key_records records;
	    print_endline "";
	    loop tl
      | hd :: tl -> ()
      | [] -> ()
  in
    loop community_list

let _ =
  if (Array.length Sys.argv) <> 6 then (
    print_endline "usage: investigate_communities db edge-file index-file community-file minsize";
    exit (-1))

let main () =
  print_endline "investigate_communities";
  let cid_map = import_igraph_communities Sys.argv.(2) Sys.argv.(3) in
    write_community_size_values cid_map "community_sizes.dat";
    community_statistics Sys.argv.(1) cid_map

let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
