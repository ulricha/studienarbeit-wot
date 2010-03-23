open Batteries
open Db_interface
open Wot_graph
open Graph_misc
open Domain_time_statistics

module Int32Set = Set.Make(Int32)

module Record  = struct
  type t = string * string * float * float option * int32 * int32
  let compare (keyid1, _, _, _, _, _) (keyid2, _, _, _, _, _) = compare keyid1 keyid2
end

module RecordSet = Set.Make(Record)
(*
let divide_period s e interval =
  let rec eat l interval_start =
    let interval_end = interval_start +. interval in
      if interval_end >= e then
	(interval_start, e) :: l
      else
	eat ((interval_start, interval_end) :: l) (interval_end +. 1.)
  in
    eat [] s
*)
let divide_period lastmonth lastyear =
  let start = {Unix.tm_sec=0; Unix.tm_min=0; Unix.tm_hour=0; Unix.tm_mday=1; Unix.tm_mon=4; Unix.tm_year=92; Unix.tm_wday=5;
	       Unix.tm_yday=31; Unix.tm_isdst=false} in
  let rec eat l (start_ts, start_tm) =
    let (next_ts, next_tm) = Unix.mktime {start_tm with Unix.tm_mon = start_tm.Unix.tm_mon + 1 } in
      if next_tm.Unix.tm_mon >= lastmonth && next_tm.Unix.tm_year >= lastyear then
	List.rev l
      else
	eat ((start_ts, (next_ts -. 1.)) :: l) (next_ts, next_tm)
  in
    eat [] (Unix.mktime start)

let increment_by_one = Graph_misc.int32map_add_or_create 1

let common_keylens = 
  Int32Set.add 512l
    (Int32Set.add 768l
       (Int32Set.add 1024l
	  (Int32Set.add 2048l
	     (Int32Set.add 3072l
		(Int32Set.add 4096l
		   Int32Set.empty)))))
  

let count_algorithm_use records =
  let project_record m r =
    let (_, _, _, _, pk_alg, _) = r in
      increment_by_one m pk_alg
  in
    List.fold_left project_record Int32Map.empty records

let count_rsa_keylen records =
  let project_record m r =
    let (_, _, _, _, pk_alg, pk_keylen) = r in
      if pk_alg = 1l && Int32Set.mem pk_keylen common_keylens then
	increment_by_one m pk_keylen
      else
	m
  in
    List.fold_left project_record Int32Map.empty records

let count_dsa_keylen records =
  let project_record m r =
    let (_, _, _, _, pk_alg, pk_keylen) = r in
      if pk_alg = 17l && Int32Set.mem pk_keylen common_keylens then
	increment_by_one m pk_keylen
      else
	m
  in
    List.fold_left project_record Int32Map.empty records

let map_records_to_statistics f period_record_list =
  print_endline "map_records_to_statistics";
  List.map (fun (start, records) -> (start, f records)) period_record_list

let explode_maps stats_list =
  print_endline "explode_maps";
  let add_new_values keys (start, map) =
    let new_keys = Int32Set.of_enum (Int32Map.keys map) in
      Int32Set.union keys new_keys
  in
  let all_values = List.fold_left add_new_values Int32Set.empty stats_list in
  let single_key_values key maplist =
    let f l (start, map) = 
      let value = try Int32Map.find key map with Not_found -> 0 in
	(start, value) :: l
    in
      List.fold_left f [] maplist
  in
  let aggregate key l =
    let value_list = single_key_values key stats_list in
      (key, value_list) :: l
  in
    Int32Set.fold aggregate all_values []

let keyids_from_graph g =
  G.fold_vertex (fun v l -> v :: l) g []

let fetch_keys_per_period dbh cid =
(*   let period_list = divide_period 665362800. 1259622000. 2592000. in *)
  let period_list = divide_period 11 109 in
  let keys_per_period = Db_interface.get_keys_per_period_cid dbh period_list cid in
    List.mapi (fun i (start, records) -> (i, records)) keys_per_period

let fetch_keys_per_period_all dbh =
(*  let period_list = divide_period 665362800. 1259622000. 2592000. in *)
  let period_list = divide_period 11 109 in
  let keys_per_period = Db_interface.get_keys_per_period_all dbh period_list in
    List.mapi (fun i (start, records) -> (i, records)) keys_per_period

module C = Component_helpers.Make(G)

let map_len = List.map (fun (i, records) -> (i, List.length records))
  
let creation_stats numbers_per_period graph_name =
  let rec cumulative l result =
    match l with
      | (i, num) :: tl ->
	  (match result with 
	     | (_, sofar) :: _ ->
		 cumulative tl ((i, num + sofar) :: result)
	     | [] ->
		 cumulative tl ((i, num) :: result)
	  )
      | [] -> List.rev result
  in
  let cumulative_numbers = cumulative numbers_per_period [] in
  let fname = graph_name ^ "-key_creation_cumulative_stats" in
    write_distribution_to_file "%d %d\n" (List.enum cumulative_numbers) fname;
    let fname = graph_name ^ "-key_creation_stats" in
      write_distribution_to_file "%d %d\n" (List.enum numbers_per_period) fname

let mscc_monthly dbh =
(*  let period_list = divide_period 665362800. (Unix.time ()) 2592000. in *)
  let period_list = divide_period 11 109 in
  let months = List.map fst period_list in
  let fetch_and_compute timestamp =
    Printf.printf "fetch sigs %s\n" (format_time timestamp);
    flush stdout;
    let sigs = Db_interface.get_valid_sigs_upto dbh timestamp in
    let sigs = List.map (fun (u, v) -> (Option.get u), (Option.get v)) sigs in
    let g = graph_from_edgelist sigs in
    let sccs = C.scc_list g in
    let mscc = List.hd (list_list_sort_reverse sccs) in
    let size = List.length mscc in
      Printf.printf "month %s graph size %d mscc size %d\n" (format_time timestamp) (G.nb_vertex g) size;
      flush stdout;
      (timestamp, mscc)
  in
  let numbers = List.map fetch_and_compute months in
    List.mapi (fun i (_start, x) -> (i, x)) numbers

let mscc_sizes msccs =
  let numbers = List.map (fun (id, vs) -> (id, (List.length vs))) msccs in
  let fname = "mscc-size.dat" in
    write_distribution_to_file "%d %d\n" (List.enum numbers) fname

let algorithm_stats dbh keys_per_period graph_name =
  let algorithm_use_stats = map_records_to_statistics count_algorithm_use keys_per_period in
  let rsa_keylen_stats = map_records_to_statistics count_rsa_keylen keys_per_period in
  let dsa_keylen_stats = map_records_to_statistics count_dsa_keylen keys_per_period in
  let write basename (key, dist) =
    let fname = Printf.sprintf "%s-%ld" basename key in
      write_distribution_to_file "%d %d\n" (List.enum dist) fname
  in
    List.iter (write (graph_name ^ "-pkalg_use_stats")) (explode_maps algorithm_use_stats);
    List.iter (write (graph_name ^ "-rsa_keylen_stats")) (explode_maps rsa_keylen_stats);
    List.iter (write (graph_name ^ "-dsa_keylen_stats")) (explode_maps dsa_keylen_stats)

let mscc_stats dbh msccs =
  let records =
    List.map
      (fun (i, vs) -> 
	 let records = divide_et_impera (get_key_records dbh) vs in
	   (i, records))
      msccs
  in
    let records_per_interval = 
      List.fold_left
	(fun (prev_records, l) (i, records) ->
	   let current_records = RecordSet.of_enum (List.enum records) in
	   let diff = RecordSet.diff prev_records current_records in
	     Printf.printf "prev %d current %d diff %d\n" 
	       (RecordSet.cardinal prev_records) 
	       (RecordSet.cardinal current_records) 
	       (RecordSet.cardinal diff);
	     (current_records, (i, (RecordSet.elements diff)) :: l))
	(RecordSet.empty, [])
	records
    in
      algorithm_stats dbh (snd records_per_interval) "mscc"

let _ =
  if Array.length Sys.argv <> 2 then (
    print_endline "usage: db";
    exit 1)

let main () =
  let dbh = PGOCaml.connect ~database:Sys.argv.(1) () in
    let msccs = mscc_monthly dbh in
      mscc_sizes msccs;
      mscc_stats dbh msccs;
      let keys_g = fetch_keys_per_period_all dbh in
	algorithm_stats dbh keys_g "whole_graph";
	creation_stats (map_len keys_g) "whole_graph"

let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
      
