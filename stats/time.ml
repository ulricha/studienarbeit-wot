open Batteries
open Db_interface
open Wot_graph
open Graph_misc

module Int32Set = Set.Make(Int32)

let divide_period s e interval =
  let rec eat l interval_start =
    let interval_end = interval_start +. interval in
      if interval_end >= e then
	(interval_start, e) :: l
      else
	eat ((interval_start, interval_end) :: l) (interval_end +. 1.)
  in
    eat [] s

let increment_by_one = Graph_misc.int32map_add_or_create 1

let count_algorithm_use records =
  let project_record m r =
    let (_, _, _, _, pk_alg, _) = r in
      increment_by_one m pk_alg
  in
    List.fold_left project_record Int32Map.empty records

let count_rsa_keylen records =
  let project_record m r =
    let (_, _, _, _, pk_alg, pk_keylen) = r in
      if pk_alg = 1l then
	increment_by_one m pk_keylen
      else
	m
  in
    List.fold_left project_record Int32Map.empty records

let count_dsa_keylen records =
  let project_record m r =
    let (_, _, _, _, pk_alg, pk_keylen) = r in
      if pk_alg = 17l then
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
	((Int64.of_float start), value) :: l
    in
      List.fold_left f [] maplist
  in
  let aggregate key l =
    let value_list = single_key_values key stats_list in
      (key, value_list) :: l
  in
    Int32Set.fold aggregate all_values []

let keyids_from_graph g =
  G.fold_vertex (fun v l -> (Misc.keyid_to_string v) :: l) g []

let _ =
  if Array.length Sys.argv <> 3 then (
    print_endline "usage: vertex.sexp edge.sexp";
    exit 1)

let main () =
  let dbh = PGOCaml.connect ~database:"wot" () in
  let (g, mscc) = Component_helpers.load_mscc Sys.argv.(1) Sys.argv.(2) in
    print_endline "mscc loaded";
  let mscc = List.map (fun v -> Misc.keyid_to_string v) mscc in
  let period_list = divide_period 665362800. (Unix.time ()) 2592000. in
  let period_list = List.sort ~cmp:(fun (start1, _) (start2, _) -> compare start1 start2) period_list in
  let keys_per_period = Db_interface.get_keys_per_period dbh period_list mscc in
  let algorithm_use_stats = map_records_to_statistics count_algorithm_use keys_per_period in
  let rsa_keylen_stats = map_records_to_statistics count_rsa_keylen keys_per_period in
  let dsa_keylen_stats = map_records_to_statistics count_dsa_keylen keys_per_period in
  let write basename (key, dist) =
    let fname = Printf.sprintf "%s-%ld" basename key in
      write_distribution_to_file "%Ld %d\n" (List.enum dist) fname
  in
    List.iter (write "pkalg_use_stats") (explode_maps algorithm_use_stats);
    List.iter (write "rsa_keylen_stats") (explode_maps rsa_keylen_stats);
    List.iter (write "dsa_keylen_stats") (explode_maps dsa_keylen_stats)

let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
      
