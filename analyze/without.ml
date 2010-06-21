open Batteries
open Wot_graph
open Community_helpers
open Misc
open Graph_misc

module C = Component_helpers.Make(G)

let today = Unix.time ()

let remove_vertices g keyids =
  List.iter (G.remove_vertex g) keyids

let remove_edges g edges =
  List.iter (fun (u, v) -> G.remove_edge g u v) edges

let keys g =
  let vertices = G.fold_vertex (fun v l -> ((G.out_degree g v), v) :: l) g [] in
  let random_order = List.map snd vertices in
  let c = (fun (d1, _) (d2, _) -> compare d1 d2) in
  let by_outdeg = List.sort ~cmp:(compare_reverse c) vertices in
  let by_outdeg = List.map snd by_outdeg in
    (random_order, by_outdeg)

let calculate_multiple g =
  let (random, by_outdeg) = keys g in
  let c = ref 0 in
  let rec f keys results =
    match keys with
      | []
      | _ when (G.nb_vertex g) <= 12 ->
	  c := 0;
	  List.rev results
      | l -> 
	    let sccs = C.scc_list g in
	    let sccs = list_list_sort_reverse sccs in
	    let mscc_size = 
	      try
		List.length (List.hd sccs) 
	      with _ -> 0 
	    in
	      let avg_s = 
		let sccs = List.tl sccs in
		let sizes = List.map List.length sccs in
		  (float_of_int (List.fold_left (+) 0 sizes ))  /. (float_of_int (List.length sizes))
	      in
	      Printf.printf "take 5 -> %d (%d -> %d, %f)\n" !c (G.nb_vertex g) mscc_size avg_s;
	      let nr_sccs = List.length sccs in
	      let i = !c * 5 in
		incr c;
		flush stdout;
		let ids = List.take 5 l in
		  remove_vertices g ids;
		f (List.drop 5 keys) ((i, mscc_size, nr_sccs, avg_s) :: results)
  in
  let fst (a,_,_,_) = a in
  let second (_, b, _, _) = b in
  let third (_, _, c, _) = c in
  let fourth (_, _, _, d) = d in
    (*
  let results = f random [] in
    write_int_values_to_file (List.enum (List.map fst results)) "random_i";
    write_int_values_to_file (List.enum (List.map second results)) "random_mscc";
    write_int_values_to_file (List.enum (List.map third results)) "random_sccs";
    write_float_values_to_file (List.enum (List.map fourth results)) "random_avg_s"; *)
    let results = f by_outdeg [] in
      write_int_values_to_file (List.enum (List.map fst results)) "outdeg_i";
      write_int_values_to_file (List.enum (List.map second results)) "outdeg_mscc";
      write_int_values_to_file (List.enum (List.map third results)) "outdeg_sccs";
      write_float_values_to_file (List.enum (List.map fourth results)) "outdeg_avg_s"

let calculate_single g headline =
  let sccs = C.scc_list g in
  let sccs = list_list_sort_reverse sccs in
  let mscc_size = List.length (List.hd sccs) in
  let nr_sccs = List.length sccs in
    Printf.printf "%s mscc size %d nr sccs %d\n" headline mscc_size nr_sccs

let single dbh g =
  let md5_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE mscc_sigs.hash_alg = 1 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE mscc_sigs.hash_alg = 1 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let rsa768 = PGSQL(dbh) "SELECT keyid from mscc_keys where alg = 1 and keylen <= 768" in
  let rsa1024 = PGSQL(dbh) "SELECT keyid from mscc_keys where alg = 1 and keylen <= 1024" in
  let ca_keys = ["2BAE3CF6DAFFB000"; "DBD245FCB3B2A12C"; "44B8DDD6BB1D9F6D"; "77AE7F129E2BD1F2"; "D2BB0D0165D0FD58"; "31DE40D503484BD3"; "E7B7A9F1D24D8B7F"; "2D8CC79ABB62BBA7"; "F2B837A3FDCB1C33"; "B903D5D67A9D7B59"; "4E8A07C0F2D58DB1"; "7E2FEA2D890C0981"; "7EBA0D1DF7E87B9D"; "93EFA5FCFE93EAB9"; "A1E3AF9D35DBF565"; "C9A658256362BE8B"; "1E170E937282B245"] in
  let md5_sigs = List.map (fun (u, v) -> Option.get u, Option.get v) md5_sigs in
  let rsa768 = List.map Option.get rsa768 in
  let rsa1024 = List.map Option.get rsa1024 in
    let g' = G.copy g in
    remove_edges  g' md5_sigs;
    calculate_single g' "md5";
    let g' = G.copy g in
    remove_vertices g' rsa768;
    calculate_single g' "rsa768";
    let g' = G.copy g in
    remove_vertices  g' rsa1024;
    calculate_single g' "rsa1024";
    let g' = G.copy g in
    remove_vertices  g' ca_keys;
    calculate_single g' "ca_keys"

let _ =
  let len = Array.length Sys.argv in
    if len <> 3 then (
      print_endline "usage: without db edges";
      exit (-1))

let main () =
  let _dbh = PGOCaml.connect ~database:Sys.argv.(1) () in
  let (g, mscc) =  Component_helpers.load_mscc Sys.argv.(2)  in
    (* single dbh (C.graph_from_node_list mscc g) *)
    (calculate_multiple (C.graph_from_node_list mscc g)) 
    

let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)

