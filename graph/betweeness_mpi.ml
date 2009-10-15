open Batteries

open Printf
open Graph
open Misc
open Graph_misc
open Ekey
open Wot_graph

module C = Component_helpers.Make(G)
module B = Betweeness.Make(G)

let sort_alist_by_value l =
  let cmp (k1, v1) (k2, v2) = compare v1 v2 in
    List.sort ~cmp:(compare_reverse cmp) l

let betweeness_centrality betweeness_function g name cnt =
  let betweeness_values = betweeness_function g cnt in
  let l = List.of_enum (B.H.enum betweeness_values) in
  let sorted = sort_alist_by_value l in
  let (keyid, value) = List.hd sorted in
    printf "most central key in %s: %s (%f)\n" name (keyid_to_string keyid) value

let combine_hashtbl_enum h alist =
  let add_or_create (key, value) =
    try
      B.H.replace h key ((B.H.find h key) + value)
    with Not_found -> B.H.add h key value
  in
    List.iter add_or_create alist

let distribute_work g numworkers =
  let v_list = G.fold_vertex (fun v l -> v :: l) g [] in
  let nr_per_worker = (List.length v_list) / numworkers in
  let rec divide_and_send_work worker list =
    match List.length list with
      | 0 -> ()
      | length when length < 2*nr_per_worker ->
	  printf "server: send workunit of size %d to worker %d\n" length worker;
	  flush stdout;
	  Mpi.send list worker 0 Mpi.comm_world
      | length ->
	  let (workunit, rest) = List.split_at nr_per_worker list in
	    printf "server: send workunit of size %d to worker %d\n" nr_per_worker worker;
	    flush stdout;
	    Mpi.send workunit worker 0 Mpi.comm_world;
	    divide_and_send_work (worker + 1) rest
  in
    divide_and_send_work 1 v_list

(*
let accumulate_results numworkers n =
  let result_tbl = B.H.create n in
    B.H.add result_tbl "foo" 6;
  let finished = ref 0 in
    while !finished < numworkers do
      begin
	print_endline "accumulate_receive";
	flush stdout;
	let res = Mpi.receive Mpi.any_source 0 Mpi.comm_world in
	  incr finished;
	  printf "received result %d from worker\n" !finished;
	  flush stdout;
	  (*combine_hashtbl_enum result_tbl res; *)
	  let f (key, value) =
	    print_endline (key ^ " " ^ (string_of_int value))
	    (*B.H.add result_tbl key value*)
	  in
	    List.iter f res;
	    print_endline "res done";
	    flush stdout
      end
    done
*)

let accumulate_results numworkers n =
  let result_map = Map.StringMap.empty in
  let rec loop_results map unfinished_workers =
    match unfinished_workers with
      | 0 -> map
      | x ->
	  printf "waiting for %d workers to finish\n" x;
	  let result = Mpi.receive Mpi.any_source 0 Mpi.comm_world in
	  let (key, value) = List.hd result in
	    printf "received result %d from worker\n" (numworkers -x);
	    let add_or_create key value =
	      try
		let prev = Map.StringMap.find key map in
		  Map.StringMap.add key (value +. prev) map
	      with Not_found -> Map.StringMap.add key value map
	    in
	      loop_results (add_or_create key value) (x - 1)
  in
    loop_results result_map numworkers

let server g =
  let numworkers = Mpi.comm_size Mpi.comm_world -1 in
    distribute_work g numworkers;
    print_endline "server: accumulate results";
    accumulate_results numworkers (G.nb_vertex g)
    
let worker g =
  let rank = Mpi.comm_rank Mpi.comm_world in
  let work = Mpi.receive 0 0 Mpi.comm_world in
  let msg = sprintf "worker %d: workunit size %d" rank (List.length work) in
    print_endline msg;
    ignore g;
    let key = sprintf "v-%d" rank in
    let result = [(key, 5.0)] in
      printf "worker %d send result\n" rank;
      flush stdout;
      Mpi.send result 0 0 Mpi.comm_world

let load_mscc v_fname e_fname =
  let storeable_g = load_structinfo_from_files v_fname e_fname in
  let g = graph_from_structinfo storeable_g in
  let scc_list = C.scc_list g in
  let scc_list_sorted = list_list_sort_reverse scc_list in
    C.graph_from_node_list (List.hd scc_list_sorted) g

(* mscc = maximum strongly connected component *)
let () =
  if (Array.length Sys.argv) <> 3 then
    begin
      print_endline "usage: basic_properties vertex.sexp edge.sexp";
      exit (-1)
    end
  else
    let rank = Mpi.comm_rank Mpi.comm_world in
    let mscc = load_mscc Sys.argv.(1) Sys.argv.(2) in
      if rank = 0 then
	begin
	  print_endline "server started";
	  let res = server mscc in
	    print_endline "server finished";
	    Map.StringMap.iter (fun k v -> printf "k %s v %f\n" k v; flush stdout) res
	end
      else
	begin
	  printf "worker %d started\n" rank;
	  flush stdout;
	  worker mscc
	end;
      Mpi.barrier Mpi.comm_world
