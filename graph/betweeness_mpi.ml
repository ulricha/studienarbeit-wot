open Batteries

open Printf
open Graph
open Misc
open Graph_misc
open Ekey
open Wot_graph

module C = Component_helpers.Make(G)
module B = Betweeness.Make(G)

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

let accumulate_results numworkers n =
  let result_map = Map.StringMap.empty in
  let rec loop_results map unfinished_workers =
    match unfinished_workers with
      | 0 -> map
      | x ->
	  printf "waiting for %d workers to finish\n" x;
	  let result = Mpi.receive Mpi.any_source 0 Mpi.comm_world in
	    printf "received result %d from worker\n" (numworkers -x);
	    let combine_results map alist =
	      List.fold_left
		(fun m (k, v) -> 
		   try
		     let prev = Map.StringMap.find k m in
		       Map.StringMap.add k (v +. prev) m
		   with Not_found -> Map.StringMap.add k v m)
		map
		alist
	    in
	      loop_results (combine_results map result) (x - 1)
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
  let bench = time_iterations (sprintf "worker %d betweeness_round" rank) 100 in
  let result_tbl = B.betweeness_centrality_node_subset g bench work in
  let result = List.of_enum (B.H.enum result_tbl) in
    print_endline msg;
    Mpi.send result 0 0 Mpi.comm_world;
    print_endline (sprintf "worker %d finished" rank)

(* mscc = maximum strongly connected component *)
let () =
  if (Array.length Sys.argv) <> 3 then
    begin
      print_endline "usage: basic_properties vertex.sexp edge.sexp";
      exit (-1)
    end
  else
    let rank = Mpi.comm_rank Mpi.comm_world in
    let (g, mscc_nodelist) = Component_helpers.load_mscc Sys.argv.(1) Sys.argv.(2) in
    let mscc = C.graph_from_node_list mscc_nodelist g in
      if rank = 0 then
	begin
	  print_endline "server started";
	  let res = server mscc in
	    print_endline "server finished";
	    let write output =
	      Map.StringMap.iter 
		(fun k v -> fprintf output "k %s v %f\n" (keyid_to_string k) v; flush stdout) 
		res
	    in
	      File.with_file_out "mscc-betweeness.out" write
	end
      else
	begin
	  printf "worker %d started\n" rank;
	  flush stdout;
	  worker mscc
	end;
      Mpi.barrier Mpi.comm_world
