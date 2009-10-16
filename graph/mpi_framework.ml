(* job description: (component_index:int * node_list:G.V.t list) *)
open Graph
open Printf
open Wot_graph
open Misc

module C = Component_helpers.Make(G)
module Stat = Network_statistics.Make(G)

module type Mpi_job = sig
  include Sig.G
  type worker_result
  val worker_function : G.t -> G.V.t list -> (unit -> unit) -> worker_result
  type combine_type
  val combine_start : combine_type
  val combine_results : combine_type -> worker_result -> combine_type
  val jobname : string
end

module Make(Job : Mpi_job) = struct

  let distribute_work component component_index numworkers =
    let v_list = G.fold_vertex (fun v l -> v :: l) component [] in
    let nr_per_worker = (List.length v_list) / numworkers in
    let rec divide_and_send_work worker vlist =
      match List.length vlist with
	| 0 -> ()
	| length when length < 2*nr_per_worker ->
	    printf "server: send workunit of size %d to worker %d\n" length worker;
	    flush stdout;
	    let job = (component_index, vlist) in
	      Mpi.send job worker 0 Mpi.comm_world
	| length ->
	    let (work, rest) = List.split_at nr_per_worker vlist in
	      printf "server: send workunit of size %d to worker %d\n" nr_per_worker worker;
	      flush stdout;
	      let job = (component_index, work) in
		Mpi.send job worker 0 Mpi.comm_world;
		divide_and_send_work (worker + 1) rest
    in
      divide_and_send_work 1 v_list

  let worker component_list_sorted =
    let rank = Mpi.comm_rank Mpi.comm_world in
    let (component_index, vlist) = Mpi.receive 0 0 Mpi.comm_world in
    let msg = sprintf "%s-worker %d: workunit size %d on component %d" 
      Job.jobname rank (List.length vlist) component_index in
      print_endline msg;
      let bench = time_iterations (sprintf "%s-worker %d" Job.jobname rank) 100 in
      let g = List.at component_list_sorted component_index in
      let result = Job.worker_function g vlist bench in
	Mpi.send result 0 0 Mpi.comm_world;
	print_endline (sprintf "%s-worker %d finished" Job.jobname rank)

  let accumulate_results numworkers =
    let rec loop_results combined_result unfinished_workers =
      match unfinished_workers with
	| 0 -> combined_result
	| x ->
	    printf "waiting for %d workers to finish\n" x;
	    flush stdout;
	    let result = Mpi.receive Mpi.any_source 0 Mpi.comm_world in
	      printf "received result %d from worker\n" (numworkers -x);
	      loop_results (Job.combine_results combined_result result) (x - 1)
    in
      loop_results Job.combine_start numworkers


  let server component_index component =
    let numworkers = Mpi.comm_size Mpi.comm_world -1 in
      distribute_work component component_index numworkers;
      print_endline "server: accumulate results";
      accumulate_results numworkers
end

