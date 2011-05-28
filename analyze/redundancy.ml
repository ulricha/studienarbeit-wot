open Batteries
open Printf
open Graph
open Graph_misc
open Misc

module Make(G : Sig.I) = struct
  module H = Hashtbl.Make(G.V)
  module M = Map.Make(G.V)
  module ES = Set.Make(struct
			 type t = G.V.t * G.V.t
			 let compare = compare
		       end) 

  module F = struct 
    type t = int
    type label = G.E.label
    let max_capacity _ = 1
    let min_capacity _ = 0
    let flow _ = 0
    let add = (+)
    let sub = (-)
    let zero = 0
    let compare = compare
  end

  module FF = Flow.Ford_Fulkerson(G)(F)

  let maxflow_value g s flow =
    G.fold_succ_e (fun e f -> f + (flow e)) g s

  let node_induced_subgraph g vertices =
    let subg = G.create ~size:(List.length vertices) () in
      List.iter (fun v -> G.add_vertex subg v) vertices;
      G.iter_edges_e
	(fun e ->
	   let src = G.E.src e in
	   let dst = G.E.dst e in
	     if G.mem_edge subg src dst then
	       G.add_edge_e subg e;
	)
	g;
      subg

  let edge_induced_subgraph g edges =
    let g' = G.create ~size:(G.nb_vertex g) () in
    let add_edge (u, v) =
      G.add_vertex g' u;
      G.add_vertex g' v;
      G.add_edge g' u v
    in
      List.iter add_edge edges;
      g'

  module Path = struct
    type t = G.V.t list
    let append v p = v :: p
    let edges p = 
      let nodes = List.rev p in
      let rec loop edges p =
	match p with 
	  | u :: v :: l -> loop ((u, v) :: edges) (v :: l)
	  | _ :: [] | [] -> edges
      in
	loop [] nodes
  end

  let push_node stack pred v = Stack.push ((pred, v) :: (Stack.top stack)) stack

  let add_path_edges peh path v =
    try 
      let v_edgeset = H.find peh v in
      let v_edgeset' = List.fold_left (fun s e -> ES.add e s) v_edgeset path in
	H.replace peh v (v_edgeset')
    with Not_found ->
      H.add peh v (List.fold_left (fun s e -> ES.add e s) ES.empty path)

  let increase_path_count h v =
    try
      let num = H.find h v in
	H.replace h v (num + 1)
    with Not_found ->
      H.add h v 1

  (* compute all simple paths of length <= max_depth to all nodes reachable from v *)
  let compute_paths max_depth g v =
    let h = H.create 65537 in
    let path_edges = H.create 65537 in
    let path_counts = H.create 65537 in
    let pathstack = Stack.create () in
    let rec visit d pred v =
      H.add h v ();
      push_node pathstack pred v;
      add_path_edges path_edges (Stack.top pathstack) v;
      increase_path_count path_counts v;
      if d < max_depth then
	G.iter_succ (fun w -> if not (H.mem h w) then visit (d + 1) v w) g v;
      ignore (Stack.pop pathstack);
      H.remove h v;
    in
      (* visit 0 v; *)
      G.iter_succ (visit 1 v) g v;
      (path_edges, path_counts)
    
  (* modified BFS iterator from ocamlgraph which includes the distance 
     d(v0, v) for each visited node v \in V *)
  let bfs_dist_iterator f g v0 = 
    let h = H.create (G.nb_vertex g) in
    let q = Queue.create () in
      (* invariant: [h] contains exactly the vertices which have been pushed *)
      H.add h v0 0;
      Queue.add v0 q;
      while not (Queue.is_empty q) do
	let u = Queue.pop q in
	let d_u = H.find h u in
	let push v = 
	  if not (H.mem h v) then 
	    begin 
	      H.add h v (d_u + 1);
	      Queue.add v q;
	    end 
	in
	  f u d_u;
	  G.iter_succ push g u
      done

  (* compute the h-neighbourhood of a node u *)
  let neighbourhood g u h =
    let neigh = ref [] in
    let visit v d =
      if d <= h then
	neigh := v :: !neigh
    in
      bfs_dist_iterator visit g u;
      !neigh

  let sum xs = List.fold_left (+) 0 xs
  let avg xs = (float_of_int (sum xs)) /. (float_of_int (List.length xs))

  type stat = {
    avg : float;
    median : int;
    max : int;
    min : int;
  }

  type result = {
    maxflow : stat;
    pathcount : stat;
  } 

  let single_redundancy g s =
    let results = ref [] in
    let n5 = node_induced_subgraph g (neighbourhood g s 5) in
    let (path_edges, path_counts) = compute_paths 5 n5 s in
      H.iter
	(fun t pes ->
	   let edges = ES.elements pes in
	   let pathgraph = edge_induced_subgraph g edges in
	   let (_, maxflow) = FF.maxflow pathgraph s t in
	   let pathcount = H.find path_counts t in
	     results := (maxflow, pathcount) :: !results)
	path_edges;
      !results

  let single_node_statistics results =
      let maxflows, pathcounts = List.split results in
      let maxflow_median = median (Array.of_enum (List.enum maxflows)) in
      let pathcount_median = median (Array.of_enum (List.enum pathcounts)) in
      let (maxflow_max, maxflow_min) = enum_max_min (List.enum maxflows) in
      let (pathcount_max, pathcount_min) = enum_max_min (List.enum pathcounts) in
      let maxflow_avg = avg maxflows in
      let pathcount_avg = avg pathcounts in
      let maxflow_stat = 
	{ 
	  avg = maxflow_avg; 
	  median = maxflow_median;
	  max = maxflow_max;
	  min = maxflow_min;
	}
      in
      let pathcount_stat = 
	{ 
	  avg = pathcount_avg; 
	  median = pathcount_median;
	  max = pathcount_max;
	  min = pathcount_min;
	}
      in
	{ 
	  maxflow = maxflow_stat; 
	  pathcount = pathcount_stat 
	}

      let redundancy_worker g vs benchmark =
	let single v =
	  benchmark ();
	  let r = single_redundancy g v in
	    single_node_statistics r
	in
	  List.map single vs

      module Redundancy_job = struct
	include G
	type worker_result = (G.V.t * result) list
	let worker_function = redundancy_worker
	type combine_type = worker_result
	let combine_start = []
	let combine_results = List.append
	let jobname = "redundancy"
      end
end

