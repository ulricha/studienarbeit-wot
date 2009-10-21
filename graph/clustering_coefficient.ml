open Batteries
open Graph
open Graph_misc
open Misc
open Printf

module Make(G : Sig.G) = struct
  module M = Map.Make(G.V)

  let neighbourhood g v =
    let test_neighbour w neighbour_list =
      if G.mem_edge g w v then
	(w :: neighbour_list)
      else
	neighbour_list
    in
      G.fold_succ test_neighbour g v []

  let clustering_coefficient g v =
    let neighbour_list = neighbourhood g v in
    let degree = List.length neighbour_list in
    let divisor = float_of_int (degree * (degree-1)) in
    let triangle_counter = ref 0 in
    let test_edge a b =
      if G.mem_edge g a b then
	incr triangle_counter
    in
      apply_all_pairs neighbour_list neighbour_list test_edge compare;
      if divisor = 0. then 
	0.
      else
	(float_of_int !triangle_counter) /. divisor
	  
  let clustering_coefficient_all_vertices g bench =
    G.fold_vertex 
      (fun v m -> 
	 bench ();
	 let cc = clustering_coefficient g v in
	   M.add v cc m)
      g
      M.empty

  let clustering_coefficient_vertex_subset g vlist bench =
    List.fold_left
      (fun alist v ->
	 bench ();
	 let cc = clustering_coefficient g v in
	   (v, cc) :: alist)
      []
      vlist

  let clustering_coefficient_graph map =
    let (n, sum) = M.fold (fun k v (n, sum) -> (n+1, sum+v)) map (0, 0) in
      (float_of_int sum) /. (float_of_int n)

  let combine_clustering_coefficient_results map alist =
    List.fold_left (fun m (k, v) -> M.add k v m) M.empty alist

  module Clustering_coefficient_job = struct
    include G
    type worker_result = (G.V.t * float) list
    let worker_function = clustering_coefficient_vertex_subset
    type combine_type = float M.t
    let combine_start = M.empty
    let combine_results = combine_clustering_coefficient_results
    let jobname = "clustering_coefficient"
  end
end
