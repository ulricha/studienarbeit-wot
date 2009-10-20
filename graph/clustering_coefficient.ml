open Batteries
open Graph
open Graph_misc
open Misc
open Printf

module type G = sig
  type t
  module V : Sig.COMPARABLE
  module E : Sig.ORDERED_TYPE
  val iter_vertex : (V.t -> unit) -> t -> unit
end

module Make(G : G) = struct
  module M = Map.Make(G)

  let neighbourhood g v =
    let rec test_neighbour w neigbour_list v =
      if G.mem_edge w v then
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
      if G.mem_edge a b then
	incr triangles
    in
      apply_all_pairs neighbour_list neighbour_list f compare;
      (float_of_int !triangle_counter) /. divisor

  let clustering_coefficient_all_vertices g =
    G.fold_vertex 
      (fun v m -> 
	 let c = clustering_coefficient g v in
	   M.add v c m)
      g
      M.empty

  let clustering_coefficient_vertex_subset g vlist bench =
    List.fold_left
      (fun v alist ->
	 bench ();
	 let c = clustering_coefficient g v in
	   (v, c) :: alist)
      vlist

  let clustering_coefficient_graph map =
    let (n, sum) = M.fold (fun k v (n, sum) -> (n+1, sum+v)) map (0, 0) in
      (float_of_int sum) /. (float_of_int n)

  let combine_clustering_coefficient_results map alist =
    List.fold_left (fun m (k, v) -> M.add k v m) M.empty alist

  module Clustering_coefficient_job = struct
    type worker_result = (V.t * float) list
    let worker_function = clustering_coefficient_vertex_subset
    type combine_type = float M.t
    let combine_start = M.empty
    let combine_results = combine_clustering_coefficient_results
    let jobname = "clustering_coefficient"
  end
end
