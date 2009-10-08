open Batteries
open Printf
open Graph
open Graph_misc
open Misc

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val fold_vertex : (V.t -> 'a -> 'a) -> t  -> 'a -> 'a
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val nb_vertex : t -> int
  val nb_edges : t -> int
  val out_degree : t -> V.t -> int
  val in_degree : t -> V.t -> int
end

module Make(G : G) = struct
  module H = Hashtbl.Make(G.V)

  (* compute eccentricity, average connected distance 
     and h-Neighbourhood during breadth-first search *)

  (* modified BFS iterator from ocamlgraph which includes the distance 
     d(v0, v) for each visited node v \in V *)
  let iter_component f g v0 = 
    let h = H.create (G.nb_vertex g) in
    let q = Queue.create () in
      (* invariant: [h] contains exactly the vertices which have been pushed *)
      H.add h v0 0;
      Queue.add v0 q;
      while not (Queue.is_empty q) do
	let u = Queue.pop q in
	let push v = 
	  if not (H.mem h v) then 
	    begin 
	      H.add h v ((H.find h u) + 1);
	      Queue.add v q;
	    end 
	in
	  f u (H.find h u);
	  G.iter_succ push g u
      done

  let single_vertex_distance_statistics g u =
    (* eccentricity = maximum distance d(u, v) encountered so far *)
    let ecc = ref 0 in
      (* summed up distance (for average) so far *)
    let accum_dist = ref 0 in
      (* number of nodes in the 2-neighbourhood and 3-neighbourhood respectively *)
    let neigh_2 = ref 0 in
    let neigh_3 = ref 0 in
    let visit v d =
      if d = 2 then 
	incr neigh_2
      else if d = 3 then 
	incr neigh_3;
      if d > !ecc then 
	ecc := d;
      accum_dist := !accum_dist + d
    in
      iter_component visit g u;
      (!ecc, !accum_dist, !neigh_2, !neigh_3)

  let distance_statistics g cnt =
    let n = G.nb_vertex g in
    let ecc_tbl = H.create n in
    let dist_accu = ref 0 in
    let neigh_2_dist = Hashtbl.create 1000 in
    let neigh_3_dist = Hashtbl.create 500 in 
    let compute_vertex v =
      display_iterations cnt "compute_vertex" 100;
      let (ecc, dist, neigh_2_size, neigh_3_size) = 
	single_vertex_distance_statistics g v
      in
	H.add ecc_tbl v ecc;
	dist_accu := !dist_accu + dist;
	let htbl_incr tbl key =
	  try
	    Hashtbl.replace tbl key ((Hashtbl.find tbl key) + 1)
	  with Not_found -> Hashtbl.add tbl key 1
	in
	  htbl_incr neigh_2_dist neigh_2_size;
	  htbl_incr neigh_3_dist neigh_3_size
    in
      G.iter_vertex compute_vertex g;
      let nr_pairs = float_of_int ((n * (n-1)) / 2) in
      let connected_avg_dist = (float_of_int !dist_accu) /. nr_pairs in
	(ecc_tbl, connected_avg_dist, neigh_2_dist, neigh_3_dist)

  let degree_distribution g =
    let (indeg_map, outdeg_map, total_in) = G.fold_vertex
      (fun v (in_map, out_map, tin) -> 
	 let outdeg = G.out_degree g v in
	 let indeg = G.in_degree g v in
	 let out_map = intmap_increase_or_add out_map outdeg in
	 let in_map = intmap_increase_or_add in_map indeg in
	   (in_map, out_map, tin + indeg)
      )
      g
      (Map.IntMap.empty, Map.IntMap.empty, 0)
    in
    let nr_vertex = float_of_int (G.nb_vertex g) in
    let avg_in = (float_of_int total_in) /. nr_vertex in
      (indeg_map, outdeg_map, avg_in)

  (* statistics which can be computed regardless of the graph size *)
  let basic_network_statistics graph graph_name =
    let nr_vertex = G.nb_vertex graph in
    let nr_edges = G.nb_edges graph in
    let (indeg_map, outdeg_map, avg_indeg) = degree_distribution graph in
      print_endline ("basic_network_statistics " ^ graph_name);
      printf "vertices %d edges %d\n" nr_vertex nr_edges;
      printf "average indegree = average outdegree %f\n" avg_indeg;
      write_distribution_to_file (Map.IntMap.enum indeg_map) (graph_name ^ "_indeg.plot");
      write_distribution_to_file (Map.IntMap.enum outdeg_map) (graph_name ^ "_outdeg.plot");
      print_endline ""

  (* adds computationally expensive statistics which can't be computed on 
     the whole graph *)
  let complete_statistics graph graph_name cnt =
    basic_network_statistics graph graph_name;
    let (ecc_tbl, avg_distance, neigh_2_dist, neigh_3_dist) =
      distance_statistics graph cnt in
    let n = G.nb_vertex graph in
    let (sum_ecc, max_ecc, min_ecc) = Enum.fold 
      (fun (sum, max, min) ecc -> 
	 let larger a b = if a > b then a else b in
	 let smaller a b = if a < b then a else b in
	   (sum + ecc, larger max ecc, smaller min ecc)
      )
      (0, 0, Int.max_num)
      (H.values ecc_tbl) 
    in
    let avg_ecc = (float_of_int sum_ecc) /. (float_of_int n) in
    let ((max_2, _), (min_2, _)) = distribution_max_min (Hashtbl.enum neigh_2_dist) in
    let ((max_3, _), (min_3, _)) = distribution_max_min (Hashtbl.enum neigh_3_dist) in
      print_endline ("complete_statistics " ^ graph_name);
      printf "eccentricity average %f max %d min %d\n" avg_ecc max_ecc min_ecc;
      printf "2-neighbourhood max %d min %d\n" max_2 min_2;
      printf "3-neighbourhood max %d min %d\n" max_3 min_3;
      write_distribution_to_file (Hashtbl.enum neigh_2_dist) (graph_name ^ "_neigh_2_dist.plot");
      write_distribution_to_file (Hashtbl.enum neigh_3_dist) (graph_name ^ "_neigh_3_dist.plot");
      print_endline ""
end
