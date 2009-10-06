(* compute eccentricity, average connected distance 
   and h-Neighbourhood during breadth-first search *)

open Graph

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val fold_vertex : (V.t -> 'a -> 'a) -> t  -> 'a -> 'a
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val nb_vertex : t -> int
end

module Bfs_statistics(G : G) = struct
  module H = Hashtbl.Make(G.V)

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
      (* number of nodes in the 1-neighbourhood, 2-neighbourhood and 
	 3-neighbourhood respectively *)
    let neigh_1 = ref 0 in
    let neigh_2 = ref 0 in
    let neigh_3 = ref 0 in
    let visit v d =
      if d = 1 then 
	incr neigh_1
      else if d = 2 then 
	incr neigh_2
      else if d = 3 then 
	incr neigh_3;
      if d > !ecc then 
	ecc := d;
      accum_dist := !accum_dist + d
    in
      iter_component visit g u;
      (!ecc, !accum_dist, !neigh_1, !neigh_2, !neigh_3)

  let distance_statistics g =
    let n = G.nb_vertex g in
    let ecc_tbl = H.create n in
    let dist_accu = ref 0 in
    let neigh_1_dist = Hashtbl.create 1400 in
    let neigh_2_dist = Hashtbl.create 1000 in
    let neigh_3_dist = Hashtbl.create 500 in
    let compute_vertex v =
      let (ecc, dist, neigh_1_size, neigh_2_size, neigh_3_size) = 
	single_vertex_distance_statistics g v
      in
	H.add ecc_tbl v ecc;
	dist_accu := !dist_accu + dist;
	let htbl_incr tbl key =
	  try
	    Hashtbl.replace tbl key ((Hashtbl.find tbl key) + 1)
	  with Not_found -> Hashtbl.add tbl key 1
	in
	  htbl_incr neigh_1_dist neigh_1_size;
	  htbl_incr neigh_2_dist neigh_2_size;
	  htbl_incr neigh_3_dist neigh_3_size
    in
      G.iter_vertex compute_vertex g;
      let nr_pairs = float_of_int ((n * (n-1)) / 2) in
      let connected_avg_dist = (float_of_int !dist_accu) /. nr_pairs in
	(ecc_tbl, connected_avg_dist, neigh_1_dist, neigh_2_dist, neigh_3_dist)
end
