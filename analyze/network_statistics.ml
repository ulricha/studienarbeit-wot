open Batteries
open Printf
open Graph
open Graph_misc
open Misc

module Make(G : Sig.G) = struct
  module H = Hashtbl.Make(G.V)
  module M = Map.Make(G.V)

  let count_oneway_edges g =
    let count src dst c =
      if not (G.mem_edge g dst src) then
	c + 1
      else
	c
    in
      G.fold_edges count g 0
	
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

  let single_vertex_distance_statistics g u =
    let n = G.nb_vertex g in
    (* eccentricity = maximum distance d(u, v) encountered so far *)
    let ecc = ref 0 in
      (* summed up distance (for average) so far *)
    let accum_dist = ref 0 in
      (* number of nodes in the 2-neighbourhood and 3-neighbourhood respectively *)
    let neigh_2 = ref 0 in
    let neigh_3 = ref 0 in
    let neigh_4 = ref 0 in
    let neigh_5 = ref 0 in
    let visit v d =
      if d <= 2 then 
	incr neigh_2;
      if d <= 3 then 
	incr neigh_3;
      if d <= 4 then
	incr neigh_4;
      if d <= 5 then
	incr neigh_5;
      if d > !ecc then 
	ecc := d;
      accum_dist := !accum_dist + d
    in
      iter_component visit g u;
      let avg_dist = (float_of_int !accum_dist) /. (float_of_int (n-1)) in
      let neighbourhoods = (!neigh_2, !neigh_3, !neigh_4, !neigh_5) in
      (!ecc, !accum_dist, avg_dist, neighbourhoods)

  let distance_statistics_vertex_subset g vlist bench =
    let compute_vertex l v =
      bench ();
      let result = single_vertex_distance_statistics g v in
	(v, result) :: l
    in
      List.fold_left compute_vertex [] vlist

(*
  let distance_statistics g bench =
    let n = G.nb_vertex g in
    let ecc_tbl = H.create n in
    let avg_dist_per_node_tbl = H.create n in
    let dist_accu = ref 0 in
    let neigh_2_dist = Hashtbl.create 1000 in
    let neigh_3_dist = Hashtbl.create 500 in 
    let compute_vertex v =
      bench ();
      let (ecc, dist_sum, dist_avg, neigh_2_size, neigh_3_size) = 
	single_vertex_distance_statistics g v
      in
	H.add ecc_tbl v ecc;
	H.add avg_dist_per_node_tbl v dist_avg;
	dist_accu := !dist_accu + dist_sum;
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
	(ecc_tbl, connected_avg_dist, avg_dist_per_node_tbl, neigh_2_dist, neigh_3_dist)
*)

  let distance_statistics g bench =
    let compute_vertex v (dist_sum, ecc_map, dist_avg_map, neigh_map) =
      let (ecc, dist_sum_v, dist_avg, neighbourhoods) =
	single_vertex_distance_statistics g v
      in
      let new_ecc_map = M.add v ecc ecc_map in
      let new_dist_avg_map = M.add v dist_avg dist_avg_map in
      let neigh_map = M.add v neighbourhoods neigh_map in
	(dist_sum + dist_sum_v, new_ecc_map, new_dist_avg_map, neigh_map)
    in
    let init = (0, M.empty, M.empty, M.empty) in
      G.fold_vertex compute_vertex g init

  let project_neigh5 m =
    let p (n2, n3, n4, n5) = n5 in
      M.map p m

  let project_neigh4 m =
    let p (n2, n3, n4, n5) = n4 in
      M.map p m

  let project_neigh3 m =
    let p (n2, n3, n4, n5) = n3 in
      M.map p m

  let project_neigh2 m =
    let p (n2, n3, n4, n5) = n2 in
      M.map p m

  let analyze_and_print_results v_number graph_name results =
    let (dist_sum, ecc_map, dist_avg_map, neigh_map) = results in
    let nr_pairs = float_of_int (v_number * (v_number-1)) in
    let connected_avg_distance = (float_of_int dist_sum) /. nr_pairs in
    let (max_ecc, min_ecc) = enum_max_min (M.values ecc_map) in
    let median_ecc = median (Array.of_enum (M.values ecc_map)) in
    let ecc_dist = values_to_distribution (M.values ecc_map) in
    let n5_map = project_neigh5 neigh_map in
    let n5_dist = values_to_distribution (M.values n5_map) in
    let (max_5, min_5) = enum_max_min (M.values n5_map) in
    let n4_map = project_neigh4 neigh_map in
    let n3_map = project_neigh3 neigh_map in
    let n2_map = project_neigh2 neigh_map in
      print_endline ("complete_statistics " ^ graph_name);
      printf "eccentricity median %d max %d min %d\n" 
	median_ecc max_ecc min_ecc;
      printf "(connected) average distance %f\n" connected_avg_distance;
      printf "5-neighbourhood max %d min %d\n" max_5 min_5;
      write_distribution_to_file "%d %d\n" (Map.IntMap.enum ecc_dist) 
	(graph_name ^ "_ecc_dist.plot");
      write_distribution_to_file "%d %d\n" (Map.IntMap.enum n5_dist) 
	(graph_name ^ "_neigh_5_dist.plot");
      write_int_values_to_file (M.values n5_map) (graph_name ^ "_n5.values");
      write_int_values_to_file (M.values n4_map) (graph_name ^ "_n4.values");
      write_int_values_to_file (M.values n3_map) (graph_name ^ "_n3.values");
      write_int_values_to_file (M.values n2_map) (graph_name ^ "_n2.values");
      write_int_values_to_file (M.values ecc_map) (graph_name ^ "_ecc.values");
      write_float_values_to_file (M.values dist_avg_map) (graph_name ^ "_avg_dist.values");
      
      print_endline ""

  let degree_distribution g =
    let (indeg_map, outdeg_map, total_in) = G.fold_vertex
      (fun v (in_map, out_map, tin) -> 
	 let outdeg = G.out_degree g v in
	 let indeg = G.in_degree g v in
	 let out_map = M.add v outdeg out_map in
	 let in_map = M.add v indeg in_map in
	   (in_map, out_map, tin + indeg)
      )
      g
      (M.empty, M.empty, 0)
    in
    let nr_vertex = float_of_int (G.nb_vertex g) in
    let avg_in = (float_of_int total_in) /. nr_vertex in
      (indeg_map, outdeg_map, avg_in)

  (* statistics which can be computed regardless of the graph size *)
  let basic_network_statistics graph =
    let nr_vertex = G.nb_vertex graph in
    let nr_edges = G.nb_edges graph in
    let (indeg_map, outdeg_map, avg_indeg) = degree_distribution graph in
      (nr_vertex, nr_edges, indeg_map, outdeg_map, avg_indeg)

  (* adds computationally expensive statistics which can't be computed on 
     the whole graph *)
  let distance_network_statistics_ser graph graph_name bench =
    let results = distance_statistics graph bench in
    let n = G.nb_vertex graph in
      analyze_and_print_results n graph_name results

  let add_result_to_maps (dist_sum, ecc_map, dist_avg_map, neigh_map) (v, result) =
    let (ecc, dist_sum_v, avg_dist, neighbourhoods) = result in
    let new_ecc_map = M.add v ecc ecc_map in
    let new_dist_avg_map = M.add v avg_dist dist_avg_map in
    let neigh_map = M.add v neighbourhoods neigh_map in
      (dist_sum + dist_sum_v, new_ecc_map, new_dist_avg_map, neigh_map)

  let combine_distance_results results alist =
    List.fold_left add_result_to_maps results alist

  module Distance_statistics_job = struct
    include G
      (* ecc, dist_sum_v, avg_dist, n2_size, n3_size *)
    type worker_result = (G.V.t * (int * int * float * (int * int * int * int))) list
    let worker_function = distance_statistics_vertex_subset
    type combine_type = int * int M.t * float M.t * (int * int * int * int) M.t
    let combine_start = (0, M.empty, M.empty, M.empty)
    let combine_results = combine_distance_results
    let jobname = "distance_statistics"
  end
end

