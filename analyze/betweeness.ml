open Batteries
open Graph
open Graph_misc
open Misc
open Printf

module Make(G : Sig.G) = struct
  module H = Hashtbl.Make(G.V)

  type vertex_info = {
    mutable d : int;
    mutable sigma : float;
    mutable pred : G.V.t RefList.t;
    mutable delta : float
  }

  let lookup_vertex_info_or_create tbl v =
    try
      H.find tbl v
    with Not_found ->
      let i = { d = (-1); sigma = 0.0; pred = RefList.empty (); delta = 0.0 } in
	H.add tbl v i;
	i

  (* update betweeness centrality values as part of the round
     function. stack is the stack accumulated during BFS traversal; s
     is the start vertex of the round and n = |V| *)
  let update_betweeness b_tbl lookup stack s n =
    while not (Stack.is_empty stack) do
      let w = Stack.pop stack in
      let w_info = lookup w in
      let compute_delta v =
	let v_info = lookup v in
	let div = v_info.sigma /. w_info.sigma in
	let t = v_info.delta +. div *. (1.0 +. w_info.delta) in
	  v_info.delta <- t
      in
	Enum.iter compute_delta (RefList.backwards w_info.pred);
	if not (w = s) then
	  let tbl_add_or_create key increment =
	    if H.mem b_tbl key then
	      let prev_val = H.find b_tbl key in
		H.replace b_tbl key (prev_val +. increment)
	    else
	      H.add b_tbl key increment
	  in
	    tbl_add_or_create w (w_info.delta)
    done

  (* compute the round function of the Betweeness Centrality algorithm: do a 
     BFS traversal beginning on s, compute the vertex dependencies and use them
     to update the betweeness values in b_tbl. *)
  let betweeness_round g s b_tbl =
    let stack = Stack.create () in
    let n = G.nb_vertex g in
    let info_tbl = H.create n in
    let lookup = lookup_vertex_info_or_create info_tbl in
    let q = Queue.create () in
    let s_info = lookup s in
      s_info.sigma <- 1.0;
      s_info.d <- 0;
      Queue.add s q;
      while not (Queue.is_empty q) do
	let v = Queue.take q in
	let v_info = lookup v in
	let push w =
	  let w_info = lookup w in
	  if w_info.d < 0 then
	    begin
	      w_info.d <- (v_info.d + 1);
	      Queue.add w q;
	    end;
	    if w_info.d = v_info.d + 1 then
	      begin
		w_info.sigma <- w_info.sigma +. v_info.sigma;
		RefList.push w_info.pred v
	      end
	in
	  Stack.push v stack;
	  G.iter_succ push g v
      done;
      update_betweeness b_tbl lookup stack s n 

  (* iteratively compute the betweeness centrality values for the graph g. *)
  let betweeness_centrality_iterative g bench =
    let n = G.nb_vertex g in
    let b_tbl = H.create n in
    let f v = 
      bench ();
      betweeness_round g v b_tbl
    in
      G.iter_vertex f g;
      b_tbl

  let betweeness_centrality_node_subset g vlist bench =
    let n = G.nb_vertex g in
    let b_tbl = H.create n in
    let f v =
      bench ();
      betweeness_round g v b_tbl
    in
      List.iter f vlist;
      List.of_enum (H.enum b_tbl)

  let combine_betweeness_results map alist =
    List.fold_left
      (fun m (k, v) ->
	 try 
	   let prev = Map.StringMap.find k m in
	     Map.StringMap.add k (v +. prev) m
	 with Not_found -> Map.StringMap.add k v m)
      map
      alist

  module Betweeness_job = struct
    include G
    type worker_result = (G.V.t * float) list
    let worker_function = betweeness_centrality_node_subset
    type combine_type = float Map.StringMap.t
    let combine_start = Map.StringMap.empty
    let combine_results = combine_betweeness_results
    let jobname = "betweeness_centrality"
  end

end
