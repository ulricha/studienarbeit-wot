open Batteries
open Graph
open Wot_graph
open Misc
open Graph_misc
open Printf

(* metavertex *)
module MV = struct
  (* identifier, vertex counter *)
  type t = int * int
  let compare = fun (id1, _) (id2, _) -> compare id1 id2
  let hash = fun (id, _) -> id
  let equal = fun (id1, _) (id2, _) -> id1 = id2
end

(* metaedge *)
module ME = struct
  type t = int ref
  let compare = fun e1 e2 -> compare !e1 !e2
  let default = ref 0
end

module MG = Imperative.Digraph.ConcreteLabeled(MV)(ME)

module Display = struct
  include MG
  let vertex_name v = let (name, n) = v in sprintf "%d.%d" name n
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Dot = Graphviz.Dot(Display)

module Make(G : Sig.G) = struct
  module VH = Hashtbl.Make(G.V)
    
  (* associates each component (= vertex list) with a integer id. returns
     a function which maps component to id *)
  let build_component_identifier component_list =
    print_endline "build_component_identifier";
    let n = List.length component_list in
    let ci = Hashtbl.create n in
    let rec loop i l =
      match l with
	| component :: tl ->
	    Hashtbl.add ci component i;
	    loop (i + 1) tl
	| [] ->
	    ()
    in
      loop 1 component_list;
      fun c -> Hashtbl.find ci c

  (* returns a function - : G.V.t -> G.t which returns the component/graph 
     to which the vertex belongs *)
  let assoc_vertex_component component_list component_to_id =
    print_endline "assoc_vertex_component";
    let h = VH.create 1000 in
      List.iter	
	(fun c -> 
	   let ci = component_to_id c in
	     List.iter (fun v -> VH.add h v ci) c)
	component_list;
      (fun v -> VH.find h v)

  (* *) 
  let construct_vertices component_list component_to_id = 
    print_endline "construct_vertices";
    let h = Hashtbl.create (List.length component_list) in
    (* create a metagraph which has nodes on the order of the number 
       of components minus the number of components of size 1 and 2
       which are contracted to one node *)
    let mg = MG.create ~size:(List.length component_list) () in
    let add_metavertex component =
      let component_id = component_to_id component in
      let n = List.length component in
      let mv = (component_id, n) in
	MG.add_vertex mg mv;
	Hashtbl.add h component_id mv
    in
      List.iter add_metavertex component_list;
      (mg, h)

  let construct_edges metagraph scc_mv_tbl g scc_list vertex_to_component_id =
    print_endline "construct_edges";
    let handle_edge e =
      let (u, v) = ((G.E.src e), (G.E.dst e)) in
      let u_cid = vertex_to_component_id u in
      let v_cid = vertex_to_component_id v in
	if u_cid = v_cid then
	  ()
	else
	  let u_mv = Hashtbl.find scc_mv_tbl u_cid in
	  let v_mv = Hashtbl.find scc_mv_tbl v_cid in
	    try
	      let me = MG.find_edge metagraph u_mv v_mv in
	      let label = MG.E.label me in
		incr label
	    with Not_found -> 
	      let me = MG.E.create u_mv (ref 0) v_mv in
		MG.add_edge_e metagraph me
    in
      G.iter_vertex (fun v -> G.iter_succ_e handle_edge g v) g

  let metagraph graph component_list =
    let component_to_id = build_component_identifier component_list in
    let vertex_to_component_id = assoc_vertex_component component_list component_to_id in
    let (metagraph, cid_mv_tbl) = construct_vertices component_list component_to_id in
      construct_edges metagraph cid_mv_tbl graph component_list vertex_to_component_id;
      metagraph
end

(* assume component list sorted reverse by size *)
let remove_small_components min_size g component_list =
  let rec loop cl large_components =
    match cl with
      | c :: tl when List.length c >= min_size ->
	  loop tl (c :: large_components)
      | c :: tl ->
	  (tl, large_components)
      | [] ->
	  ([], large_components)
  in
  let (small_components, large_components) = loop component_list [] in
  let remove_nodes component = List.iter (fun v ->  G.remove_vertex g v) component in
    List.iter remove_nodes small_components;
    large_components

let filter g component_list =
  let c = ref 0 in
  let cv =
    List.fold_left
      (fun s l -> List.fold_left (fun s v -> Set.StringSet.add v s) s l)
      Set.StringSet.empty
      component_list
  in
  let remove_if_notin_c v =
    if Set.StringSet.mem v cv then
      () 
    else
      begin
	incr c;
	G.remove_vertex g v
      end
  in
    G.iter_vertex remove_if_notin_c g;
    !c

let remove_singletons mg =
  let f v =
    if MG.out_degree mg v = 0 && MG.in_degree mg v = 0 then
      MG.remove_vertex mg v
  in
    MG.iter_vertex f mg

let string_of_intpair (id, size) = sprintf "%d.%d" id size

let export_cfinder g fname =
  let write output =
    let write_edge e =
      let (u_name, u_size) = MG.E.src e in
      let (v_name, v_size) = MG.E.dst e in
      let label = MG.E.label e in
      let line = Printf.sprintf "%d %d %d\n" u_name v_name !label in
	IO.nwrite output line
    in
      MG.iter_edges_e write_edge g
  in
    File.with_file_out fname write

let export_attributes g fname =
  let write output =
    let write_vertex (name, size) =
      let line = Printf.sprintf "%d %d\n" name size in
	IO.nwrite output line
    in
      MG.iter_vertex write_vertex g
  in
    File.with_file_out fname write
