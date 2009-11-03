open Batteries
open Graph
open Wot_graph
open Misc
open Graph_misc
open Printf

module C = Component_helpers.Make(G)

(* metavertex *)
module MV = struct
  type t = int * int ref
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
  let vertex_name v = let (name, n) = v in sprintf "%d.%d" name !n
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
    let h = VH.create 1000 in
      List.iter	
	(fun c -> 
	   List.iter
	     (fun v -> VH.add h v (component_to_id c))
	     c) 
	component_list;
      (fun v -> VH.find h v)

  (* *) 
  let construct_metagraph_nodes component_list component_to_id = 
    let i = ref 0 in
    let h = Hashtbl.create (List.length component_list) in
    (* create a metagraph which has nodes on the order of the number 
       of components minus the number of components of size 1 and 2
       which are contracted to one node *)
    let mg = MG.create ~size:((List.length component_list) - 217000) () in
      let add_metavertex component =
	let component_id = component_to_id component in
	let n = List.length component in
	  incr i;
	  let mv = (!i, ref n) in
	    MG.add_vertex mg mv;
	    Hashtbl.add h component_id mv
      in
	List.iter add_metavertex component_list;
	(mg, h)

  let construct_metagraph_edges metagraph scc_mv_tbl original_graph scc_list vertex_to_component_id =
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
      G.iter_vertex (fun v -> G.iter_succ_e handle_edge original_graph v) original_graph

  let metagraph graph component_list =
    let component_to_id = build_component_identifier component_list in
    let vertex_to_component_id = assoc_vertex_component component_list component_to_id in
    let (metagraph, cid_mv_tbl) = construct_metagraph_nodes component_list component_to_id in
      construct_metagraph_edges metagraph cid_mv_tbl graph component_list vertex_to_component_id;
      metagraph
end

module M = Make(G)
module Statistics = Network_statistics.Make(MG)

exception Invalid_arg

(* assume component list sorted reverse by size *)
let remove_small_components g component_list =
  let rec loop cl small_components =
    match cl with
      | c :: tl when List.length c <= 10 ->
	  loop tl (c :: small_components)
      | c :: tl ->
	  (small_components, tl)
      | [] ->
	  raise Invalid_arg
  in
  let (small_components, large_components) = loop component_list [] in
  let remove_nodes component = List.iter (fun v ->  G.remove_vertex g v) component in
    List.iter remove_nodes small_components;
    large_components

let () =
  if (Array.length Sys.argv) <> 3 then
    begin
      print_endline "usage: basic_properties vertex.mar edges.mar";
      exit (-1)
    end
  else
    begin
      print_endline "compute basic properties of wot graph";
      let vertex_fname = Sys.argv.(1) in
      let edge_fname = Sys.argv.(2) in
      let (g, scc_list) = time_eval (fun () -> Component_helpers.load_scc_list vertex_fname edge_fname) "scc_list" in
      let large_components = remove_small_components g scc_list in
      let metagraph = (time_eval (fun () -> M.metagraph g scc_list) "metagraph") in
      let oc = Pervasives.open_out "metagraph.dot" in 
	ignore large_components;
	Dot.output_graph oc metagraph;
	Pervasives.close_out oc
    end
