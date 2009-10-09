open Batteries
open Graph

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

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val nb_vertex : t -> int
end

module Make(G : G) = struct
  module VH = Hashtbl.Make(G.V)

  (* returns a function - : G.V.t -> G.t which returns the component/graph 
     to which the vertex belongs *)
  let assoc_vertex_component component_list =
    let h = VH.create 1000 in
      List.iter	(fun g -> G.iter_vertex (fun v -> VH.add h v g) g) component_list;
      (fun v -> VH.find h v)

  let build_component_identifier component_list =
    let n = List.length component_list in
    let ci = Hashtbl.create n in
    let ic = Hashtbl.create n in
    let rec loop i l =
      match l with
	| component :: tl ->
	    Hashtbl.add ic i component;
	    Hashtbl.add ci component i;
	    loop (i + 1) tl
	| [] ->
	    ()
    in
      loop 1 component_list;
      (ic, ci)

  (* *) 
  let construct_metagraph_nodes component_list = 
    let mv_1 = (1, ref 0) in
    let mv_2 = (2, ref 0) in
    let i = ref 3 in
    let h = Hashtbl.create (List.length component_list) in
    (* create a metagraph which has nodes on the order of the number 
       of components minus the number of components of size 1 and 2
       which are contracted to one node *)
    let mg = MG.create ~size:((List.length component_list) - 217000) () in
      MG.add_vertex mg mv_1;
      MG.add_vertex mg mv_2;
      let add_metavertex component =
	let n = List.length component in
	  if n = 1 then
	    begin
	      incr (snd mv_1);
	      Hashtbl.add h component mv_1
	    end
	  else if n = 2 then
	    begin
	      incr (snd mv_2);
	      Hashtbl.add h component mv_2
	    end
	  else
	    begin
	      incr i;
	      let mv = (!i, ref n) in
		MG.add_vertex mg mv;
		Hashtbl.add h component mv
	    end
      in
	List.iter add_metavertex component_list;
	(mg, h)

  let construct_metagraph_edges metagraph scc_mv_tbl scc_list =
    ignore metagraph;
    ignore scc_mv_tbl;
    ignore scc_list

end

let () = print_endline "foo"
