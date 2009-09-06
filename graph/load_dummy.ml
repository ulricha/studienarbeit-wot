TYPE_CONV_PATH "Load_dummy"

open Batteries
open SExpr
open Printf
open Graph

open Ekey

let hash_keyid keyid =
  let x = 0 in
  let x = x lor (int_of_char keyid.[3] lsl 24) in
  let x = x lor (int_of_char keyid.[2] lsl 16) in
  let x = x lor (int_of_char keyid.[1] lsl 8) in
  let x = x lor (int_of_char keyid.[0]) in
    x

module V = struct
  type t = epki with sexp
  let compare = (fun k1 k2 -> compare k1.key_keyid k2.key_keyid)
  let hash = (fun k1 -> hash_keyid k1.key_keyid)
  let equal = (fun k1 k2 -> k1.key_keyid = k2.key_keyid)
end

module E = struct
  type t = string * string with sexp
  let compare = (fun (v1, v2) (v3, v4) ->
		   compare (v1 ^ v2) (v3 ^ v4))
end

type vertex = V.t with sexp
type edge_index_with_label = ((int * int) * esiginfo) with sexp
type vertex_list = V.t list with sexp
type edge_list = edge_index_with_label list with sexp
type sexp_graph = vertex_list * edge_list with sexp
		   
module G = Imperative.Digraph.ConcreteBidirectional(V)

module Edge_siginfo_map = Map.Make(E)

module Keyid_key_map = Map.Make(String)

let time_evaluation f =
  let t1 = Unix.time () in
  let ret = f () in
  let t2 = Unix.time() in
    printf "%d sec\n" (int_of_float (t2 -. t1));
    ret

let search_keyid array keyid =
  let cmp keyid epki = compare keyid epki.key_keyid in
  let rec search low high =
    if high < low then
      raise Not_found
    else
      let mid = (low + (high-low)) / 2 in
	match cmp keyid (Array.get array mid)  with
	  | x when x > 0 -> search low (mid-1)
	  | x when x < 0 -> search (mid+1) high
	  | _ -> mid
  in
    search 0 (Array.length array)

let display_iterations counter operation =
  incr counter;
  if (!counter mod 10000) = 0 then
    printf "%s: %d iterations\n" operation !counter

let ekey_list_to_sexp_graph l =
  let cnt = ref 0 in
  let vlist_unsorted = List.fold_left 
    (fun vl ek -> ek.pki :: vl)
    []
    l
  in
  let varray_sorted = Array.of_list (List.sort ~cmp:V.compare vlist_unsorted) in
  let search_varray = search_keyid varray_sorted in
  let single_key_signatures k =
    let destination = k.pki.key_keyid in
      List.fold_left 
	(fun siglist (source, info) -> 
	   let source_index = search_varray source in
	   let destination_index = search_varray destination in
	     ((source_index, destination_index), info) :: siglist)
	[]
	k.signatures
  in
  let edge_list = List.fold_left (fun el k -> (single_key_signatures k) @ el) [] l in
    (varray_sorted, edge_list)

(*
let sexp_graph_to_graph g =
  let (vertices, edges) = g in
    List.iter 
*)

let dump_sexp_graph_to_file vertex_filename edge_filename g =
  let (vertex_list, edge_list) = g in
  let v_channel = open_out vertex_filename in
  let e_channel = open_out edge_filename in
    Array.iter
      (fun v ->
	 let s = sexp_of_vertex v in
	   output_mach v_channel s;	
	   output_char v_channel '\n'
      )
      vertex_list
    ;
    List.iter
      (fun e ->
	 let s = sexp_of_edge_index_with_label e in
	   output_mach e_channel s;
	   output_char e_channel '\n'
      )
      edge_list

let load_sexp_graph_from_files vertex_filename edge_filename g =
  let vertices = List.map vertex_of_sexp (load_sexps vertex_filename) in
  let edges = List.map edge_index_with_label_of_sexp (load_rev_sexps edge_filename) in
    (vertices, edges)

let create_graph sexp_graph =
  let (vertices, edges) = sexp_graph in
  let g = G.create ~size:300000 () in
    begin
      List.iter (fun v -> G.add_vertex g v) vertices;
    end


let () =
  print_endline "started";
  let filename = Sys.argv.(1) in
  let sexp_list = load_rev_sexps filename in
  let ekey_list = List.map ekey_of_sexp sexp_list in
    begin
      print_endline "loaded";
      Gc.full_major ();
      printf "loaded %d ekeys from dump file\n" (List.length ekey_list)
    end
    ;
    let (vl, el) as g = time_evaluation (fun () -> ekey_list_to_sexp_graph ekey_list) in
      time_evaluation (fun () -> dump_sexp_graph_to_file "vertex_list.sexp" "edge_list.sexp" g)
(*
    let f () = 
      let tbl = Keyid_key_map.empty in
	List.fold_left 
	  (fun tbl ekey -> Keyid_key_map.add ekey.pki.key_keyid ekey tbl)
	  tbl
	  ekey_list
    in
      ignore (time_evaluation f)
*)
	  
 
