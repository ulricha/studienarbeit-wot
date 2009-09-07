TYPE_CONV_PATH "Load_dummy"

open Batteries
open SExpr
open Printf
open Graph

open Ekey
open Misc

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
type sig_list_per_signee = int * ((int * esiginfo) list) with sexp
type vertex_list = V.t list with sexp
type edge_list = sig_list_per_signee list with sexp
type sexp_graph = vertex_list * edge_list with sexp
		   
module G = Imperative.Digraph.ConcreteBidirectional(V)

module Edge_siginfo_map = Map.Make(E)

module Keyid_key_map = Map.Make(String)

let lookup_key_index_in_array array keyid =
  let cmp keyid epki = compare keyid epki.key_keyid in
  let rec search low high =
    if high < low then
      raise Not_found
    else
      let mid = low + ((high-low) / 2) in
	match cmp keyid (Array.get array mid)  with
	  | a when a < 0 -> search low (mid-1)
	  | b when b > 0 -> search (mid+1) high
	  | _ -> mid
  in
    search 0 ((Array.length array) - 1)

let ekey_list_to_sexp_graph ekey_list =
  print_endline "ekey_list_to_sexp_graph";
  let key_cnt = ref 0 in
  let sig_cnt = ref 0 in
  let varray = Array.of_list (List.map (fun ekey -> ekey.pki) ekey_list) in
  let lookup_key_index = lookup_key_index_in_array varray in
  let edge_list = Ref_list.empty () in
  let one_key_signatures index ekey =
    let signee_index = index in
    let signer_list = List.fold_left 
      (fun l esig ->
	 display_iterations sig_cnt "sigs";
	 let (signer_keyid, siginfo) = esig in
	 let signer_index = lookup_key_index signer_keyid in
	   (signer_index, siginfo) :: l)
      []
      ekey.signatures
    in
      (signee_index, signer_list)
  in
    List.iteri 
      (fun i ekey -> 
	 display_iterations key_cnt "keys";
	 Ref_list.push edge_list (one_key_signatures i ekey)
      )
      ekey_list
    ;
    (varray, (Ref_list.to_list edge_list))

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
	 let s = sexp_of_sig_list_per_signee e in
	   output_mach e_channel s;
	   output_char e_channel '\n'
      )
      edge_list

let load_sexp_graph_from_files vertex_filename edge_filename g =
  let vertices = List.map vertex_of_sexp (load_sexps vertex_filename) in
  let edges = List.map sig_list_per_signee_of_sexp (load_rev_sexps edge_filename) in
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
    print_endline "loaded sexps";
    let ekey_list = List.fast_sort compare_ekey (List.map ekey_of_sexp sexp_list) in
      begin
	printf "loaded %d ekeys from dump file\n" (List.length ekey_list);
	printf "%d %d\n"  (List.length (List.sort_unique compare_ekey ekey_list)) (List.length ekey_list);
	let (vl, el) as g = time_evaluation (fun () -> ekey_list_to_sexp_graph ekey_list) in
	  time_evaluation (fun () -> dump_sexp_graph_to_file "vertex_list.sexp" "edge_list.sexp" g)
      end

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
	  
 
