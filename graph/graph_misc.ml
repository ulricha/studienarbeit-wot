open Batteries
open Printf
open Graph
open Misc

let list_list_sort_reverse l =
  let c l1 l2 = compare (List.length l1) (List.length l2) in
  let compare_length = compare_reverse c in
    List.sort ~cmp:compare_length l

let write_distribution_to_file format enum fname = 
  let f out =
    Enum.iter (fun (k, v) -> fprintf out format k v) enum
  in
    File.with_file_out fname f

let write_float_values_to_file enum fname =
  let write output =
    Enum.iter (fun v -> fprintf output "%f\n" v) enum
  in
    File.with_file_out fname write

let write_int_values_to_file enum fname =
  let write output =
    Enum.iter (fun v -> fprintf output "%d\n" v) enum
  in
    File.with_file_out fname write

let intmap_add_or_create increment map key =
  try 
    Map.IntMap.add key ((Map.IntMap.find key map) + increment) map
  with Not_found -> Map.IntMap.add key increment map

module Int32Map = Map.Make(Int32)

let int32map_add_or_create increment map key =
  try 
    let old = Int32Map.find key map in
      Int32Map.add key (old + increment) map
  with Not_found -> Int32Map.add key increment map

let stringmap_add_or_create increment map key =
  try
    let old = Map.StringMap.find key map in
      Map.StringMap.add key (old + increment) map
  with Not_found -> Map.StringMap.add key increment map

let distribution_max_min enum =
  let max_min (max_p, min_p) p =
    let larger_pair p1 p2 =
      let (k1, v1) = p1 in
      let (k2, v2) = p2 in
	if v1 > v2 then p1 else p2
    in
    let smaller_pair p1 p2 =
      let (k1, v1) = p1 in
      let (k2, v2) = p2 in
	if v1 < v2 then p1 else p2
    in
      (larger_pair max_p p, smaller_pair min_p p)
  in
    Enum.fold max_min ((0, 0), (0, Int.max_num)) enum

let enum_max_min enum =
  let max_min (maximum, minimum) x =
      (max maximum x, min minimum x)
  in
    Enum.fold max_min (0, Int.max_num) enum

let median a = 
  Array.sort compare a;
  Array.get a ((Array.length a) / 2)

let values_to_distribution enum =
  Enum.fold (fun d value -> intmap_add_or_create 1 d value) Map.IntMap.empty enum

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
end

let apply_lines input f = 
  try
    while true do
      let line = IO.read_line input in
	f line
    done;
  with IO.No_more_input -> ()

module Graph_helpers(G : G) = struct

  exception Abort
  let take_some_vertex g =
    let v = ref None in
    let f u = 
      match !v with
	| Some v -> raise Abort
	| None -> v := Some u
    in
      try 
	G.iter_vertex f g;
	None
      with Abort -> !v

end

