open Batteries
open Graph

let read_index index_fname =
  let h = Hashtbl.create 45000 in
  let add l =
    let (keyid, numid) = String.split l " " in
      Hashtbl.add h (int_of_string numid) keyid
  in
    File.with_file_in index_fname (fun input -> Enum.iter add (IO.lines_of input));
    (fun numid -> Hashtbl.find h numid)

let add_map m k v =
  try
    let old = Map.IntMap.find k m in
      Map.IntMap.add k (v :: old) m
  with Not_found -> Map.IntMap.add k [v] m

(* return: cid->node, node->cid *)
let import_igraph_communities index_fname communities_fname =
  let numid_to_keyid = read_index index_fname in
  let add_line i l m =
    let cid = int_of_string l in
    let keyid = numid_to_keyid i in
      add_map m cid keyid
  in
  let fold_lines input =
    Enum.foldi add_line Map.IntMap.empty (IO.lines_of input)
  in
    File.with_file_in communities_fname fold_lines

let write_community_size_values m out_fname =
  let output = File.open_out out_fname in
  let write_size cid l =
    let len = List.length l in
    let line = Printf.sprintf "%d\n" len in
      IO.nwrite output line
  in
    Map.IntMap.iter write_size m;
    IO.close_out output

let _ =
  if (Array.length Sys.argv) <> 3 then (
    print_endline "usage: investigate_communities edge-file index-file community-file";
    exit (-1))

let main () =
  print_endline "investigate_communities";
  let cid_map = import_igraph_communities Sys.argv.(2) Sys.argv.(3) in
    write_community_size_values cid_map "community_sizes.dat"

let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
