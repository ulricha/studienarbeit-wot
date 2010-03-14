open Batteries

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

let import_infomap_communities communities_fname =
  let add_line m l =
    if l.[0] = '#' then
      m
    else
      let cid = int_of_string (fst (String.split l ":")) in
      let keyid = String.slice ~last:(-1) (snd (String.split l "\"")) in
	add_map m cid keyid
  in
  let fold_lines input =
    Enum.fold add_line Map.IntMap.empty (IO.lines_of input)
  in
    File.with_file_in communities_fname fold_lines

let import_copra_communities communities_fname =
  let add_line i line m =
    let members = String.nsplit line " " in
      Map.IntMap.add i members m
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

let community_size_values c_map =
  List.of_enum
    (Enum.map
       (fun (_id, mem) -> List.length mem)
       (Map.IntMap.enum c_map))
    
