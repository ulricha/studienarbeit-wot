open Batteries
open Printf

let write_intmap_to_file map fname = 
  let f out =
    Map.IntMap.iter (fun k v -> fprintf out "%d %d\n" k v) map
  in
    File.with_file_out fname f

let intmap_increase_or_add map key =
  try 
    Map.IntMap.add key ((Map.IntMap.find key map) + 1) map
  with Not_found -> Map.IntMap.add key 1 map
