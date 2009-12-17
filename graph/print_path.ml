open Batteries
open Graph
open Wot_graph

module W = struct
  type label = G.E.label
  type t = int
  let weight l = 1
  let compare = Pervasives.compare
  let add = (+)
  let zero = 0
end

module D = Path.Dijkstra(G)(W)

let _ = 
  if Array.length Sys.argv <> 4 then (
    print_endline "usage: print_path edge_file source target";
    exit 1)

let main () =
  let source = Sys.argv.(2) in
  let target = Sys.argv.(3) in
  let g = load_graph_from_file Sys.argv.(1) in
    Printf.printf "nb_vertex %d nb_edge %d\n" (G.nb_vertex g) (G.nb_edges g);
    Printf.printf "%s -> %s\n" source target;
    try
      let (path, length) = D.shortest_path g source target in
	Printf.printf "found shortest path of length %d\n" length;
	let print_edge e = 
	  let t = (G.E.dst e) in
	    Printf.printf "%s %s\n" t (String.sub t 8 8) 
	in
	  List.iter print_edge path
    with Not_found -> print_endline "no path";
      Printf.printf "\n%s -> %s\n" target source;
      try
	let (path, length) = D.shortest_path g target source in
	  Printf.printf "found shortest path of length %d\n" length;
	  let print_edge e = 
	    let t = (G.E.dst e) in
	      Printf.printf "%s %s\n" t (String.sub t 8 8) 
	  in
	    List.iter print_edge path
      with Not_found -> print_endline "no path"


let _ =
  try main () with e -> prerr_endline (Printexc.to_string e)
    
