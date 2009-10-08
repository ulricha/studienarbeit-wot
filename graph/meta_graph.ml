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
  type t = int
  let compare = fun w1 w2 -> compare w1 w2
  let default = 0
end

module M = Imperative.Digraph.ConcreteLabeled(MV)(ME)

let () = print_endline "foo"
