open Batteries
open SExpr
open Printf
open Graph

open Ekey

module Keyid_key_map = Map.Make(String)

let () =
  let filename = Sys.argv.(1) in
  let sexp_list = load_rev_sexps filename in
  let ekey_list = List.map ekey_of_sexp sexp_list in
    begin
      Gc.full_major ();
      printf "loaded %d ekeys from dump file\n" (List.length ekey_list);
      ignore (read_line ());
    end
    ;
    let tbl = Keyid_key_map.empty in
    let tbl = List.fold_left 
	      (fun tbl ekey -> Keyid_key_map.add ekey.key_keyid ekey tbl)
	      tbl
	      ekey_list
    in
      ignore tbl
