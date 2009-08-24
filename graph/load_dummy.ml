open Batteries
open SExpr
open Printf

open Ekey

let () =
  let sexp_list = load_rev_sexps "sks_dump.sexp" in
  let ekey_list = List.map ekey_of_sexp sexp_list in
    printf "loaded %d ekeys from dump file\n" (List.length ekey_list)
;;
