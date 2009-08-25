open Batteries
open SExpr
open Printf

open Ekey

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
    let compare_ekey ek1 ek2 =
      compare ek1.key_keyid ek2.key_keyid
    in
    let a = List.sort ?cmp:(Some compare_ekey) ekey_list in
      printf "list length %d \n" (List.length a)
;;
