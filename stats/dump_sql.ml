open Batteries
open Ekey
open Misc

exception Malformed_code
(* TODO: rewrite function to replace invalid UTF8 sequences
let validate s =
  let rec trail c i a =
    if c = 0 then a else
    if i >= String.length s then raise Malformed_code else
    let n = Char.code (String.unsafe_get s i) in
    if n < 0x80 || n >= 0xc0 then raise Malformed_code else
    trail (c - 1) (i + 1) (a lsl 6 lor (n - 0x80)) in
  let rec main i =
    if i >= String.length s then () else
    let n = Char.code (String.unsafe_get s i) in
    if n < 0x80 then main (i + 1) else
    if n < 0xc2 then raise Malformed_code else
    if n <= 0xdf then 
      if trail 1 (i + 1) (n - 0xc0) < 0x80 then raise Malformed_code else 
      main (i + 2)
    else if n <= 0xef then 
      if trail 2 (i + 1) (n - 0xe0) < 0x800 then raise Malformed_code else 
      main (i + 3)
    else if n <= 0xf7 then 
      if trail 3 (i + 1) (n - 0xf0) < 0x10000 then raise Malformed_code else
      main (i + 4)
    else if n <= 0xfb then 
      if trail 4 (i + 1) (n - 0xf8) < 0x200000 then raise Malformed_code else
      main (i + 5)
    else if n <= 0xfd then 
      let n = trail 5 (i + 1) (n - 0xfc) in
      if n lsr 16 < 0x400 then raise Malformed_code else
      main (i + 6)
    else raise Malformed_code in
  main 0
*)

let load_ekey_list fname = 
  List.map ekey_of_sexp (SExpr.load_sexps fname)

let email_regex = Str.regexp ".*<.*>.*"

let validate_utf8 s =
  let repair s = 
    let e = Str.search email_regex s in
      match Enum.get e with
	| Some (_, _, email) -> email
	| None -> "unrepairable string"
  in
    try
      UTF8.validate s; s
    with UTF8.Malformed_code -> repair s

let insert_epki dbh epki =
  let keyid = keyid_to_string epki.key_keyid in
  let puid = validate_utf8 epki.key_puid in
  let ctime = epki.key_ctime in
  let exptime = epki.key_exptime in
  let key_alg = Int32.of_int epki.key_alg in
  let key_len = Int32.of_int epki.key_len in
    PGSQL(dbh) "INSERT into keys (keyid, puid, ctime, exptime, alg, keylen)
    values ($keyid, $puid, $ctime, $?exptime, $key_alg, $key_len)"

let insert_uid_list dbh epki =
  let keyid = keyid_to_string epki.key_keyid in
  List.iter
    (fun uid -> 
       let uid = validate_utf8 uid in
	 PGSQL(dbh) "insert into uids (keyid, uid) values ($keyid, $uid)")
    epki.key_all_uids

let insert_sig_list dbh signee esig_list =
  let insert_esig esig =
    let (signer, info) = esig in
    let signee = keyid_to_string signee in
    let signer = keyid_to_string signer in
    let level = Int32.of_int info.sig_level in
    let exptime = info.sig_exptime in
    let ctime = info.sig_ctime in
    let hash_alg = Int32.of_int info.sig_hash_alg in
    let pk_alg = Int32.of_int info.sig_pk_alg in
      PGSQL(dbh) "insert into sigs (signer, signee, level, exptime, ctime, hash_alg, pk_alg)
                  values ($signer, $signee, $level, $?exptime, $?ctime,
                  $hash_alg, $pk_alg)"
  in
    List.iter insert_esig esig_list

let filter_duplicates ekeys =
  let rec filter l unique s =
    match l with
      | ekey :: tl ->
	  let keyid = keyid_to_string ekey.pki.key_keyid in
	    if Set.StringSet.mem keyid s then (
	      Printf.printf "dupe %s %s\n" keyid ekey.pki.key_puid;
	      filter tl unique s
	    ) else (
	      filter tl (ekey :: unique) (Set.StringSet.add keyid s)
	    )
      | [] -> unique
  in
    filter ekeys [] Set.StringSet.empty

let insert_ekeys dbh ekey_list =
  let bench = time_iterations "insert_epki" 10000 in
    List.iter (fun ekey -> bench (); insert_epki dbh ekey.pki) ekey_list;
    let bench = time_iterations "insert_uid_list" 10000 in
      List.iter (fun ekey -> bench ();insert_uid_list dbh ekey.pki) ekey_list;
      let bench = time_iterations "insert_sig_list" 10000 in
	List.iter 
	  (fun ekey -> 
	     let signee = ekey.pki.key_keyid in
	       bench ();
	       insert_sig_list dbh signee ekey.signatures)
	  ekey_list
  
let get_all_keys dbh =
  PGSQL(dbh) "select * from keys"

let _ =
  let dbh = PGOCaml.connect ~database:"wot" () in
    print_endline "connected to db";
    let ekeys = load_ekey_list Sys.argv.(1) in
    let ekeys_unique = filter_duplicates ekeys in
      print_endline "loaded ekeys";
      Printf.printf "ekeys %d unique %d\n" (List.length ekeys) (List.length ekeys_unique);
      insert_ekeys dbh ekeys_unique
