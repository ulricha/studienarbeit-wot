open Batteries
open Ekey
open Misc

exception Malformed_code
  (* TODO: rewrite function to replace invalid UTF8 sequences *)
let validate s =
  let replace s start c =
    if start + c - 1 >= String.length s then raise Malformed_code;
    for i = start to (start + c - 1) do
      s.[i] <- '?'
    done
  in
  let rec trail c i a =
    if c = 0 then a else
      if i >= String.length s then raise Malformed_code else
	let n = Char.code (String.unsafe_get s i) in
	  if n < 0x80 || n >= 0xc0 then 0 else
	    trail (c - 1) (i + 1) (a lsl 6 lor (n - 0x80)) in
  let rec main i =
    if i >= String.length s then
      () 
    else
      let n = Char.code (String.unsafe_get s i) in
	if n < 0x80 then 
	  main (i + 1) 
	else if n < 0xc2 then 
	  begin
	    replace s i 1; 
	    main (i + 1) 
	  end
	else if n <= 0xdf then 
	  begin
	    if trail 1 (i + 1) (n - 0xc0) < 0x80 then 
	      replace s i 2; 
	    main (i + 2)
	  end
	else if n <= 0xef then 
	  begin
	    if trail 2 (i + 1) (n - 0xe0) < 0x800 then 
	      replace s i 3; 
	    main (i + 3)
	  end
	else if n <= 0xf7 then 
	  begin
	    if trail 3 (i + 1) (n - 0xf0) < 0x10000 then 
	      replace s i 4; 
	    main (i + 4)
	  end
	else if n <= 0xfb then 
	  begin
	    if trail 4 (i + 1) (n - 0xf8) < 0x200000 then 
	      replace s i 5; 
	    main (i + 5)
	  end
	else if n <= 0xfd then 
	  let n = trail 5 (i + 1) (n - 0xfc) in
	    begin
	      if n lsr 16 < 0x400 then 
		replace s i 6;
	      main (i + 6)
	    end
	else raise Malformed_code 
  in
    try
      main 0
    with Invalid_argument x as e -> print_endline s; raise e

let load_ekey_list fname = 
  List.map ekey_of_sexp (SExpr.load_sexps fname)

let replace_chars s =
  for i = 0 to ((String.length s) - 1) do
    match s.[i] with
      | '\x00' .. '\x1f' -> s.[i] <- '?'
      | _ -> ()
  done

let regex_mail = Str.regexp "<.*>"

let extract_email s =
  let e = Str.search regex_mail s in
    match Enum.get e with
      | Some (_, _, email) -> email
      | None -> "unrepairable"

let validate_string o =
  let s = o in
    try
      validate s;
      replace_chars s;
      if s <> o then
	Printf.printf "repair %s %s\n" o s;
      s
    with Malformed_code ->
      let email = extract_email s in
	try
	  validate email;
	  replace_chars s;
	  if s <> o then
	    Printf.printf "repair %s %s\n" o email;
	  email
	with Malformed_code ->
	  Printf.printf "repair %s not possible\n" o;
	  "unrepairable"

let insert_epki dbh epki =
  let keyid = keyid_to_string epki.key_keyid in
  let puid = validate_string epki.key_puid in
  let ctime = epki.key_ctime in
  let exptime = epki.key_exptime in
  let key_alg = Int32.of_int epki.key_alg in
  let key_len = Int32.of_int epki.key_len in
    PGSQL(dbh) "INSERT into keys (keyid, puid, ctime, exptime, alg, keylen)
    values ($keyid, $puid, $ctime, $?exptime, $key_alg, $key_len)"

let insert_uid_list dbh epki =
  let keyid = keyid_to_string epki.key_keyid in
  let validated = List.map validate_string epki.key_all_uids in
  let unique = List.sort_unique compare validated in
  List.iter
    (fun uid -> 
	 PGSQL(dbh) "insert into uids (keyid, uid) values ($keyid, $uid)")
    unique

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

(*
let filter_missing_signers ekeys =
  let set = 
    List.fold_left 
      (fun s ekey -> Set.StringSet.add ekey.pki.key_keyid) 
      Set.StringSet.empty
      ekeys
  in
*)    

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
