open ExtList;;
open ExtString;;

type section = Names | Readme | Keys | Signatures | Debug | None | Version;;
type cert_level = Generic | Persona | Casual | Positive;;
type signature = { primary_sig : bool; level : cert_level; offset : Int32.t};;

exception Wotsap_parse_error of string;;

let cert_level_of_int32 code =
  match code with
    | 0l -> Generic
    | 1l -> Persona
    | 2l -> Casual
    | 3l -> Positive
    | _ -> raise (Wotsap_parse_error "Illegal cert level")

let parse_signature bin_signature =
  let offset = Int32.logand bin_signature 0x0fffffffl in
  let sigtype = Int32.shift_right_logical bin_signature 28 in
  let level = cert_level_of_int32 (Int32.logand sigtype 0x3l) in
  let primary_signed =
    let b = Int32.logand (Int32.shift_right_logical sigtype 1) 0x1l in
      if b = 1l then
	true
      else
	false
  in
    { primary_sig = primary_signed; level = level; offset = offset }

let section_of_string s =
  try
    let off = String.index s '/' in
    let cand = String.sub s 0 off in
      if cand = "names" then Names
      else if cand = "README" then Readme
      else if cand = "WOTVERSION" then Version
      else if cand = "keys" then Keys
      else if cand = "signatures" then Signatures
      else if cand = "debug" then Debug
      else None
  with Not_found -> None

let strip_trailing_space s =
  try 
    let off = String.index s ' ' in
      String.sub s 0 off
  with Not_found -> s

let parse_section_header input =
  let section_type = section_of_string (IO.really_nread input 16) in
  let mtime = Int32.of_string (strip_trailing_space (IO.really_nread input 12)) in
  let uid = int_of_string (strip_trailing_space (IO.really_nread input 6)) in
  let gid = int_of_string (strip_trailing_space (IO.really_nread input 6)) in
  let mode = int_of_string (strip_trailing_space (IO.really_nread input 8)) in
  let size = int_of_string  (strip_trailing_space (IO.really_nread input 10)) in
  let trailer = (strip_trailing_space (IO.really_nread input 2)) in
    section_type, mtime, uid, gid, mode, size, trailer

let read_names input names size =
  let s = IO.really_nread input size in
  let name_list = Str.split (Str.regexp "\n") s in 
    List.iter (fun name -> DynArray.add names name) name_list

let read_keys input keys size =
  let rec loop remaining =
    if remaining = 0 then
      ()
    else
      let keyid = IO.BigEndian.read_real_i32 input in
	DynArray.add keys keyid;
	loop (remaining - 4);
  in
    loop size

let read_signatures inp signatures num =
  let rec loop remaining =
    if remaining = 0 then
      ()
    else
      let num_i32 = IO.BigEndian.read_real_i32 inp in
      let number_of_sigs = Int32.to_int num_i32 in
      let rec loop_sigs count sig_list =
        if count = 0 then
          DynArray.add signatures sig_list
        else
          let binsig = IO.BigEndian.read_real_i32 inp in
            loop_sigs (count - 1) ((parse_signature binsig) :: sig_list)
      in
	loop_sigs number_of_sigs [];
	loop (remaining - 1)
  in
    loop num

let read_wotsap_file inc names keys signatures =
  let input = IO.input_channel inc in
    if (IO.read_line input) = "!<arch>" then
      try
	while true do
          let section_type, _, _, _, _, size, _ = parse_section_header input in
            match section_type with
	      | Readme -> 
		  seek_in inc ((pos_in inc) + size + 1)
	      | Names -> 
		  read_names input names size
	      | Keys -> 
		  read_keys input keys size
	      | Signatures -> 
		  read_signatures input signatures (DynArray.length names)
	      | Version -> 
		  ignore (IO.really_nread input size);
	      | Debug -> 
		  seek_in inc ((pos_in inc) + size)
	      | None -> raise (Wotsap_parse_error "no valid section header")
	done
      with IO.No_more_input -> 
	assert ((DynArray.length names) = (DynArray.length keys));
	assert ((DynArray.length names) = (DynArray.length signatures));
	IO.close_in input
    else
      raise (Wotsap_parse_error "no header found")

let _ =
  let inc = open_in (Sys.argv.(1)) in
  let names = DynArray.make 41000 in
  let keyids = DynArray.make 41000 in
  let signatures = DynArray.make 41000 in
    read_wotsap_file inc names keyids signatures;
    close_in inc;
    print_endline "foobar"
