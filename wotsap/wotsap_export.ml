open Ekey
open Printf

let wotsap_signature esig index =
  let cert_level = 
    let l = esig.sig_level in
      if l >= 0x10 && l <= 0x13 then
	Int32.of_int (l - 0x10)
      else
	failwith "wotsap_export: certlevel not in 0x10 - 0x13"
  in
  let out = if esig.sig_puid_signed then 0x40000000l else 0l in
  let level_mask = Int32.shift_left cert_level 28 in
  let out = Int32.logor out level_mask in
  let index_mask = Int32.of_int index in
    Int32.logor out index_mask

(* (int * int32 list) list *)
let wotsap_signatures all_key_sigs lookup_index =
  let f single_key_sigs =
    let num = Int32.of_int (List.length single_key_sigs) in
    let binsigs =
      List.map 
	(fun esig ->
	   let (signer_id, siginfo) = esig in
	   let index = lookup_index (Misc.keyid32_of_keyid signer_id) in
	     wotsap_signature siginfo index)
	single_key_sigs
    in
      (num, binsigs)
  in
    List.map f all_key_sigs
      
let ekey_list_to_wotsap_data ekeys =
  let sorted = List.sort ~cmp:Ekey.compare_ekey ekeys in
  let keyid_list = List.map (fun ekey -> Misc.keyid32_of_keyid ekey.pki.key_keyid) sorted in
  let puid_list = List.map (fun ekey -> ekey.pki.key_puid) sorted in
  let siglist = List.map (fun ekey -> ekey.signatures) sorted in
  let keyid_array = Array.of_list keyid_list in
  let lookup_index k =
    printf "lookup_index 0x%x\n" (Int32.to_int k);
    let f i = compare k (Array.get keyid_array i) in
      Misc.bsearch f 0 (Array.length keyid_array - 1)
  in
    (keyid_array, puid_list, (wotsap_signatures siglist lookup_index))

let write_filename output filename size =
  let mtime = string_of_int (int_of_float (Unix.time ())) in
  let uid = "0" in
  let gid = "0" in
  let mode = "100644" in
  let size = string_of_int size in
  let s = Printf.sprintf "%-16s/%-12s%-6s%-6s%-8s%-10s`\n" 
    filename mtime uid gid mode size in
    IO.nwrite output s

let timestamp_string () =
  let tm = Unix.localtime (Unix.time ()) in
    sprintf "%d.%d.%d" tm.Unix.tm_mday tm.Unix.tm_mon (tm.Unix.tm_year + 1900)

let write_header_sections output = 
  let readme = sprintf "wotsap complete graph (%s)" (timestamp_string ()) in
  let version = "0.2\n" in
  IO.nwrite output "!<arch>\n";
    write_filename output "README" (String.length readme);
    IO.nwrite output readme;
    write_filename output "WOTVERSION" (String.length version);
    IO.nwrite output version
    
let write_names_section output names =
  let buf = Buffer.create ((List.length names) * 40) in
    List.iter (fun name -> Buffer.add_string buf name; Buffer.add_char buf '\n') names;
    write_filename output "names" (Buffer.length buf);
    IO.nwrite output (Buffer.contents buf)

let write_keys_section output keyids =
  let len = Array.length keyids * 8 in
    write_filename output "keys" len;
    Array.iter (fun k -> IO.BigEndian.write_real_i32 output k) keyids

let write_signature_section output signatures =
  let write_single_key_sigs (num, sigs) =
    IO.BigEndian.write_real_i32 output num;
    List.iter (fun s -> IO.BigEndian.write_real_i32 output s) sigs
  in
  let len = List.fold_left (fun len (num, _) -> len + 1 + (4 + (Int32.to_int num))) 0 signatures in
    write_filename output "signatures" len;
    List.iter write_single_key_sigs signatures

exception Finished

let dump_wotsap_file fname ekeys =
  let (keyids, puids, signatures) = ekey_list_to_wotsap_data ekeys in
  let output = File.open_out fname in
    try 
      write_header_sections output;
      write_names_section output puids;
      write_keys_section output keyids;
      write_signature_section output signatures;
      raise Finished
    with 
      | Finished -> IO.close_out output
      | _ as e -> IO.close_out output; raise e
    
