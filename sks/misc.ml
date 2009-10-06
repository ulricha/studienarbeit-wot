open Printf

let hexstring digest = 
  let result = String.create (String.length digest * 2) in
  let hex = "0123456789ABCDEF" in
    for i = 0 to String.length digest - 1 do
      let c = Char.code digest.[i] in
	result.[2*i] <- hex.[c lsr 4];
	result.[2*i+1] <- hex.[c land 0xF]
    done;
    result

let keyid_to_string ?(short=false) keyid = 
  let hex = hexstring keyid in
  if short
  then String.sub hex (String.length hex - 8) 8
  else hex

type cert_level = Generic | Persona | Casual | Positive

let cert_level_of_int d =
  match d with
    | 0x10 -> Generic
    | 0x11 -> Persona
    | 0x12 -> Casual
    | 0x13 -> Positive
    | _ -> failwith (sprintf "cert_level_of_int: unexpected value %d" d)

let string_of_cert_level l =
  match l with
    | Generic -> "Generic"
    | Persona -> "Persona"
    | Casual -> "Casual"
    | Positive -> "Positive"

let hash_keyid keyid =
  let x = 0 in
  let x = x lor (int_of_char keyid.[3] lsl 24) in
  let x = x lor (int_of_char keyid.[2] lsl 16) in
  let x = x lor (int_of_char keyid.[1] lsl 8) in
  let x = x lor (int_of_char keyid.[0]) in
    x

let time_evaluation f operation =
  let t1 = Unix.time () in
  let ret = f () in
  let t2 = Unix.time() in
    printf "%s: %d sec\n" operation (int_of_float (t2 -. t1));
    ret

let display_iterations counter operation =
  incr counter;
  if (!counter mod 10000) = 0 then
    printf "%s: %d iterations\n" operation !counter

let compare_reverse f a b =
  match f a b with 
    | 0 -> 0
    | x when x > 0 -> -1
    | x -> 1

let apply_all_pairs l1 l2 f cmp =
  let rec outer_loop l =
    match l with
      | v :: tl ->
	  begin
	    let rec inner_loop l =
	      match l with 
		| inner_v :: tl ->
		    if (cmp v inner_v) = 0 then
		      inner_loop tl
		    else 
		      begin
			f v inner_v;
			inner_loop tl
		      end
		| [] -> 
		    ()
	    in
	      inner_loop l2;
	      outer_loop tl
	  end
      | [] -> ()
  in
    outer_loop l1

let write_intmap_to_file map fname = 
  let f out =
    Map.IntMap.iter (fun k v -> fprintf out "%d %d\n" k v) map
  in
    File.with_file_out fname f

let intmap_increase_or_add map key =
  try 
    Map.IntMap.add key ((Map.IntMap.find key map) + 1) map
  with Not_found -> Map.IntMap.add key 1 map
