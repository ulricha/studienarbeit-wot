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
  let t1 = Unix.gettimeofday () in
  let ret = f () in
  let t2 = Unix.gettimeofday() in
  let m = sprintf "%s: %f sec" operation (t2 -. t1) in
    print_endline m;
    ret

let display_iterations counter operation interval =
  incr counter;
  if (!counter mod interval) = 0 then
    let msg = sprintf "%s: %d iterations" operation !counter in
      print_endline msg

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
