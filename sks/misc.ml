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

let keyid32_of_keyid k =
  let s = hexstring k in
  let hex = if not (s.[0] = '0' && s.[1] = 'x') then "0x" ^ s else s in
  let x = Int64.of_string hex in
    Int64.to_int32 x

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

let time_eval f operation =
  let t1 = Unix.gettimeofday () in
  let ret = f () in
  let t2 = Unix.gettimeofday() in
  let m = sprintf "%s: %f sec" operation (t2 -. t1) in
    print_endline m;
    ret

let time_iterations op interval =
  let counter = ref 0 in
  let timer = ref (Unix.gettimeofday ()) in
    fun () ->
      incr counter;
      if (!counter mod interval) = 0 then
	let current_time = Unix.gettimeofday () in
	let last_interval = current_time -. !timer in
	let msg = sprintf "%s: %d iterations (last interval took %f sec)" op !counter last_interval in
	  timer := current_time;
	  print_endline msg

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


(** Binary search.

   (f i) returns -1, 0 or 1, and should be monotonic.
   f should have values for all i in [low,high], inclusive.

   if \E i \in [low,high] such that (f i) = 0, 
   then such an i is returned.
   Otherwise, i is returned such that 
   (f i = 1) and (f (i-1)=-1).
   Unless it's all 1's or all -1s.  If it's all 1s, the first 1 is returned.
   If it's all -1's, then raise Not_found
*)
let bsearch ~f ~low ~high = 
  let rec bsearch ~f ~low ~high =
    if low = high then 
      match f low with
	  0 -> low
	| 1 -> low
	| _ -> raise Not_found
    else let mid = (low + high)/2 in
      match f mid with
	  0 -> mid
	| 1 -> bsearch ~f ~low ~high:mid
	| (-1) -> bsearch ~f ~low:(mid+1) ~high
	| _ -> raise (Failure ("bsearch: " ^ 
			       "Search returned value other than -1,0,1"))
  in 
    if high < low
    then raise Not_found
    else bsearch ~f ~low ~high

let i64_to_float_option = function
  | Some i -> Some (Int64.to_float i)
  | None -> None

let int_to_float_option = function
  | Some i -> Some (float_of_int i)
  | None -> None
