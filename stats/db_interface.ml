open Batteries

let today = Unix.time ()

let sig_creation_times dbh keyids =
  PGSQL(dbh) "select ctime from sigs where signee in $@keyids and signer in $@keyids"

let get_key_records dbh keyids =
  PGSQL(dbh) "select keyid, puid, ctime, exptime from keys where keyid in $@keyids"

let get_keys_per_period dbh interval_list keyids =
  print_endline "get_keys_per_period";
  let get (interval_start, interval_end) =
    let records = 
      PGSQL(dbh) "select * from keys where ctime >= $interval_start and ctime <= $interval_end and keyid in $@keyids"
    in
      print_endline (Printf.sprintf "get interval %f keys %d" interval_start (List.length records));
      (interval_start, records)
  in
    List.map get interval_list

let get_keys_per_period_cid dbh interval_list cid =
  print_endline "get_keys_per_period";
  let get (interval_start, interval_end) =
    let records = 
      PGSQL(dbh) "select keys.keyid, keys.puid, keys.ctime, keys.exptime, keys.revoktime, keys.alg, keys.keylen from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = $cid and ctime >= $interval_start and ctime <= $interval_end"
    in
      print_endline (Printf.sprintf "get interval %f keys %d" interval_start (List.length records));
      (interval_start, records)
  in
    List.map get interval_list

let get_keys_per_period_all dbh interval_list =
  print_endline "get_keys_per_period";
  let get (interval_start, interval_end) =
    let records = 
      PGSQL(dbh) "select * from keys where ctime >= $interval_start and ctime <= $interval_end and revoktime is null and (exptime is null or exptime > $interval_end)"
    in
      print_endline (Printf.sprintf "get interval %f keys %d" interval_start (List.length records));
      (interval_start, records)
  in
    List.map get interval_list

let get_valid_sigs dbh =
  PGSQL(dbh) "(select signer, signee from sigs inner join keys on sigs.signer = keys.keyid where keys.revoktime is null and (keys.exptime is null or keys.exptime > $today) and sigs.revoktime is null and (sigs.exptime is null or sigs.exptime > $today)) intersect (select signer, signee from sigs inner join keys on sigs.signee = keys.keyid where keys.revoktime is null and (keys.exptime is null or keys.exptime > $today) and sigs.revoktime is null and (sigs.exptime is null or sigs.exptime > $today))"

let get_valid_signed_keys dbh =
  PGSQL(dbh) "select keyid from keys inner join sigs on sigs.signer = keys.keyid or sigs.signee = keys.keyid where keys.revoktime is null and (keys.exptime is null or keys.exptime > $today) and sigs.revoktime is null and (sigs.revoktime is null or sigs.revoktime > $today)"
