open Batteries

let sig_creation_times dbh keyids =
  PGSQL(dbh) "select ctime from sigs where signee in $@keyids and signer in $@keyids"

let get_key_records dbh keyids =
  PGSQL(dbh) "select keyid, puid, ctime, exptime from keys where keyid in $@keyids"

let get_keys_per_period dbh interval_list keyids =
  print_endline "get_keys_per_period";
  let get (interval_start, interval_end) =
    print_endline (Printf.sprintf "get interval %f" interval_start);
    let records = 
      PGSQL(dbh) "select * from keys where ctime >= $interval_start and ctime <= $interval_end and keyid in $@keyids"
    in
      (interval_start, records)
  in
    List.map get interval_list
