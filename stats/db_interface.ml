open Batteries

let sig_creation_times dbh keyids =
  PGSQL(dbh) "select ctime from sigs where signee in $@keyids and signer in $@keyids"

let get_key_records dbh keyids =
  PGSQL(dbh) "select keyid, puid, ctime, exptime from keys where keyid in $@keyids"

let divide_period s e interval =
  let rec eat l interval_start =
    let interval_end = interval_start +. interval in
      if interval_end >= e then
	(interval_start, e) :: l
      else
	eat ((interval_start, interval_end) :: l) (interval_end +. 1.)
  in
    eat [] s

let get_period_keys dbh interval_list =
  let get (interval_start, interval_end) =
    PGSQL(dbh) "select keyid from keys where ctime >= $interval_start and ctime <= $interval_end"
  in
    List.map get interval_list
