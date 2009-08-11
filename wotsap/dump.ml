open StdLabels
open MoreLabels
open Printf
open Common
open Packet
open KeyMerge

(*
typ key = packet list (packet.ml)
parsen von keys/packet lists -> struktur in keyMerge.ml
Keydb.iter iteriert ueber alle keys (= packet lists)
*)

let settings = {
  Keydb.withtxn = !Settings.transactions;
  Keydb.cache_bytes = !Settings.cache_bytes;
  Keydb.pagesize = !Settings.pagesize;
  Keydb.dbdir = Lazy.force Settings.dbdir;
  Keydb.dumpdir = Lazy.force Settings.dumpdir;
}

module Keydb = Keydb.Unsafe

let itertest ~hash ~key =
  if parseable key = true then
    print_endline "parseable"
  else
    print_endline "not parseable"
;;

let () = Keydb.iter itertest;;

