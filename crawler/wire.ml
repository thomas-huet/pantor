open Unix
open Lwt
open Lwt_unix

let receive_timeout = 3.

type t = {
  sock : file_descr;
  info_hash : string;
  peer_id : string;
}

exception Bad_message
exception Unsupported

let receive sock buf =
  lwt n = pick [timeout receive_timeout; recv sock buf 0 (Bytes.length buf) []] in
  if n < Bytes.length buf then fail Bad_message
  else return ()

let create addr infohash =
  let sock = socket PF_INET SOCK_STREAM 0 in
  lwt () = connect sock addr in
  let peer_id = "Pantor              " in
  let header = "\019BitTorrent protocol\000\000\000\000\000\x10\000\000" in
  lwt _ = send sock (header ^ infohash ^ peer_id) 0 68 [] in
  let pstr = Bytes.create 20 in
  lwt () = receive sock pstr in
  if pstr <> "\019BitTorrent protocol" then
    fail Bad_message
  else
    let features = Bytes.create 8 in
    lwt () = receive sock features in
    if Char.code features.[5] land 0x10 = 0x10 then
      let info_hash = Bytes.create 20 in
      lwt () = receive sock info_hash in
      let peer_id = Bytes.create 20 in
      lwt () = receive sock peer_id in
      return {sock; info_hash; peer_id}
    else
      fail Unsupported
