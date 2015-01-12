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

let receive_string sock len =
  let buf = Bytes.create len in
  lwt n = pick [timeout receive_timeout; recv sock buf 0 len []] in
  if n < len then fail Bad_message
  else return (Bytes.to_string buf)

let create addr infohash =
  let sock = socket PF_INET SOCK_STREAM 0 in
  lwt () = connect sock addr in
  let peer_id = "Pantor              " in
  let header = "\019BitTorrent protocol\000\000\000\000\000\x10\000\000" in
  lwt _ = send sock (header ^ infohash ^ peer_id) 0 68 [] in
  lwt pstr = receive_string sock 20 in
  if pstr <> "\019BitTorrent protocol" then
    fail Bad_message
  else
    lwt features = receive_string sock 8 in
    if Char.code features.[5] land 0x10 = 0x10 then
      lwt info_hash = receive_string sock 20 in
      lwt peer_id = receive_string sock 20 in
      return {sock; info_hash; peer_id}
    else
      fail Unsupported
