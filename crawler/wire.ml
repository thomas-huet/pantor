open Unix
open Lwt
open Lwt_unix

let receive_timeout = 3.
let max_message_size = 20000;

type t = {
  sock : file_descr;
  info_hash : string;
  peer_id : string;
  metadata_size : int;
  ut_metadata : int;
}

exception Bad_message
exception Unsupported

let fail_close exn sock =
  let () = shutdown sock SHUTDOWN_ALL in
  fail exn

let receive_string sock len =
  let buf = Bytes.create len in
  lwt n = pick [timeout receive_timeout; recv sock buf 0 len []] in
  if n < len then fail_close Bad_message sock
  else return (Bytes.to_string buf)

let create addr infohash =
  let sock = socket PF_INET SOCK_STREAM 0 in
  lwt () = connect sock addr in
  let peer_id = "Pantor              " in
  let header = "\019BitTorrent protocol\000\000\000\000\000\x10\000\000" in
  lwt _ = send sock (header ^ infohash ^ peer_id) 0 68 [] in
  lwt pstr = receive_string sock 20 in
  if pstr <> "\019BitTorrent protocol" then
    fail_close Bad_message sock
  else
    lwt features = receive_string sock 8 in
    if Char.code features.[5] land 0x10 = 0x10 then
      lwt info_hash = receive_string sock 20 in
      lwt peer_id = receive_string sock 20 in
      return {sock; info_hash; peer_id; metadata_size = 0; ut_metadata = 0}
    else
      fail_close Unsupported sock

let close wire = shutdown wire.sock SHUTDOWN_ALL

let receive wire =
  lwt lenstr = receive_string wire.sock 4 in
  let len = (Char.code lenstr.[0] lsl 24) lor (Char.code lenstr.[1] lsl 16) lor (Char.code lenstr.[2] lsl 8) lor (Char.code lenstr.[3]) in
  if len > max_message_size then fail_close Bad_message wire.sock
  else receive_string wire.sock len

open Bencode

let extended_handshake wire =
  lwt hand = receive wire in
  if String.length hand > 2 && hand.[0] = '\020' && hand.[1] = '\000' then try_lwt
    let Dict handshake, _ = decode (String.sub hand 2 (String.length hand - 2)) in
    let Int metadata_size = Dict.find "metadata_size" handshake in
    let Dict m = Dict.find "m" handshake in
    let Int ut_metadata = Dict.find "ut_metadata" m in
    let Unix.ADDR_INET (ip, _) = getpeername wire.sock in
    let buf = Buffer.create 4 in
    Scanf.sscanf (Unix.string_of_inet_addr ip) "%d.%d.%d.%d" (fun a b c d ->
      Buffer.add_char buf (Char.chr a);
      Buffer.add_char buf (Char.chr b);
      Buffer.add_char buf (Char.chr c);
      Buffer.add_char buf (Char.chr d);
    );
    let shake = encode (Dict (dict_of_list [
      "m", Dict (dict_of_list ["ut_metadata", Int ut_metadata]);
      "metadata_size", Int metadata_size;
      "p", Int 0;
      "v", String "Pantor (github.com/thomas-huet/pantor)";
      "yourip", String (Buffer.contents buf);
      "reqq", Int 42]))
    in
    lwt _ = send wire.sock shake 0 (String.length shake) [] in
    return {wire with metadata_size; ut_metadata}
  with _ -> fail_close Bad_message wire.sock
  else
    fail_close Bad_message wire.sock

let rec log wire =
  let open Log in
  let open Lwt_io in
  let ADDR_INET (ip, port) = getpeername wire.sock in
  lwt () = printf "%a Listenning %s (%s:%d) for %s\n" date () (hex wire.peer_id) (string_of_inet_addr ip) port (hex wire.info_hash) in
  lwt s = receive wire in
  lwt () = printf "%a Received %s\n" date () (hex s) in
  log wire
