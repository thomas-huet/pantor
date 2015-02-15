open Unix
open Lwt
open Lwt_unix

let receive_timeout = 3.
let max_message_size = 20_000
let max_metadata_size = 20_000_000

let block_size = 16384

type t = {
  sock : file_descr;
  info_hash : string;
  peer_id : string;
}

type metadata = {
  name : string;
  piece_length : int;
  pieces : string;
  files : (string list * int) list;
}

type message =
| Keep_alive
| Choke
| Unchoke
| Interested
| Not_interested
| Have of int
| Bitfield of string
| Request of int * int * int
| Piece of int * int * string
| Cancel of int * int * int
| Port of int
| Extended of int * string
| Unknown of int * string

type extended_info = {
  ut_metadata : int;
  metadata_size : int;
}

exception Bad_wire of string

let fail_close exn sock =
  shutdown sock SHUTDOWN_ALL;
  fail exn

let raise_close exn sock =
  shutdown sock SHUTDOWN_ALL;
  raise exn

let close wire = shutdown wire.sock SHUTDOWN_ALL

let receive_string sock len =
  let buf = Bytes.create len in
  let rec read_all i =
    if i < len then
      lwt n = pick [timeout receive_timeout; recv sock buf i (len - i) []] in
      if n = 0 then
        fail_close (Bad_wire "Can't read") sock
      else
        read_all (i + n)
    else
      return_unit
  in
  lwt () = read_all 0 in
  return (Bytes.to_string buf)

let read_big_endian s i =
  (Char.code s.[i] lsl 24)
  lor (Char.code s.[i + 1] lsl 16)
  lor (Char.code s.[i + 2] lsl 8)
  lor (Char.code s.[i + 3])

let receive_message sock = try_lwt
  lwt lenstr = receive_string sock 4 in
  let len = read_big_endian lenstr 0 in
  if len > max_message_size then
    fail_close (Bad_wire "Message too large") sock
  else if len = 0 then
    return (Some Keep_alive)
  else
    lwt data = receive_string sock len in
    try_lwt
      let msg = match Char.code data.[0] with
      | 0 -> Choke
      | 1 -> Unchoke
      | 2 -> Interested
      | 3 -> Not_interested
      | 4 -> Have (read_big_endian data 1)
      | 5 -> Bitfield (String.sub data 1 (len - 1))
      | 6 -> Request (read_big_endian data 1, read_big_endian data 5, read_big_endian data 9)
      | 7 ->
        Piece (read_big_endian data 1, read_big_endian data 5, String.sub data 9 (len - 9))
      | 8 -> Cancel (read_big_endian data 1, read_big_endian data 5, read_big_endian data 9)
      | 9 -> Port (read_big_endian data 1)
      | 20 -> Extended (Char.code data.[1], String.sub data 2 (len - 2))
      | n -> Unknown (n, String.sub data 1 (len - 1))
      in
      return (Some msg)
    with _ -> fail_close (Bad_wire "Can't read message") sock
with Timeout -> return None

let write_big_endian n =
  let buf = Buffer.create 4 in
  Buffer.add_char buf (Char.chr ((n lsl 24) mod 256));
  Buffer.add_char buf (Char.chr ((n lsl 16) mod 256));
  Buffer.add_char buf (Char.chr ((n lsl 8) mod 256));
  Buffer.add_char buf (Char.chr (n mod 256));
  Buffer.contents buf

let send_string sock str =
  let lenstr = write_big_endian (String.length str) in
  send sock (lenstr ^ str) 0 (4 + String.length str) []

let send_message sock msg =
  let str = match msg with
  | Keep_alive -> ""
  | Choke -> "\000"
  | Unchoke -> "\001"
  | Interested -> "\002"
  | Not_interested -> "\003"
  | Have piece -> "\004" ^ write_big_endian piece
  | Bitfield b -> "\005" ^ b
  | Request (index, start, len) ->
    "\006" ^ write_big_endian index ^ write_big_endian start ^ write_big_endian len
  | Piece (index, start, data) ->
    "\007" ^ write_big_endian index ^ write_big_endian start ^ data
  | Cancel (index, start, len) ->
    "\008" ^ write_big_endian index ^ write_big_endian start ^ write_big_endian len
  | Port n -> "\009" ^ write_big_endian n
  | Extended (n, data) -> "\020" ^ String.make 1 (Char.chr n) ^ data
  | Unknown (n, data) -> String.make 1 (Char.chr n) ^ data
  in
  lwt _ = send_string sock str in
  return_unit

let create addr infohash =
  let sock = socket PF_INET SOCK_STREAM 0 in
  try_lwt
    lwt () = pick [timeout receive_timeout; connect sock addr] in
    let peer_id = "Pantor              " in
    let header = "\019BitTorrent protocol\000\000\000\000\000\x10\000\000" in
    lwt _ = send sock (header ^ infohash ^ peer_id) 0 68 [] in
    lwt pstr = receive_string sock 20 in
    if pstr <> "\019BitTorrent protocol" then
      fail_close (Bad_wire "Wrong protocol") sock
    else
      lwt features = receive_string sock 8 in
      if Char.code features.[5] land 0x10 <> 0x10 then
	fail_close (Bad_wire "No support for extended protocol") sock
      else
	lwt info_hash = receive_string sock 20 in
	if info_hash <> infohash then
	  fail_close (Bad_wire "Infohash does not match") sock
	else
	  lwt peer_id = receive_string sock 20 in
	  return {sock; info_hash; peer_id}
  with Unix_error(ECONNREFUSED, "connect", "") ->
    fail_close (Bad_wire "Connection failed") sock

open Bencode

let extended_handshake ip hand =
  let Dict handshake, _ = decode hand in
  let Int metadata_size = Dict.find "metadata_size" handshake in
  let Dict m = Dict.find "m" handshake in
  let Int ut_metadata = Dict.find "ut_metadata" m in
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
    "v", String "Pantor";
    "yourip", String (Buffer.contents buf);
    "reqq", Int 42]))
  in
  Extended (0, shake), {ut_metadata; metadata_size}

let request_metadata_piece sock ut_metadata i =
  let request =
    encode (Dict (dict_of_list [
      "msg_type", Int 0;
      "piece", Int i]))
  in
  send_message sock (Extended (ut_metadata, request))

let get_metadata wire =
  let Unix.ADDR_INET (ip, _) = getpeername wire.sock in
  let rec loop ext raw =
    lwt msg = receive_message wire.sock in
    match msg with
    | None -> fail_close (Bad_wire "No message (timeout)") wire.sock
    | Some msg -> match msg with
      | Extended (0, hand) -> begin try
	let shake, ext =
          extended_handshake ip hand
        in
	lwt () = send_message wire.sock shake in
        lwt () = request_metadata_piece wire.sock ext.ut_metadata 0 in
        if ext.metadata_size > max_metadata_size then
          fail_close (Bad_wire "Metadata too large") wire.sock
        else
	  loop ext (Array.make ((ext.metadata_size + block_size - 1) / block_size) None)
      with _ -> fail_close (Bad_wire "Bad extended handshake") wire.sock end
      | Extended (n, data) when n = ext.ut_metadata -> begin try
        let Dict d, i = decode data in
        match Dict.find "msg_type" d with
        | Int 0 -> fail_close (Bad_wire "Data requested") wire.sock
        | Int 1 ->
          let Int piece = Dict.find "piece" d in
          raw.(piece) <- Some (String.sub data i (String.length data - i));
          if piece + 1 < Array.length raw then
            lwt () = request_metadata_piece wire.sock ext.ut_metadata (piece + 1) in
            loop ext raw
          else
            let buf = Buffer.create 42 in
            let add = function
            | None -> raise_close (Bad_wire "Missing pieces of metadata") wire.sock
            | Some s -> Buffer.add_string buf s
            in
            Array.iter add raw;
            let s = Buffer.contents buf in
            let sha1 = Cryptokit.Hash.sha1 () in
            sha1#add_string s;
            if wire.info_hash <> sha1#result then
              fail_close (Bad_wire "Incorrect hash") wire.sock
            else begin try
              let Dict meta, _ = decode s in
              let String name = Dict.find "name" meta in
              let Int piece_length = Dict.find "piece length" meta in
              let String pieces = Dict.find "pieces" meta in
              let files =
                if Dict.mem "length" meta then
                  let Int length = Dict.find "length" meta in
                  [[name], length]
                else
                  let List l = Dict.find "files" meta in
                  let extract (Dict file) =
                    let Int length = Dict.find "length" file in
                    let List path = Dict.find "path" file in
                    List.map (fun (String s) -> s) path, length
                  in
                  List.map extract l
              in
              return {name; piece_length; pieces; files}
            with _ -> fail_close (Bad_wire "Can't parse metadata") wire.sock end
        | Int 2 -> fail_close (Bad_wire "Request for data rejected") wire.sock
      with _ -> fail_close (Bad_wire "Can't parse extended message") wire.sock end
      | _ -> loop ext raw
  in
  loop {ut_metadata = 0; metadata_size = 0} [||]
