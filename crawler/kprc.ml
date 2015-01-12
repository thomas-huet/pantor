type tid = string
type nid = string
type infohash = string
type token = string
type ninfo = nid * Unix.sockaddr

type t =
| Error of tid * int * string
| Ping of tid * nid
| Pong of tid * nid
| Find_node of tid * nid * nid
| Found_node of tid * nid * ninfo list
| Get_peers of tid * nid * infohash
| Got_peers of tid * nid * token * string list
| Got_nodes of tid * nid * token * ninfo list
| Announce_peer of tid * nid * token * infohash * int * bool

let sockaddrs_of_compact s =
  let rec loop i =
    if 26 * i >= String.length s then [] else
    let inet_addr =
      Unix.inet_addr_of_string
        (Printf.sprintf "%d.%d.%d.%d"
          (Char.code s.[26*i+20])
          (Char.code s.[26*i+21])
          (Char.code s.[26*i+22])
          (Char.code s.[26*i+23]))
    in
    (String.sub s (26*i) 20,
     Unix.ADDR_INET (inet_addr, Char.code s.[26*i+24] * 256 + Char.code s.[26*i+25]))
    :: loop (i+1)
  in
  loop 0

let compact_of_sockaddrs l =
  let buf = Buffer.create 26 in
  let compact (nid, Unix.ADDR_INET (ip, port)) =
    Buffer.add_string buf nid;
    Scanf.sscanf (Unix.string_of_inet_addr ip) "%d.%d.%d.%d" (fun a b c d ->
      Buffer.add_char buf (Char.chr a);
      Buffer.add_char buf (Char.chr b);
      Buffer.add_char buf (Char.chr c);
      Buffer.add_char buf (Char.chr d);
    );
    Buffer.add_char buf (Char.chr (port / 256));
    Buffer.add_char buf (Char.chr (port mod 256));
  in
  List.iter compact l;
  Buffer.contents buf

open Bencode

exception Malformed_message

let bdecode s = try
  let gets k d =
    let String s = Dict.find k d in
    s
  in
  let geti k d =
    let Int s = Dict.find k d in
    s
  in
  let Dict d = decode s in
  let tid = gets "t" d in
  match gets "y" d with
  | "q" -> begin
    let Dict a = Dict.find "a" d in
    match gets "q" d with
    | "ping" ->
      Ping (tid, gets "id" a)
    | "find_node" ->
      Find_node (tid, gets "id" a, gets "target" a)
    | "get_peers" ->
      Get_peers (tid, gets "id" a, gets "info_hash" a)
    | "announce_peer" ->
      let implied = Dict.mem "implied_port" a && Dict.find "implied_port" a = Int 1 in
      Announce_peer (tid, gets "id" a, gets "token" a, gets "info_hash" a, geti "port" a, implied)
    | _ -> raise Malformed_message
  end
  | "r" ->
    let Dict r = Dict.find "r" d in
    let nid = gets "id" r in
    if Dict.mem "values" r then
      let List peers = Dict.find "values" r in
      Got_peers (tid, nid, gets "token" r, List.map (fun (String s) -> s) peers)
    else if Dict.mem "nodes" r then
      let nodes = sockaddrs_of_compact (gets "nodes" r) in
      if Dict.mem "token" r then
	Got_nodes (tid, nid, gets "token" r, nodes)
      else
	Found_node (tid, nid, nodes)
    else Pong (tid, nid)
  | "e" ->
    let List [Int code; String descr] = Dict.find "e" d in
    Error (tid, code, descr)
  | _ -> raise Malformed_message
with _ -> raise Malformed_message

let bencode = function
| Error (tid, code, descr) ->
  encode (Dict (dict_of_list [
    "t", String tid;
    "y", String "e";
    "e", List [Int code; String descr]]))
| Ping (tid, nid) ->
  encode (Dict (dict_of_list [
    "t", String tid;
    "y", String "q";
    "q", String "ping";
    "a", Dict (dict_of_list ["id", String nid])]))
| Pong (tid, nid) ->
  encode (Dict (dict_of_list [
    "t", String tid;
    "y", String "r";
    "r", Dict (dict_of_list ["id", String nid])]))
| Find_node (tid, nid, target) ->
  encode (Dict (dict_of_list [
    "t", String tid;
    "y", String "q";
    "q", String "find_node";
    "a", Dict (dict_of_list ["id", String nid; "target", String target])]))
| Found_node (tid, nid, nodes) ->
  encode (Dict (dict_of_list [
    "t", String tid;
    "y", String "r";
    "r", Dict (dict_of_list [
      "id", String nid;
      "nodes", String (compact_of_sockaddrs nodes)])]))
| Get_peers (tid, nid, infohash) ->
  encode (Dict (dict_of_list [
    "t", String tid;
    "y", String "q";
    "q", String "get_peers";
    "a", Dict (dict_of_list ["id", String nid; "info_hash", String infohash])]))
| Got_peers (tid, nid, token, peers) ->
  encode (Dict (dict_of_list [
    "t", String tid;
    "y", String "r";
    "r", Dict (dict_of_list [
      "id", String nid;
      "token", String token;
      "values", List (List.map (fun s -> String s) peers)])]))
| Got_nodes (tid, nid, token, nodes) ->
  encode (Dict (dict_of_list [
    "t", String tid;
    "y", String "r";
    "r", Dict (dict_of_list [
      "id", String nid;
      "token", String token;
      "nodes", String (compact_of_sockaddrs nodes)])]))
| Announce_peer (tid, nid, token, infohash, port, implied) ->
  encode (Dict (dict_of_list [
    "t", String tid;
    "y", String "q";
    "q", String "announce_peer";
    "a", Dict (dict_of_list [
      "id", String nid;
      "token", String token;
      "info_hash", String infohash;
      "port", Int port;
      "implied_port", Int (if implied then 1 else 0)])]))
