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
| Got_peers of tid * nid * token * Unix.sockaddr list
| Got_nodes of tid * nid * token * ninfo list
| Announce_peer of tid * nid * token * infohash * int * bool

exception Malformed_message

val bdecode : string -> t

val bencode : t -> string
