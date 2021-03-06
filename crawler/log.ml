open Kprc
open Unix
open Util

let date () () =
  let t = localtime (time ()) in
  Printf.sprintf "[%02d:%02d:%02d]" t.tm_hour t.tm_min t.tm_sec

let input (ADDR_INET (ip, port)) msg = begin
  let open Lwt_io in
  match msg with
  | Ping (_, nid) ->
    printf "%a Ping from %s (%s:%d)\n" date () (hex nid) (string_of_inet_addr ip) port
  | Find_node (_, nid, target) ->
    printf "%a Find_node %s from %s (%s:%d)\n" date () (hex target) (hex nid) (string_of_inet_addr ip) port
  | Get_peers (_, nid, target) ->
    printf "%a Get_peers for %s from %s (%s:%d)\n" date () (hex target) (hex nid) (string_of_inet_addr ip) port
  | Got_peers (_, nid, _, peers) ->
    lwt () = printf "%a Got_peers from %s (%s:%d)\n" date () (hex nid) (string_of_inet_addr ip) port in
    let string_of_peer () (ADDR_INET (ip, port)) =
      Printf.sprintf "%s:%d" (string_of_inet_addr ip) port
    in
    Lwt_list.iter_s (printf "%a\n" string_of_peer) peers
  | Got_nodes (_, nid, _, nodes) ->
    lwt () = printf "%a Got_nodes from %s (%s:%d)\n" date () (hex nid) (string_of_inet_addr ip) port in
    Lwt_list.iter_s (fun (nid, ADDR_INET (ip, port)) -> printf "%s (%s:%d)\n" (hex nid) (string_of_inet_addr ip) port) nodes
  | Announce_peer (_, nid, token, infohash, _, _) ->
    printf "%a Announce_peer %s (%s:%d) for %s (%s)\n" date () (hex nid) (string_of_inet_addr ip) port (hex infohash) (hex token)
  | Error (_, code, descr) ->
    printf "%a Error %d: \"%s\" from %s:%d\n" date () code descr (string_of_inet_addr ip) port
  | _ -> Lwt.return_unit
  | Pong (_, nid) ->
    printf "%a Pong from %s (%s:%d)\n" date () (hex nid) (string_of_inet_addr ip) port
  | Found_node (_, nid, nodes) -> begin
    lwt () = printf "%a Found_node from %s (%s:%d)\n" date () (hex nid) (string_of_inet_addr ip) port in
    Lwt_list.iter_s (fun (nid, ADDR_INET (ip, port)) -> printf "%s (%s:%d)\n" (hex nid) (string_of_inet_addr ip) port) nodes
  end
end
