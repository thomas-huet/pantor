open Kprc
open Unix

let digit n =
  if n < 10 then Char.chr (n + 48)
  else Char.chr (n + 55)

let hex s =
  let h = Bytes.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    h.[2 * i] <- digit (Char.code s.[i] / 16);
    h.[2 * i + 1] <- digit (Char.code s.[i] mod 16);
  done;
  Bytes.to_string h

let date () () =
  let t = localtime (time ()) in
  Printf.sprintf "[%d:%d:%d]" t.tm_hour t.tm_min t.tm_sec

let input (ADDR_INET (ip, port)) msg = begin
  match msg with
  | Ping (_, nid) ->
    Lwt_io.printf "%a Ping from %s (%s:%d)\n" date () (hex nid) (string_of_inet_addr ip) port
  | Pong (_, nid) ->
    Lwt_io.printf "%a Pong from %s (%s:%d)\n" date () (hex nid) (string_of_inet_addr ip) port
  | Find_node (_, nid, target) ->
    Lwt_io.printf "%a Find_node %s from %s (%s:%d)\n" date () (hex target) (hex nid) (string_of_inet_addr ip) port
  | Found_node (_, nid, nodes) -> begin
    lwt () = Lwt_io.printf "%a Found_node from %s (%s:%d)\n" date () (hex nid) (string_of_inet_addr ip) port in
    Lwt_list.iter_s (fun (nid, ADDR_INET (ip, port)) -> Lwt_io.printf "%s (%s:%d)\n" (hex nid) (string_of_inet_addr ip) port) nodes
  end
  | Get_peers (_, nid, target) ->
    Lwt_io.printf "%a Get_peers for %s from %s (%s:%d)\n" date () (hex target) (hex nid) (string_of_inet_addr ip) port
  | Got_peers (_, nid, _, peers) ->
    lwt () = Lwt_io.printf "%a Got_peers from %s (%s:%d)\n" date () (hex nid) (string_of_inet_addr ip) port in
    Lwt_list.iter_s (Lwt_io.printf "%s\n") peers
  | Got_nodes (_, nid, _, nodes) ->
    lwt () = Lwt_io.printf "%a Got_nodes from %s (%s:%d)\n" date () (hex nid) (string_of_inet_addr ip) port in
    Lwt_list.iter_s (fun (nid, ADDR_INET (ip, port)) -> Lwt_io.printf "%s (%s:%d)\n" (hex nid) (string_of_inet_addr ip) port) nodes
  | Announce_peer (_, nid, token, infohash, _, _) ->
    Lwt_io.printf "%a Announce_peer %s (%s:%d) for %s (%s)\n" date () (hex nid) (string_of_inet_addr ip) port (hex infohash) (hex token)
  | Error (_, code, descr) ->
    Lwt_io.printf "%a Error %d: \"%s\" from %s:%d\n" date () code descr (string_of_inet_addr ip) port
end
