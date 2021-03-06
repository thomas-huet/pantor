(*
    Pantor
    Copyright (C) 2015  Thomas HUET

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open Lwt
open Unix
open Lwt_unix

module PGOCaml = PGOCaml_generic.Make(Thread)

module StrSet = Set.Make(String)

module SockSet = Set.Make(struct
  type t = sockaddr
  let compare = compare
end)

module NodeSet = Set.Make(struct
  type t = string * sockaddr
  let compare = compare
end)

let n_bits = 9
let timeout_good_nodes = 14. *. 60.
let k_bits = 3
let token = "token"
let bootstrap_nodes = [
  ADDR_INET (inet_addr_of_string "67.215.246.10", 6881);
  ADDR_INET (inet_addr_of_string "82.221.103.244", 6881);
  ADDR_INET (inet_addr_of_string "91.121.60.42", 6881);
  ADDR_INET (inet_addr_of_string "212.129.33.50", 6881);
]
let wait_time = 30.
let ping_n = 10

let my_ip, base_port =
  if Array.length Sys.argv = 3 then
    inet_addr_of_string Sys.argv.(1), int_of_string Sys.argv.(2)
  else
    let () = Printf.printf "usage: pantor ip port_low\n" in
    exit 1

let create_socket i =
  let s = socket PF_INET SOCK_DGRAM 0 in
  bind s (ADDR_INET (inet_addr_any, base_port + i));
  s

let ids = Array.make (1 lsl n_bits) ""

let randomize_ids () =
  let base_id = Bytes.init 20 (fun _ -> Char.chr (Random.int 256)) in
  for i = 0 to Array.length ids - 1 do
    if n_bits < 8 then
      Bytes.set base_id 0
	(Char.chr
	  (((i mod 256) lsl (8 - n_bits))
	   lor (((1 lsl (8 - n_bits)) - 1) land Char.code (Bytes.get base_id 0))))
    else
      Bytes.set base_id 0 (Char.chr (i mod 256));
    if n_bits > 8 then
      Bytes.set base_id 1
	(Char.chr
	  (((i / 256) lsl (16 - n_bits))
	   lor ((1 lsl (16 - n_bits) - 1) land Char.code (Bytes.get base_id 1))));
    ids.(i) <- Bytes.to_string base_id
  done

let sockets = Array.init (1 lsl n_bits) create_socket
let () = randomize_ids ()

open Kprc

let good_nodes = Array.make (1 lsl n_bits) (NodeSet.empty, NodeSet.empty)

let read_int n s =
  let rec read_int n i =
    if n <= 8 then Char.code s.[i] lsr (8 - n)
    else Char.code s.[i] lsl (n - 8) + read_int (n - 8) (i + 1)
  in
  read_int n 0

let send_string sock dst str = try_lwt
  lwt _ = sendto sock str 0 (String.length str) [] dst in
  return_unit
with _ -> return_unit

let get_nodes target =
  let i = read_int n_bits target in
  NodeSet.elements (fst good_nodes.(i))

let propose_good ((nid, ADDR_INET (ip, port)) as node) =
  if ip <> my_ip then
  let i = read_int n_bits nid in
  let good, unknown = good_nodes.(i) in
  if NodeSet.mem node good then ()
  else if NodeSet.cardinal good < (1 lsl k_bits) then
    good_nodes.(i) <- NodeSet.add node good, NodeSet.filter ((<>) node) unknown
  else
    good_nodes.(i) <- NodeSet.add node good, NodeSet.empty

let propose_unknown ((nid, ADDR_INET (ip, port)) as node) =
  if ip <> my_ip then
  let i = read_int n_bits nid in
  let good, unknown = good_nodes.(i) in
  if not (NodeSet.mem node good)
  && not (NodeSet.mem node unknown)
  && NodeSet.cardinal good < (1 lsl k_bits) then begin
    good_nodes.(i) <- good, NodeSet.add node unknown;
    async (fun () ->
      Find_node ("pi", ids.(i), ids.(i))
      |> bencode
      |> send_string sockets.(i) (ADDR_INET (ip, port)))
  end

let wait d = catch (fun () -> timeout d) (fun _ -> return ());;

let lru = Lru.create 1000

let nodes_for_hash = Hashtbl.create 42

let peers_for_hash = Hashtbl.create 42

let is_done db infohash =
  if Lru.mem lru infohash then begin
    Lru.add lru infohash;
    return_true
  end else
    let infohex = Util.hex infohash in
    lwt witness = PGSQL(db) "SELECT torrent FROM file WHERE torrent = $infohex LIMIT 1" in
    if witness <> [] || Lru.mem lru infohash then begin
      Lru.add lru infohash;
      return_true
    end else
      return_false

let mark_done infohash =
  Lru.add lru infohash;
  begin try Hashtbl.remove nodes_for_hash (String.sub infohash 0 2) with Not_found -> () end;
  try Hashtbl.remove peers_for_hash infohash with Not_found -> ()

let request_metadata db delay peer infohash () =
  if String.length infohash <> 20 then return_unit else
  lwt already = is_done db infohash in
  if already then return_unit else
  let peers = try
    Hashtbl.find peers_for_hash infohash
  with Not_found -> begin
    Hashtbl.add peers_for_hash infohash SockSet.empty;
    SockSet.empty
  end
  in
  if SockSet.mem peer peers then return_unit else begin
  Hashtbl.replace peers_for_hash infohash (SockSet.add peer peers);
  lwt () = wait delay in
  let open Wire in
  try_lwt
    lwt wire = create peer infohash in
    lwt metadata = get_metadata wire in
    close wire;
    lwt already = is_done db infohash in
    if already then return_unit else begin
      mark_done infohash;
      let infohex = Util.hex infohash in
      let add name size =
	lwt () = Lwt_io.printf "%s\t%d\n" name size in
	let size = Int64.of_int size in
	PGSQL(db) "INSERT INTO file(torrent, name, size)
		   VALUES($infohex, $name, $size)"
      in
      let total = List.fold_left (fun a (_, b) -> a + b) 0 metadata.files in
      lwt () = add (Yojson.Basic.to_string (`String metadata.name)) total in
      lwt () = match metadata.files with
      | [[], _] -> return_unit
      | _ ->
	Lwt_list.iter_s
	  (fun (path, size) ->
	    add (Yojson.Basic.to_string (`List (List.map (fun s -> `String s) path))) size)
	  metadata.files
      in
      return_unit
    end
  with
  | Bad_wire s ->
    Lwt_io.printf "/!\\ Bad wire \"%s\"\n" s
  | Timeout ->
    Lwt_io.printf "/!\\ Timeout\n"
  end

let hunt db info nodes () = try
  let infohash =
    if String.length info = 20 then info
    else fst (Hashtbl.find nodes_for_hash info)
  in
  let info = String.sub info 0 2 in
  lwt finished = is_done db infohash in
  if finished then return_unit
  else begin
    let bits, already = try
      snd (Hashtbl.find nodes_for_hash info)
    with Not_found -> begin
      Hashtbl.add nodes_for_hash info (infohash, (n_bits, StrSet.empty));
      n_bits, StrSet.empty
    end
    in
    let query already (node, addr) =
      if read_int bits node <> read_int bits infohash
      || StrSet.mem node already then already
      else begin
        async (fun () ->
          let i = Random.int (1 lsl n_bits) in
	  Get_peers (info, ids.(i), infohash)
	  |> bencode
	  |> send_string sockets.(i) addr);
        StrSet.add node already
      end
    in
    let already = List.fold_left query already nodes in
    let best =
      StrSet.filter
        (fun node -> read_int (bits + 1) node = read_int (bits + 1) infohash)
        already
    in
    if StrSet.cardinal best > (1 lsl k_bits) * 2 then
      Hashtbl.replace nodes_for_hash info (infohash, (bits + 1, best))
    else
      Hashtbl.replace nodes_for_hash info (infohash, (bits, already));
    return_unit
  end
with _ -> return_unit

let answer db i orig = function
| Ping (tid, nid) ->
  propose_unknown (nid, orig);
  Pong (tid, ids.(i))
  |> bencode
  |> send_string sockets.(i) orig
| Find_node (tid, nid, target) ->
  Found_node (tid, ids.(i), get_nodes target)
  |> bencode
  |> send_string sockets.(i) orig
| Get_peers (tid, nid, infohash) ->
  if String.length infohash = 20 then begin
    async (hunt db infohash (get_nodes infohash));
    Got_nodes (tid, ids.(i), token, get_nodes infohash)
    |> bencode
    |> send_string sockets.(i) orig
  end else
    Error (tid, 203, "invalid info_hash (not 20 bytes)")
    |> bencode
    |> send_string sockets.(i) orig
| Found_node (_, nid, nodes) ->
  propose_good (nid, orig);
  List.iter propose_unknown nodes;
  return_unit
| Announce_peer (_, nid, _, infohash, port, implied) ->
  let addr =
    if implied then orig
    else
      let ADDR_INET (ip, _) = orig in
      ADDR_INET (ip, port)
  in
  let () = async (request_metadata db wait_time addr infohash) in
  return_unit
| Pong (_, nid) ->
  propose_good (nid, orig);
  return_unit
| Got_peers (info, nid, token, peers) ->
  propose_good (nid, orig);
  begin try
    let infohash = fst (Hashtbl.find nodes_for_hash info) in
    List.iter (fun peer -> async (request_metadata db 0. peer infohash)) peers
  with Not_found -> ()
  end;
  return_unit
| Got_nodes (info, nid, _, nodes) ->
  propose_good (nid, orig);
  if String.length info = 2 then
    async (hunt db info nodes); 
  return_unit
| Error (_, _, _) -> return_unit

let rec thread db i =
  let buf = Bytes.create 512 in
  lwt (_, orig) = recvfrom sockets.(i) buf 0 512 [] in
  let msg = try
    bdecode buf
  with _ -> Error ("", 0, String.escaped buf)
  in
  lwt () = Log.input orig msg in
  lwt () = answer db i orig msg in
  thread db i

let bootstrap () =
  let find_node addr =
    let i = Random.int (1 lsl n_bits) in
    Find_node ("tr", ids.(i), ids.(i))
    |> bencode
    |> send_string sockets.(i) addr
  in
  Lwt_list.iter_s find_node bootstrap_nodes

let close_node i =
  let rec scan j bits =
    if j < (1 lsl bits) then
      let nodes = fst good_nodes.(((i lsr bits) lsl bits) + j) in
      if NodeSet.is_empty nodes then scan (j + 1) bits
      else Some (j, NodeSet.choose nodes)
    else
      if bits >= n_bits then None else
      scan 0 (bits + 1)
  in
  scan 0 0

let rec supervisor i =
  if i = 0 then async bootstrap;
  if i < 1 lsl n_bits then
    lwt () = wait (timeout_good_nodes /. float (1 lsl n_bits)) in
    let good, _ = good_nodes.(i) in
    good_nodes.(i) <- NodeSet.empty, good;
    let ping (_, addr) =
      for j = 1 to ping_n do
        async (fun () ->
          let j = Random.int (1 lsl n_bits) in
	  Ping ("pi", ids.(j))
	  |> bencode
	  |> send_string sockets.(j) addr)
      done
    in
    NodeSet.iter ping good;
    if NodeSet.cardinal good < (1 lsl k_bits) then
      match close_node i with
      | None -> supervisor (i + 1)
      | Some (j, (_, addr)) ->
	lwt () =
          Find_node ("tr", ids.(i), ids.(i))
          |> bencode
          |> send_string sockets.(i) addr
        in
        supervisor (i + 1)
    else
      supervisor (i + 1)
  else
    supervisor 0

let main = try_lwt
  lwt db = PGOCaml.connect () in
  for i = 0 to (1 lsl n_bits) - 1 do
    async (fun () -> thread db i)
  done;
  supervisor 0
with Unix_error(ENOENT, "connect", "") ->
  Lwt_io.eprintf "Can't connect to database\n"

let () = Lwt_main.run main
