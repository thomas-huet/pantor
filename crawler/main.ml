open Lwt
open Unix
open Lwt_unix

module PGOCaml = PGOCaml_generic.Make(Thread)

module S = Set.Make(String)

let n_bits = 9
let timeout_good_nodes = 7. *. 60.
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

let good_nodes = Array.make (1 lsl n_bits) ([], [])

let read_int n s =
  let rec read_int n i =
    if n <= 8 then Char.code s.[i] lsr (8 - n)
    else Char.code s.[i] lsl (n - 8) + read_int (n - 8) (i + 1)
  in
  read_int n 0

let send_string sock dst str =
  lwt _ = sendto sock str 0 (String.length str) [] dst in
  return_unit

let get_nodes target =
  let i = read_int n_bits target in
  let good, unknown = good_nodes.(i) in
  good @ unknown

let propose_good ((nid, ADDR_INET (ip, port)) as node) =
  if ip <> my_ip then
  let i = read_int n_bits nid in
  let good, unknown = good_nodes.(i) in
  if List.mem node good then ()
  else if List.mem node unknown then
    good_nodes.(i) <- node :: good, List.filter ((<>) node) unknown
  else if List.length good + List.length unknown < (1 lsl k_bits) then
    good_nodes.(i) <- node :: good, unknown
  else if unknown = [] then ()
  else
    good_nodes.(i) <- node :: good, List.tl unknown

let propose_unknown ((nid, ADDR_INET (ip, port)) as node) =
  if ip <> my_ip then
  let i = read_int n_bits nid in
  let good, unknown = good_nodes.(i) in
  if List.mem node good then ()
  else begin
    async (fun () ->
      Ping ("tr", ids.(i))
      |> bencode
      |> send_string sockets.(i) (ADDR_INET (ip, port)));
    if not (List.mem node unknown)
       && List.length good + List.length unknown < (1 lsl k_bits)
    then
      good_nodes.(i) <- good, node :: unknown
  end

let wait d = catch (fun () -> timeout d) (fun _ -> return ());;

let lru = Lru.create 1000

let nodes_for_hash = Hashtbl.create 42

let is_done db infohash =
  if Lru.mem lru infohash then begin
    Lru.add lru infohash;
    return_true
  end else
    lwt witness = PGSQL(db) "SELECT torrent FROM file WHERE torrent = $infohash LIMIT 1" in
    if witness <> [] || Lru.mem lru infohash then begin
      Lru.add lru infohash;
      return_true
    end else
      return_false

let mark_done infohash =
  Lru.add lru infohash;
  try Hashtbl.remove nodes_for_hash infohash with Not_found -> ()

let request_metadata db delay peer infohash () =
  lwt already = is_done db infohash in
  if already then return_unit else
  lwt () = wait delay in
  let open Wire in
  try_lwt
    lwt wire = create peer infohash in
    lwt metadata = get_metadata wire in
    close wire;
    lwt already = is_done db infohash in
    if already then return_unit else begin
      mark_done infohash;
      let add name size =
	lwt () = Lwt_io.printf "%s\t%d\n" name size in
	let size = Int64.of_int size in
	PGSQL(db) "INSERT INTO file(torrent, name, size)
		   VALUES($infohash, $name, $size)"
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

let hunt db infohash nodes () =
  lwt finished = is_done db infohash in
  if finished then return_unit
  else begin
    let already = try
      Hashtbl.find nodes_for_hash infohash
    with Not_found -> begin
      Hashtbl.add nodes_for_hash infohash S.empty;
      S.empty
    end
    in
    let query already (node, addr) =
      if S.mem node already then already
      else begin
        async (fun () ->
          let i = Random.int (1 lsl n_bits) in
	  Get_peers (infohash, ids.(i), infohash)
	  |> bencode
	  |> send_string sockets.(i) addr);
        S.add node already
      end
    in
    Hashtbl.replace nodes_for_hash infohash (List.fold_left query already nodes);
    return_unit
  end

let answer db i orig = function
| Ping (tid, _) ->
  Pong (tid, ids.(i))
  |> bencode
  |> send_string sockets.(i) orig
| Find_node (tid, nid, target) ->
  Found_node (tid, ids.(i), get_nodes target)
  |> bencode
  |> send_string sockets.(i) orig
| Get_peers (tid, nid, infohash) ->
  async (hunt db infohash (get_nodes infohash));
  Got_nodes (tid, ids.(i), token, get_nodes infohash)
  |> bencode
  |> send_string sockets.(i) orig
| Found_node (tid, nid, nodes) ->
  List.iter propose_unknown nodes;
  return_unit
| Announce_peer (tid, nid, _, infohash, port, implied) ->
  let addr =
    if implied then orig
    else
      let ADDR_INET (ip, _) = orig in
      ADDR_INET (ip, port)
  in
  let () = async (request_metadata db wait_time addr infohash) in
  return_unit
| Pong (tid, nid) ->
  propose_good (nid, orig);
  return_unit
| Got_peers (infohash, nid, token, peers) ->
  let () =
    List.iter (fun peer -> async (request_metadata db 0. peer infohash)) peers
  in
  return_unit
| Got_nodes (infohash, _, _, nodes) ->
  async (hunt db infohash nodes); 
  return_unit
| Error (_, _, _) -> return_unit

let rec thread db i =
  let buf = Bytes.create 512 in
  lwt (_, orig) = recvfrom sockets.(i) buf 0 512 [] in
  let msg = try
    bdecode buf
  with _ -> Error ("", 0, "")
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
      match good_nodes.(((i lsr bits) lsl bits) + j) with
      | node :: _, _ | [], node :: _ -> Some (j, node)
      | [], [] -> scan (j + 1) bits
    else
      if bits >= n_bits then None else
      scan 0 (bits + 1)
  in
  scan 0 1

let rec supervisor i =
  if i = 0 then async bootstrap;
  if i < 1 lsl n_bits then
    lwt () = wait (timeout_good_nodes /. float (1 lsl n_bits)) in
    let good, _ = good_nodes.(i) in
    good_nodes.(i) <- [], good;
    let ping (_, addr) =
      for j = 1 to ping_n do
        async (fun () ->
          let j = Random.int (1 lsl n_bits) in
	  Ping ("tr", ids.(j))
	  |> bencode
	  |> send_string sockets.(j) addr)
      done
    in
    List.iter ping good;
    if List.length good < (1 lsl k_bits) then
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
