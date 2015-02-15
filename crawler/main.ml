open Lwt
open Unix
open Lwt_unix

module PGOCaml = PGOCaml_generic.Make(Thread)

let n_bits = 9
let timeout_good_nodes = 7 * 60
let k_bits = 3
let token = "token"
let bootstrap_nodes = [
  ADDR_INET (inet_addr_of_string "67.215.246.10", 6881);
  ADDR_INET (inet_addr_of_string "82.221.103.244", 6881);
  ADDR_INET (inet_addr_of_string "91.121.60.42", 6881);
  ADDR_INET (inet_addr_of_string "212.129.33.50", 6881);
]
let wait_time = 30

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

type status =
| Good of ninfo
| Unknown of ninfo
| Empty

let good_nodes = Array.make (1 lsl n_bits) Empty

let read_int n s =
  let rec read_int n i =
    if n <= 8 then Char.code s.[i] lsr (8 - n)
    else Char.code s.[i] lsl (n - 8) + read_int (n - 8) (i + 1)
  in
  read_int n 0

let get_nodes target =
  let i = read_int (n_bits - k_bits + 1) target lsl (k_bits - 1) in
  let rec loop j =
    if j <= 0 then [] else
    let good_node = match good_nodes.(i + j - 1) with
    | Empty ->
      let k = Random.int (1 lsl n_bits) in
      ids.(k), ADDR_INET (my_ip, base_port + k)
    | Good node | Unknown node -> node
    in
    (ids.(i + j - 1), ADDR_INET (my_ip, base_port + i + j - 1))
    :: good_node
    :: loop (j - 1)
  in
  loop (1 lsl (k_bits - 1))

let add_node (nid, addr) =
  let i = read_int n_bits nid in
  good_nodes.(i) <- Good (nid, addr)

let propose_node ((nid, ADDR_INET (ip, port)) as node) =
  if ip <> my_ip then
  let i = read_int n_bits nid in
  match good_nodes.(i) with
  | Empty | Unknown _ -> good_nodes.(i) <- Good node
  | Good _ -> ()

let send_string sock dst str =
  lwt _ = sendto sock str 0 (String.length str) [] dst in
  return ()

let wait n = catch (fun () -> timeout (float n)) (fun _ -> return ());;

let lru = Lru.create 1000

let request_metadata db delay peer infohash () =
  if Lru.mem lru infohash then begin
    Lru.add lru infohash;
    return_unit
  end else
    lwt witness = PGSQL(db) "SELECT torrent FROM file WHERE torrent = $infohash LIMIT 1" in
    if witness <> [] then begin
      Lru.add lru infohash;
      return_unit
    end else
      lwt () = wait delay in
      let open Wire in
      try_lwt
	lwt wire = create peer infohash in
	lwt metadata = get_metadata wire in
	close wire;
	let add name size =
	  lwt () = Lwt_io.printf "%s\t%d\n" name size in
	  let size = Int32.of_int size in
	  PGSQL(db) "INSERT INTO file(torrent, name, size)
		     VALUES($infohash, $name, $size)"
	in
	begin match metadata.files with
	| [[name], size] -> add name size
	| l ->
	  let total = List.fold_left (fun a (_, b) -> a + b) 0 l in
	  add metadata.name total (* TODO *)
	end;
	Lru.add lru infohash;
	return_unit
      with Bad_wire _ -> return_unit

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
  let _ =
    lwt () = wait wait_time in
    Get_peers (infohash, ids.(i), infohash)
    |> bencode
    |> send_string sockets.(i) orig
  in
  Got_nodes (tid, ids.(i), token, get_nodes infohash)
  |> bencode
  |> send_string sockets.(i) orig
| Found_node (tid, nid, nodes) -> return (List.iter propose_node nodes)
| Announce_peer (tid, nid, _, infohash, port, implied) ->
  let addr =
    if implied then orig
    else
      let ADDR_INET (ip, _) = orig in
      ADDR_INET (ip, port)
  in
  let () = async (request_metadata db wait_time addr infohash) in
  return ()
| Pong (tid, nid) -> return (add_node (nid, orig))
| Got_peers (infohash, nid, token, peers) ->
  let () =
    List.iter (fun peer -> async (request_metadata db 0 peer infohash)) peers
  in
  return ()
| Error (_, _, _)
| Got_nodes (_, _, _, _) -> return ()

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

let bootstrap =
  let ping addr =
    let i = Random.int (1 lsl n_bits) in
    Ping ("tr", ids.(i))
    |> bencode
    |> send_string sockets.(i) addr
  in
  Lwt_list.iter_s ping bootstrap_nodes

let close_node i =
  let rec scan j bits =
    if j < (1 lsl bits) then
      match good_nodes.(((i lsr bits) lsl bits) + j) with
      | Good node | Unknown node -> Some (j, node)
      | Empty -> scan (j + 1) bits
    else
      if bits >= n_bits then None else
      scan 0 (bits + 1)
  in
  scan 0 1

let rec supervisor i =
  if i < 1 lsl n_bits then
    match good_nodes.(i) with
    | Good ((_, addr) as node) -> begin
      good_nodes.(i) <- Unknown node;
      lwt () =
        Ping ("tr", ids.(i))
        |> bencode
        |> send_string sockets.(i) addr
      in
      supervisor (i + 1)
    end
    | Unknown _ | Empty ->
      match close_node i with
      | None -> supervisor (i + 1)
      | Some (j, (_, addr)) ->
	lwt () =
          Find_node ("tr", ids.(j), ids.(i))
          |> bencode
          |> send_string sockets.(j) addr
        in
        supervisor (i + 1)
  else
    lwt () = wait timeout_good_nodes in
    supervisor 0

let main =
  lwt db = PGOCaml.connect () in
  for i = 0 to (1 lsl n_bits) - 1 do
    async (fun () -> thread db i)
  done;
  supervisor 0

let () = Lwt_main.run main
