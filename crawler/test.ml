let digit c =
  if c <= '9' then Char.code c - Char.code '0'
  else if c <= 'Z' then Char.code c - Char.code 'A' + 10
  else Char.code c - Char.code 'a' + 10

let of_hex h =
  let n = String.length h / 2 in
  let s = Bytes.create n in
  for i = 0 to n - 1 do
    s.[i] <- Char.chr (digit h.[2 * i] * 16 + digit h.[2 * i + 1]);
  done;
  Bytes.to_string s

let main =
  let ip = Unix.inet_addr_of_string Sys.argv.(1) in
  let port = int_of_string Sys.argv.(2) in
  let infohash = of_hex Sys.argv.(3) in
  lwt wire = Wire.create (Unix.ADDR_INET (ip, port)) infohash in
  lwt metadata = Wire.get_metadata wire in
  Wire.close wire;
  lwt () = Lwt_io.printf "%s\n\n" metadata.Wire.name in
  Lwt_list.iter_s (fun (path, len) -> Lwt_io.printf "%s\t%d\n" (List.fold_left (fun a b -> a ^ "/" ^ b) "" path) len) metadata.Wire.files

let () = Lwt_main.run main
