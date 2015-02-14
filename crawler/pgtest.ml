module PG = PGOCaml_generic.Make(Thread)

let main =
  lwt db = PG.connect ~unix_domain_socket_dir:"/var/run/postgresql" () in
  lwt () = Lwt_io.printf "ready\n" in
  PG.close db

let () = Lwt_main.run main
