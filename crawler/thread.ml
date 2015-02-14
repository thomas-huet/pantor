type 'a t = 'a Lwt.t

let return = Lwt.return

let (>>=) = Lwt.bind

let fail = Lwt.fail

let catch = Lwt.catch

type in_channel = Lwt_io.input_channel

type out_channel = Lwt_io.output_channel

let open_connection sockaddr =
  let sock = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Lwt_unix.SOCK_STREAM 0 in
  try_lwt
    lwt () = Lwt_unix.connect sock sockaddr in
    Lwt_unix.set_close_on_exec sock;
    return (Lwt_io.of_fd ~mode:Lwt_io.input sock, Lwt_io.of_fd ~mode:Lwt_io.output sock)
  with exn ->
    lwt () = Lwt_unix.close sock in
    fail exn

let output_char = Lwt_io.write_char

let output_binary_int oc n =
  lwt () = output_char oc (Char.chr (n lsr 24)) in
  lwt () = output_char oc (Char.chr ((n lsr 16) land 255)) in
  lwt () = output_char oc (Char.chr ((n lsr 8) land 255)) in
  output_char oc (Char.chr (n land 255))

let output_string = Lwt_io.write

let flush = Lwt_io.flush

let input_char = Lwt_io.read_char

let input_binary_int ic =
  lwt a = input_char ic in
  lwt b = input_char ic in
  lwt c = input_char ic in
  lwt d = input_char ic in
  return ((Char.code a lsl 24)
      lor (Char.code b lsl 16)
      lor (Char.code c lsl 8)
      lor (Char.code c))

let really_input = Lwt_io.read_into_exactly

let close_in = Lwt_io.close
