type t

exception Bad_message
exception Unsupported

val create : Lwt_unix.sockaddr -> string -> t Lwt.t

val close : t -> unit

val receive : t -> string Lwt.t

val extended_handshake : t -> t Lwt.t

val log : t -> unit Lwt.t
