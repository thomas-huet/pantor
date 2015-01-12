type t

exception Bad_message
exception Unsupported

val create : Lwt_unix.sockaddr -> string -> t Lwt.t
