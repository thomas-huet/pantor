type t

type metadata = {
  name : string;
  piece_length : int;
  pieces : string;
  files : (string list * int) list;
}

exception Bad_wire of string

val create : Lwt_unix.sockaddr -> string -> t Lwt.t

val close : t -> unit

val get_metadata : t -> metadata Lwt.t
