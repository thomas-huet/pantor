type 'a t

val create : int -> 'a t

val mem : 'a t -> 'a -> bool

val add : 'a t -> 'a -> unit
