type 'a t

val make : 'a -> 'a t

val get : 'a t -> 'a

val set : 'a t -> 'a -> unit

val fetch_and_add : int t -> int -> int
