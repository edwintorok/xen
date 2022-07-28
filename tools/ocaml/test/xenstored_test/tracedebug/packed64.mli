(** type for storing int64 or floats without allocation *)
type 'a t

val create : int -> 'a t
(** [create n] creates an array of size [n]. *)

val fill : _ t -> unit
(** [fill t] fills [t] with the default value. *)

val set_int64 : int64 t -> int -> int64 -> unit
(** [set_int64 t pos i64] sets the item at element [pos] to [i64]
  without allocating or copying [i64] *)

val set_float : float t -> int -> float -> unit
(** [set_float t pos f] sets the item at element [pos] to [f],
  withoutallocating or copying [f] *)

val get_int64 : int64 t -> int -> int64
(** [get_int64 t pos] gets the [pos] item from [t] *)

val get_float : float t -> int -> float
(** [get_float t pos] gets the [pos] item from [t] *)
