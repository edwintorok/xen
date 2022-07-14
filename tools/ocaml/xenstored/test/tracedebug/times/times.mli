type t (** type for time ring *)

val create: int -> t
(** [create n] creates a time ring of size [n] *)

val record: t -> int -> unit
(** [record t index] records the current timestamp in the ring at index [i].
  Caller has to ensure the ring index is within bounds.
  This call is performance critical, it records the timestamp without conversion
  and allocation. *)

val fill : t -> unit
(** [fill t] fills [t] with invalid timestamps *)

val get_as_ns : t -> int -> int64
(** [get_as_ns t i] returns the [i]th timestamp converted to nanoseconds *)

val precision: int
(** [precision] usual precision in fractional digits. [9] would be [1 ns] *)

val id: string
(** [id] identifies which method we used to fetch the time *)
