(** 62-bit unsigned integer.

  Unboxed on 64-bit architectures, range [0, max_int] = [0, 2^62-1].

  Boxed on 32-bit architectures, same range [0, 2^62-1].
*)
type t = private int

val of_int64 : int64 -> t
(** [of_int64 i] converts [i] to [t] by masking off bit 63. *)

val to_int64 : t -> int64
(** [to_int64 i] *)
