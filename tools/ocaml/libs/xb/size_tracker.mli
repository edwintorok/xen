(** Module to calculate an upper bound on the memory used by various OCaml values.
	The computation is done incrementally, such that updates and queries can be completed in O(1)
	without walking the entire data structure to sum up its elements.
	*)


type t = private int
(** The type used to calculate the size of various OCaml data structures.
	`private int` is used here so that the compiler can avoid boxing.
	Internally it stores the size in words.
	*)

val of_string_length: int -> t
(** [of_string_length n] calculates the size of an OCaml value storing a string of [n] bytes in
length. Takes into consideration the extra padding at the end of the string and the extra 1 word of
GC header overhead *)

val string: string -> t
(** [string s] is [s |> String.length |> of_string_length] *)

val bytes: bytes -> t
(** [bytes b] is [b |> Bytes.length |> of_string_length] *)

val record_field: t
(** [record_field] is the overhead of one record field, i.e. one word that either points to an OCaml
	value, or is an unboxed integer *)

val to_byte_count : t -> int
(** [to_byte_count t] converts [t] to a number of bytes.
	This is not called [to_bytes] to avoid confusion with the OCaml [bytes] type *)

val add: t -> t -> t
(** [add a b] is [a + b] *)

val sub: t -> t -> t
(** [sub a b] is [a - b].
	It is the caller's responsibility to ensure this is not negative *)

val mul: t -> int -> t
(** [mul x c] is [c * x] *)

val zero: t
(** [zero] is 0 words *)

type 'a size_of = 'a -> t (** a function to calculate the size of 'a values *)

val queue: 'a size_of -> 'a Queue.t size_of
(** [queue element_size_of queue] calculates the size of a queue
	by summing up individual element sizes computed using [element_size_of] *)

val hashtbl: 'a size_of -> 'b size_of -> ('a, 'b) Hashtbl.t size_of
(** [hashtbl key_size_of value_size_of hashtbl] calculates the size of a small [hashtbl] knowing the
	sizes of keys through [key_size_of] and values through [value_size_of] *)

val list: 'a size_of -> 'a list size_of
(** [list element_size_of queue] calculates the size of a list
	by summing up individual element sizes computed using [element_size_of] *)
