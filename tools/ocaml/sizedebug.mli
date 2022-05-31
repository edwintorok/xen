(**
   Debug size measurements:
   This looks into internal data structures, measuring the size using Obj.reachable_words,
   and comparing it with what is calculated using Size_tracker, and logs any discrepancies.
   This is very specific to the data structure, and has a runtime overhead, so it is not part of the regular code,
   but only used during fuzzing/debugging, and only once a mismatch has been found.

   There are some size calculations that are required to be exact, and some that are only approximate (calculated size >= real size).
   Strings are actually only approximate, unless all of them are unique, because as an optimization string storage could be shared for identical strings.
*)

type 'a t (** a size measurement debugger for type 'a *)

val check_exn: 'a t -> 'a -> unit
(** [check_exn debugger value] checks that the size of [value] is computed
 correctly using [debugger]
 *)

val same: 'a t -> 'a t -> 'a t
(** [same a b] asserts that [a] and [b] compute compatible sizes,
  e.g. one could be that sums up record elements, the other that calls
  a function in Xenstored
*)

val v: ?exact:bool -> ('a -> Xenbus.Size_tracker.t) -> string -> 'a t
(** [v ?exact size_of name] is a size debugger for [name] comparing size computed
  through Obj.reachable_words and [size_of].
  if [exact] is true the size computation is required to match exactly,
  otherwise the real size is allowed to be <= computed size (to allow for sharing,
  or approximate overhead computation)
  *)

type 'a field
(** size debugger for record fields *)

val field: string -> 'a t -> ('b -> 'a) -> 'b field
(** [field fieldname fieldtype getter] is a size debugger for the record field
  [fieldname] using [fieldtype] to compute size and [getter] to get the record
  field from the record
*)

val record: string -> 'b field list -> 'b t
(** [record name fields] is a size debugger for a record consisting of [fields]. *)

val seq: ('a -> 'b Seq.t) -> 'b t -> string -> 'a t
(** [seq to_seq element name] is a size debugger for a type ['a]
 that has a sequence of elements ['b] (accessed using [to_seq]) named [name]
 *)

type 'a case
val case: string -> 'a t -> ('b -> 'a option) -> 'b case
(** [variant constructor element construct] is a size debugger for a variant
  with [constructor] containing [element], that can be constructed using [construct]
*)

val variant : 'a case list -> string -> 'a t

val pair: 'a t -> 'b t -> ('a * 'b) t

val bool: bool t
val int: int t
val int32: int32 t
val int64: int64 t
val nativeint: nativeint t
val float: float t
val string: string t
val bytes: bytes t

val pair_seq: 'a t -> 'b t -> ('c -> ('a * 'b) Seq.t) -> string -> 'c t
