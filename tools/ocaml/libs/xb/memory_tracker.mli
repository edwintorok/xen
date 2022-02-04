module Size : sig
  type t = private int
  (** a type representing the size of a value with overflow detection *)

  val invalid: t
  (** [invalid] is a size value that has overflown and is not valid. *)
  (*@ ensures result > 0 *)

  (* we cannot attach invariant to type aliases, so we'll add 'requires/ensures'
     to all functions handling 't' *)
  (*@ predicate is_t(t: integer) = 0 <= t <= invalid *)
  (*@ predicate is_valid_t(t: integer) = is_t t && t <> invalid *)

  val to_int: t -> int option
  (** [to_int t] returns [Some t] if [t] represents a valid size, and None otherwise. *)
  (*@ r = to_int i
      requires is_t i
      pure
      ensures i = invalid <-> r = None
      ensures in_valid_t i <-> r = Some i
      *)

  val of_int : int -> t
  (** [of_int t] is [t] when [t] >= 0, or an invalid value such that [to_int] will return None.  *)
  (*@ r = of_int i
      pure
      ensures is_t r
      ensures is_valid_t i <-> r = i && to_int r = Some i
      ensures not in_valid_t i <-> to_int r = None
      *)

  val (+) : t -> t -> t
  (** [a + b] is a saturating add of [a] and [b].
      If [a+b] would overflow then the result is set to [max_int].
      This ensures that overflows do not go undetected *)
  (*@ r = (+) a b
      requires is_t a && is_t b
      pure
      ensures r = of_int (a+b)
   *)

  val (-) : t -> t -> t
  (** [a - b] is a saturating subtraction of [a] and [b].
      If [a - b] would become negative then the result is set to [-1]. *)
  (*@ r = (-) a b
      requires is_t a && is_t b
      pure
      ensures r = of_int (a-b)
    *)

  val of_words: int -> t
  (** [words i] is the size of a value containing [i] words.
      Automatically adds 1 for OCaml value overhead *)
  (*@ r = of_words w
      pure
      ensures is_t r
      ensures r = of_int (w+1)
      *)

  val of_bytes: int -> t
  (** [of_bytes b] is the size of a value containing [i] bytes,
      rounded up to nearest word *)
  (*@ w = of_bytes b
      pure
      ensures is_t w
      ensures is_valid_t b -> is_valid_t w
      ensures not is_valid_t b -> w = invalid
      *)
end
