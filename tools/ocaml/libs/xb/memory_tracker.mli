module Size : sig
  type t = private int
  (** a type representing the size of a value with overflow detection *)
  (* we cannot attach invariant to type aliases, so we'll add 'requires/ensures'
     to all functions handling 't' *)

  val invalid: t
  (** [invalid] is a size value that has overflown and is not valid. *)
  (*@ ensures result < 0 *)

  (*@ function to_int(x: t): int *)
  (*@ predicate is_t(i:t) *)

  val of_int: int -> t
  (** [of_int t] is [t] when [0 <= t < max_int/2], or an invalid value otherwise,
      such that [to_int (of_int invalid)] will return None. *)
  (*@ r = of_int i
      pure
      ensures is_t r
      ensures is_valid_size i -> to_int r = i
      ensures not is_valid_size i <-> not is_valid_size (to_int r)
    *)
  (* 'to_int r = i -> is_valid_size i' is not true when i = min_int so we only use -> and not <-> *)

  val[@logic] to_int_opt: t -> int option
  (** [to_int_opt t] returns [Some t] if [t] represents a valid size, and None otherwise. *)
  (*@ r = to_int_opt i
      requires is_t i
      pure
      ensures is_valid_size (to_int i)  <-> r = Some (to_int i)
      ensures not is_valid_size (to_int i) <-> r = None
    *)
  (* r is None if and only if [i] is not a valid size, hence the <-> *)

  val invalid: t
  (** [invalid] is a size value that has overflown and is not valid. *)
  (*@ ensures to_int_opt invalid = None *)

  val (+): t -> t -> t
  (** [a + b] adds [a] and [b] if they do not overflow.
      If [a+b] would overflow then the result is set to an invalid value,
      such that all further operations on the invalid value will result in an invalid value.
      This ensures that overflows do not go undetected *)
  (*@ r = (+) a b
      requires is_t a && is_t b
      pure
      ensures is_t r
      ensures is_valid_size (to_int a)
              && is_valid_size (to_int b)
              && is_valid_size ((to_int a) + (to_int b)) <-> to_int_opt r = Some ((to_int a) + (to_int b))
      ensures not is_valid_size (to_int a)
              || not is_valid_size (to_int b)
              || not is_valid_size ((to_int a) + (to_int b)) <-> to_int_opt r = None
    *)

  val (-): t -> t -> t
  (** [a - b] subtracts [a] and [b] if they do not overflow.
      If [a-b] would overflow then the result is set to an invalid value,
      such that all further operations on the invalid value will result in an invalid value.
      This ensures that overflows do not go undetected *)
  (*@ r = (-) a b
      requires is_t a && is_t b
      pure
      ensures is_t r
      ensures is_valid_size (to_int a)
              && is_valid_size (to_int b)
              && is_valid_size ((to_int a) - (to_int b)) <-> to_int_opt r = Some ((to_int a) - (to_int b))
      ensures not is_valid_size (to_int a)
              || not is_valid_size (to_int b)
              || not is_valid_size ((to_int a) - (to_int b)) <-> to_int_opt r = None
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
