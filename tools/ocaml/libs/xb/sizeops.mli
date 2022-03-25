(*@ predicate is_valid_size(i: integer) *)
(* TODO: gospel check doesn't like the mix of 'int' and 'integer' here, but cameleer works with it *)

type t = private int
(** a size in units of OCaml words.
    OCaml only allocates memory in multiples of words, so any byte lengths will have to be rounded
    up to words *)

(* we cannot define a GOSPEL model here yet,
   because we want this to be an integer, and not a record containing an integer *)
(* is it very important that the type 't' here is either completely abstract,
   or 'private int', i.e. the only way to construct a type t is through one of the functions
   below, even though you may inspect it directly after construction *)

(* Declare some helper functions and predicates (boolean typed functions)
   that are used in the GOSPEL specifications below.
   The definition of these can be found below in [module Size]
  *)
(*@ function to_int(x: t): int *)
(*@ predicate is_t(i:t) *)

(* [@logic] means that this is a pure function that can also be used in specifications later,
   and otherwise ignored by the regular compiler
 *)
val[@logic] of_int: int -> t
(** [of_int t] is [t] when [0 <= t < max_int/2], or an invalid value otherwise,
    such that [to_int (of_int invalid)] will return None. *)
(*@ r = of_int i
    (* header: all GOSPEL specs must give names to parameters and return values, so we can refer
       to them in the specification below *)

    pure
    (* no side-effects, terminates and doesn't raise exceptions *)

    ensures is_t r
    (* all functions returning a type t need this annotation until we can add global invariants *)

    ensures is_valid_size i -> to_int r = i
    (* if we were given a valid integer then we've stored it unaltered
       the arrow is one way (an implication), because when i = invalid although r and i might be
       equal, [i] is not a valid size
     *)


    ensures not is_valid_size i <-> not is_valid_size (to_int r)
    (* invalid integers are internally invalid too, there are more constraints below once we introduce [to_int_opt] *)
  *)

val[@logic] to_int_opt: t -> int option
(** [to_int_opt t] returns [Some t] if [t] represents a valid size in words, and None otherwise. *)
(*@ r = to_int_opt i

    requires is_t i
    (* all functions that take a parameter of type t needs a constraint here for the invariant *)

    pure

    ensures is_valid_size (to_int i)  <-> r = Some (to_int i)
    (* valid integers result in Some  when converted, and all Some values are built from valid
       integers *)

    ensures not is_valid_size (to_int i) <-> r = None
    (* invalid integers result in None, and all None values are invalid integers, hence the <-> *)
  *)

val (+): t -> t -> t
(*@ r = (+) a b
    requires is_t a && is_t b
    (* type invariant *)

    pure

    ensures is_t r
    (* type invariant *)

    ensures is_valid_size (to_int a)
            && is_valid_size (to_int b)
            && is_valid_size ((to_int a) + (to_int b)) -> to_int_opt r = Some ((to_int a) + (to_int b))
    (* if all values are valid then the result is a valid sum of the operands. *)

    ensures to_int_opt a = None
            || to_int_opt b = None
            || not is_valid_size ((to_int a) + (to_int b)) <-> to_int_opt r = None
    (* if either operand or the sum is invalid, or outside the valid range then the result is not
       valid.
       We leave the actual invalid value an implementation detail, to allow more room for
       optimization *)

    ensures b = of_int 0 -> r = a
    (* 0 leaves the value unchanged. Although a size of an item is never truly zero, something
       like a None reference might be considered of size 0 if the storage for the 'None' field is
       accounted for in other ways *)

*)


val (-): t -> t -> t
(*@ r = (-) a b
    requires is_t a && is_t b
    (* type invariant *)

    pure
    ensures is_t r
    (* type invariant *)

    ensures is_valid_size (to_int a)
            && is_valid_size (to_int b)
            && is_valid_size ((to_int a) - (to_int b)) <-> to_int_opt r = Some ((to_int a) - (to_int b))
    (* same as for +, but for - here *)

    ensures not is_valid_size (to_int a)
            || not is_valid_size (to_int b)
            || not is_valid_size ((to_int a) - (to_int b)) <-> to_int_opt r = None
    (* same as for +, but for - here *)
  *)

val[@logic] invalid: t
(** [invalid] is a size value that has overflown and is not valid. *)
(*@ ensures to_int_opt result = None
    (* invalid values are always None when converted. we must refer to the 'invalid' constant by
       the name 'result', because 'invalid' is not yet fully defined in the specification: we're
       inside its definition here *)

    ensures forall x. is_t x && to_int_opt x = None -> x = result
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
    rounded up to nearest word, including the 1 word overhead from OCaml values. *)
(*@ w = of_bytes b
    pure
    ensures is_t w
    ensures is_valid_size b -> is_valid_size (to_int w)
    ensures b < 0 -> to_int_opt w = None
    *)

val pp_dump: Format.formatter -> t -> unit
(** [pp_dump ppf t] pretty prints an internal representation of size on [t] *)
