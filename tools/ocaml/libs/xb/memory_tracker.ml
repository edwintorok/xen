(* TODO: this should be added by the cameleer wrapper script, and loaded from sys.mli *)
module type Sys = sig
  val[@logic] word_size: int (* axiom ws: word_size = 32 || word_size = 64 *)
  (*@ axiom ws: word_size = 32 *)
  (* The word size can currently only be 32 or 64, use an axiom to define this.
     It ensures that the counter-example uses more realistic values *)
end
module type Pervasives = sig
  val[@logic] min_int: int

  val[@logic] max_int: int

  (* GOSPEL doesn't support 0x yet, and although this can be defined using the Power module and ^
     that will slow down proof/counterexample search a lot (since ^ is defined using proof machinery)
   *)
  (*@ axiom int31mm: Sys.word_size = 32 -> min_int = -1073741824 && max_int = 1073741823 *)
  (*@ axiom int63mm: Sys.word_size = 64 -> min_int = -4611686018427387904 && max_int = 4611686018427387903 *)
end

(* The GOSPEL specifications can be checked via cameleer, either by using the dune rule,
   or directly:
   ```
   cameleer memory_tracker.ml --prover=cvc4-ce --batch
   ```

   alt-ergo can also be used as a prover, however for debugging the specification and the code
   using CVC4 or Z3 is better because they can also produce counterexamples directly on a failure
   (that is what the -ce suffix is for
*)

let[@logic] invalid_size = Pervasives.max_int / 2
(*@ ensures result * 2 + 1 = Pervasives.max_int *)
(* an upper limit on valid sizes: we do not want overflow, so we want a value that when added
   together stays within max_int *)

(*@ predicate is_valid_size(i: integer) = 0 <= i < invalid_size *)

module type SizeSig = sig
  type t (* = private int - removed for cameleer *)
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
  (** [to_int_opt t] returns [Some t] if [t] represents a valid size, and None otherwise. *)
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
end


module Size: SizeSig = struct
  type t = int
  (* could've been implemented as 'int option', however it is very unlikely
     this would overflow and it is more efficient to just operate on regular integers:
     using an 'option' time would cause extra GC allocation, when what we're trying to measure here
     is memory consumption *)

  (* any negative value here represents an invalid size,
     and any positive value a valid size.
     We do not allow values that would overflow when we perform
     operations on them at all: it would be cumbersome to specify 2's complement arithmetic
     in cameleer currently
     *)

  (*@ function to_int(x: t): int = x *)

  (* this could've been written more simply by changing the left hand interval,
     but it is more explicit this way that -1 is not a valid value *)
  (*@ predicate is_t(i:t) = 0 <= i < invalid_size || i = -1 *)

  (* [@logic] means that this is a pure function that can also be used in specifications later,
     and otherwise ignored by the regular compiler
   *)
  let[@logic] to_int_opt i = if i < 0 then None else Some i
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

  let[@logic] invalid = -1
  (*@ ensures to_int_opt result = None && result = -1 *)

  (* for now the specifications below are a copy of those from the SizeSig and the .mli
     some tooling could help automatically copy these specifications so we don't have to duplicate
     them *)

  let[@logic] of_int i = if i < 0 || i >= invalid_size then invalid else i
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

  (*@ predicate is_valid_binop(res a b: t) =
        is_valid_size (to_int a)
        && is_valid_size (to_int b)
        && is_valid_size (to_int res) *)
  (* helper predicate (boolean typed function) to be used below *)

  let[@inline] binop_result ~res ~a ~b = if a < 0 || b < 0 then invalid else of_int res
  (*@ r = binop ~res ~a ~b
      requires is_t a && is_t b
      pure
      ensures is_t r
      ensures is_valid_binop res a b <-> is_valid_size r && r = res
      ensures not is_valid_binop res a b <-> to_int_opt r = None
  *)


  let (+) a b = binop_result ~res:(a + b) ~a ~b
  (*@ r = (+) a b
      requires is_t a && is_t b
      (* type invariant *)

      pure

      ensures is_valid_binop (a+b) a b -> r = a + b
      ensures not is_valid_binop (a+b) a b <-> to_int_opt r = None
      (* some extra conditions we can check given that we know 'r' is an int *)

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


  let (-) a b = binop_result ~res:(a - b) ~a ~b
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

  let of_words x = of_int (x+1)
  (** [words i] is the size of a value containing [i] words.
      Automatically adds 1 for OCaml value overhead *)
  (*@ r = of_words w
      pure
      ensures is_t r
      ensures r = of_int (w+1)
      *)


  let bytes_per_word = Sys.word_size / 8
  (*@ ensures result = 4 *)

  let words_of_bytes b = (b + bytes_per_word - 1) / bytes_per_word
  (*@ w = words_of_bytes b
      requires b >= 0
      pure
      ensures (w-1) * bytes_per_word < b <= w * bytes_per_word
    *)

  let of_bytes b = if b < 0 then invalid else of_words (words_of_bytes b)
  (** [of_bytes b] is the size of a value containing [i] bytes,
      rounded up to nearest word *)
  (*@ w = of_bytes b
      pure
      ensures is_t w
      ensures is_valid_size b -> is_valid_size w
      ensures b < 0 -> to_int_opt w = None
      *)
end
