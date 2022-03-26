let[@logic] invalid_size = max_int / 2
(*@ ensures result * 2 + 1 = Pervasives.max_int *)
(* an upper limit on valid sizes: we do not want overflow, so we want a value that when added
 together stays within max_int *)

(*@ predicate is_valid_size(i: integer) = 0 <= i < invalid_size *)

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
   and the annotation otherwise ignored by the regular compiler
   TODO: is that true for 4.02.3 too?
 *)
let[@logic] to_int_opt i = if i < 0 then None else Some i
(*@ r = to_int_opt i

    requires is_t i
    (* all functions that take a parameter of type t needs a constraint here for the invariant *)

    pure

    ensures is_valid_size (to_int i)  <-> r = Some (to_int i)
    (* valid integers result in Some  when converted, and all Some values are built from valid
       integers *)

  *)

let[@logic] invalid = -1
(*@ ensures to_int_opt result = None && result = -1 *)

(* for now the specifications below are a copy of those from the SizeSig and the .mli
   some tooling could help automatically copy these specifications so we don't have to duplicate
   them *)

let[@logic] of_int i = if i < 0 || i >= invalid_size then begin
    (* raising doesn't work: it gets caught and transformed into Xb.Invalid *)
    (* TODO: only raise in "debug/fuzz" mode *)
  (*  let bt = Printexc.get_callstack 100 in
    Printexc.print_raw_backtrace stderr bt;
    Printf.eprintf "invalid size %d\n" i;*)
    -1
  end else i
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

(* TODO: the specification exists in the .mli file too,
   there is some duplication here, can we move all the specification into the .mli file,
   and potentially use a script to generate a merged file for cameleer? *)
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

      ensures b = of_int 0 -> r = a
      (* 0 leaves the value unchanged. Although a size of an item is never truly zero, something
         like a None reference might be considered of size 0 if the storage for the 'None' field is
         accounted for in other ways *)

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

let words_of_bytes b = (b + bytes_per_word ) / bytes_per_word
(*@ w = words_of_bytes b
    requires b >= 0
    pure
    ensures (w-1) * bytes_per_word <= b < w * bytes_per_word
  *)

let of_bytes b = if b < 0 then invalid else of_words (words_of_bytes b)
(** [of_bytes b] is the size of a value containing [i] bytes,
    padded the way a string in OCaml would be padded,
    so its size is a multiple of word, and last byte is used to indicate amount of padding *)
(*@ w = of_bytes b
    pure
    ensures is_t w
    ensures is_valid_size b -> is_valid_size w
    ensures b < 0 -> to_int_opt w = None
    *)

let pp_dump ppf t = Format.pp_print_int ppf t
