(* TODO: this should be added by the cameleer wrapper script, and loaded from sys.mli *)
module type Sys = sig
  val[@logic] word_size: int
  (* axiom ws: word_size = 32 || word_size = 64 *)
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

let[@logic] invalid_size = Pervasives.max_int / 2
(*@ ensures result * 2 + 1 = Pervasives.max_int *)

(*@ predicate is_valid_size(i: integer) = 0 <= i < invalid_size *)

module type SizeSig = sig
  type t (* = private int - removed for cameleer *)

  (*@ function to_int(x: t): int *)
  (*@ predicate is_t(i:t) *)

  val of_int: int -> t
  (*@ r = of_int i
      pure
      ensures is_t r
      ensures is_valid_size i -> to_int r = i
      ensures not is_valid_size i <-> not is_valid_size (to_int r)
    *)
  (* 'to_int r = i -> is_valid_size i' is not true when i = min_int so we only use -> and not <-> *)

  val[@logic] to_int_opt: t -> int option
  (*@ r = to_int_opt i
      requires is_t i
      pure
      ensures is_valid_size (to_int i)  <-> r = Some (to_int i)
      ensures not is_valid_size (to_int i) <-> r = None
    *)
  (* r is None if and only if [i] is not a valid size, hence the <-> *)

  val (+): t -> t -> t
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
      ensures forall x y. (+) x y = (+) y x
      ensures forall x y z. (+) x ((+) y z) = (+) ((+) x y) z
      ensures forall x. (+) x 0 = x
    *)
  (* perhaps we shouldn't allow a size of 0 though? nothing is truly zero *)

  val (-): t -> t -> t
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

  val[@logic] invalid: t
  (** [invalid] is a size value that has overflown and is not valid. *)
  (*@ ensures to_int_opt result = None *)
  (*@ ensures forall x. invalid + x = invalid && x + invalid = invalid *)
  (*@ ensures forall x. invalid - x = invalid && x - invalid = invalid *)

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
     operations on them at all. *)

  (*@ function to_int(x: t): int = x *)

  (* this could've been written more simply by changing the left hand interval,
     but it is more explicit this way that -1 is not a valid value *)
  (*@ predicate is_t(i:t) = 0 <= i < invalid_size || i = -1 *)

  let[@logic] to_int_opt i = if i < 0 then None else Some i
  (*@ r = to_int_opt i
      requires is_t i
      pure
      ensures is_valid_size (to_int i)  <-> r = Some (to_int i)
      ensures not is_valid_size (to_int i) <-> r = None
    *)

  let[@logic] invalid = -1
  (*@ ensures to_int_opt result = None && result = -1 *)

  let of_int i = if i < 0 || i >= invalid_size then invalid else i
  (*@ r = of_int i
      pure
      ensures is_t r
      ensures is_valid_size i -> to_int r = i
      ensures not is_valid_size i <-> not is_valid_size (to_int r)
    *)

  let (+) a b =
    if a < 0 || b < 0 then invalid else of_int (a + b)
  (*@ r = (+) a b
      requires is_t a && is_t b
      pure
      ensures is_t r
      ensures is_valid_size (to_int a)
              && is_valid_size (to_int b)
              && is_valid_size (to_int a + to_int b)
              <-> to_int_opt r = Some(to_int a + to_int b)
      ensures not is_valid_size a
              || not is_valid_size b
              || not is_valid_size (a+b) <-> to_int_opt r = None
    *)

  let (-) a b = if a < 0 || b < 0 then -1 else of_int (a - b)
  (*@ r = (-) a b
      requires is_t a && is_t b
      pure
      ensures is_t r
      ensures is_valid_size (to_int a)
              && is_valid_size (to_int b)
              && is_valid_size (to_int a - to_int b)
              <-> to_int_opt r = Some(to_int a - to_int b)
      ensures not is_valid_size a
              || not is_valid_size b
              || not is_valid_size (a-b) <-> to_int_opt r = None
    *)
end
