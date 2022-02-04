(* TODO: this should be added by the cameleer wrapper script, and loaded from sys.mli *)
module type Sys = sig
  val[@logic] word_size: int
  (*@ axiom ws: word_size = 32 || word_size = 64 *)
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

module Size = struct

  type t = int
  (* could've been implemented as 'int option', however it is very unlikely
     this would overflow and it is simpler to just operate on regular integers *)

  let[@logic] invalid = Pervasives.max_int / 2
  (*@ ensures result * 2 + 1 = Pervasives.max_int *)

  (*@ predicate is_t(t: integer) = 0 <= t <= invalid *)
  (*@ predicate is_valid_t(t: integer) = is_t t && t <> invalid *)

  (*@ ensures to_int result = None *)

  let[@logic] out_of_bounds i = i < 0 || i >= invalid
  (*@ b = out_of_bounds i
      pure
      ensures is_valid_t i <-> not b *)

  let[@logic] to_int i = if i < invalid then Some i else None
  (*@ r = to_int i
      requires is_t i
      pure
      ensures i = invalid <-> r = None
      ensures is_valid_t i <-> r = Some i
      *)

  let[@logic] of_int i =
    if out_of_bounds i then invalid
    else i
  (*@ r = of_int i
      pure
      ensures is_t r
      ensures is_valid_t i <-> r = i && to_int r = Some i
      ensures not is_valid_t i <-> to_int r = None
      *)

  let of_words w = of_int (w + 1)
  (*@ r = of_words w
      pure
      ensures is_t r
      ensures r = of_int (w+1)
      *)

  let[@logic] words_in_bytes = Sys.word_size / 8
  (*@ ensures 8 * result = Sys.word_size*)

  let of_bytes b =
    if out_of_bounds b then invalid
    else
    of_words ((b + words_in_bytes - 1) / words_in_bytes)
  (*@ w = of_bytes b
      pure
      ensures is_t w
      ensures is_valid_t b -> is_valid_t w
      ensures not is_valid_t b -> w = invalid
      *)

  let (+) a b = of_int (a+b)
  (*@ r = (+) a b
      requires is_t a && is_t b
      pure
      ensures is_t r
      ensures r = of_int (a+b)
   *)

  let (-) a b = of_int (a-b)
  (*@ r = (-) a b
      requires is_t a && is_t b
      pure
      ensures is_t r
      ensures r = of_int (a-b)
    *)

end
