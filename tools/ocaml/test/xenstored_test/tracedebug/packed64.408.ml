(* this module is for compat with OCaml < 4.08,
   float array is always in packed form there *)

type _ t = bytes

let invalid = '\x00'

let create n = Bytes.make (n * 8) invalid

let fill t = Bytes.fill t 0 (Bytes.length t) invalid

let set_int64 t i i64 = Bytes.set_int64_ne t (8 * i) i64 [@@ocaml.inline]

let set_float t i f = set_int64 t i (Int64.bits_of_float f) [@@ocaml.inline]

let get_int64 t i = Bytes.get_int64_ne t (8 * i)

let get_float t i = Int64.float_of_bits (get_int64 t i)
