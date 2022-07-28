(* this module is for compat with OCaml < 4.08,
   float array is always in packed form there *)

type _ t = float array

let invalid = 0.

let create n = Array.make n invalid

let fill t = Array.fill t 0 (Array.length t) invalid

let set_float = Array.unsafe_set

let set_int64 t i i64 = set_float t i (Int64.float_of_bits i64) [@@ocaml.inline]

let get_float t i = t.(i)

let get_int64 t i = Int64.bits_of_float (get_float t i)
