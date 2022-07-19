type t = int

let of_int64 i = Int64.to_int i land max_int [@@ocaml.inline]

let to_int64 = Int64.of_int
