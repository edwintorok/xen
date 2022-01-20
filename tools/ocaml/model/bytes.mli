(*@ open Stdlib *)
type t = bytes
(*@ model length: integer
    invariant 0 <= length *)

(*@ function length(s: bytes) : integer  = s.length *)
