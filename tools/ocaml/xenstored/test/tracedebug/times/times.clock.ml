type t = bytes

(* records timestamps using the system-wide monotonic clock *)
let record t i =
  (* a bigarray of int64 would allocate when calling the setter,
     even the unsafe variant *)
  Bytes.set_int64_ne t (8 * i) (Monotonic_clock.now ())
  [@@ocaml.inline]

let get_as_ns t idx = Bytes.get_int64_ne t (8 * idx)

let fill t = Bytes.fill t 0 (Bytes.length t) '\x00'

let create n = Bytes.make (n * 8) '\x00'

(* ~600ns-43000ns overhead, drop 2 digits that are completely inaccurate *)
let precision = 7

let id = "monotonic system-wide clock"
