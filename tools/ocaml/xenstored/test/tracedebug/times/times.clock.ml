type t = bytes

let record t i =
  (* a bigarray of int64 would allocate when calling the setter,
     even the unsafe variant *)
  Bytes.set_int64_ne t (8*i) (Monotonic_clock.get_ns ())
  [@@ocaml.inline]

let fill t = Bytes.fill t 0 (Bytes.length t) '\x00'
let create n = Bytes.make (n*8) '\x00'
let get t idx =
  Bytes.get_int64_ne t (8*idx)

let is_valid idx = idx <> 0L

let delta =
  let t0 = Monotonic_clock.get_ns () in
  let t1 = Monotonic_clock.get_ns () in
  Int64.sub t1 t0

let () = Printf.eprintf "E:::%Lu\n" delta

let to_ns i = i

(* ~600ns overhead, drop 2 digits that are completely inaccurate *)
let precision = 7
