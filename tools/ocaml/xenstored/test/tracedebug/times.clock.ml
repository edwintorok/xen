type t = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

(* caller ensures idx is within bounds by applying a mask based on ring size *)
external record: t -> int -> unit = "stub_clock_record" [@@noalloc]

let fill t = Bigarray.Array1.fill t 0L
let create n =
  let t = Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout n in
  fill t;
  t
let is_valid idx = idx <> 0L
let get t idx =
  Bigarray.Array1.get t idx

let to_ns i = i
