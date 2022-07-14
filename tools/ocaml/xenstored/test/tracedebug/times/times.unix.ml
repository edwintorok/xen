type t = Float.Array.t

(* caller ensures idx is within bounds by applying a mask based on ring size *)
let record t i = Float.Array.unsafe_set t i (Unix.gettimeofday ())

let get_as_ns t i =
  (* more precise if we leave the final multiplication as integer *)
  let f = Float.Array.get t i in
  if Float.is_nan f then
    0L
  else
    f *. 1e6 |> Float.round |> Int64.of_float |> Int64.mul 1000L

let fill t = Float.Array.fill t 0 (Float.Array.length t) Float.nan

let create n = Float.Array.make n Float.nan

let precision = 6
(* gettimeofday returns time in [us], not [ns],
   actual precision ~1us - 100us *)

let id = "gettimeofday"
