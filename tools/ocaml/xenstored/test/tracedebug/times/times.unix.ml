type t = Float.Array.t

(* caller ensures idx is within bounds by applying a mask based on ring size *)
let record t i =
  Float.Array.unsafe_set t i (Unix.gettimeofday ())

let fill t = Float.Array.fill t 0 (Float.Array.length t) Float.nan

let create n =
  Float.Array.make n Float.nan

let is_valid t = not (Float.is_nan t)
let get = Float.Array.get (* not perf critical *)

let precision = 6
(* gettimeofday returns time in [us], not [ns],
   actual precision ~1us - 100us *)

let to_ns f =
  (* more precise if we leave the final multiplication as integer *)
  f *. 1e6 |> Float.round |> Int64.of_float |> Int64.mul 1000L

let id = "gettimeofday"
