type t = Float.Array.t

(* caller ensures idx is within bounds by applying a mask based on ring size *)
let record t i =
  Float.Array.unsafe_set t i (Unix.gettimeofday ())

let fill t = Float.Array.fill t 0 (Float.Array.length t) Float.nan

let create n =
  Float.Array.make n Float.nan

let is_valid t = not (Float.is_nan t)
let get = Float.Array.get (* not perf critical *)

let to_ns f =
  (* multiplying by 1e9 would be less precise: nanoseconds would sometimes be nonzero.
     Perform the multiplication by 1000 as an integer instead *)
  f *. 1e6 |> Float.round |> Int64.of_float |> Int64.mul 1000L
