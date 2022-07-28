(* compat module for OCaml < 4.08, implements only functions we use *)

module Array = struct
  type t = float array

  let set = Array.set

  let fill = Array.fill

  let get = Array.get

  let length = Array.length

  let make = Array.make
end

type t = float

let is_nan f = classify_float f = FP_nan

let nan = nan

let round f =
  (* an approximation, doesn't handle all the corner cases like Float.round does *)
  if f > 0. then
    floor (f +. 0.5)
  else
    ceil (f -. 0.5)
