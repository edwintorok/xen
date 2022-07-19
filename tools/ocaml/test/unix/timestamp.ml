type t = Uint62.t

let now () =
  (* drop bottom 2 bits from the mantissa *)
  let i64 = Unix.gettimeofday () |> Int64.bits_of_float in
  Int64.shift_right_logical i64 2 |> Uint62.of_int64

let to_epoch_ns t =
  let frac, integral =
    Int64.shift_left (Uint62.to_int64 t) 2 |> Int64.float_of_bits |> modf
  in
  let s = Int64.(of_float integral)
  and ns = Int64.of_float ((frac *. 1e6) +. 0.5) in
  Int64.add Int64.(mul 1_000_000_000L s) ns

let sub_ns ~t1:_ ~t0:_ = None

let id = "gettimeofday"
