open Tracer_core

let uint62_max = Uint62.of_int64 (-1L) |> Uint62.to_int64

let check_roundtrip i64 =
  assert (0L <= i64 && i64 <= uint62_max) ;
  let r64 = Uint62.to_int64 (Uint62.of_int64 i64) in
  assert (r64 = i64)

let check_masked_negative i64 =
  let int63 = Int64.add uint62_max 1L in
  let r1 = Uint62.to_int64 (Uint62.of_int64 i64)
  and r2 = Uint62.to_int64 (Uint62.of_int64 (Int64.add int63 i64)) in
  assert (r1 = r2)

let test () =
  assert (uint62_max = Int64.sub (Int64.shift_left 1L 62) 1L) ;
  List.iter check_roundtrip
    [
      0L
    ; 1L
    ; Int64.(shift_right_logical uint62_max 2)
    ; uint62_max
    ; Int64.of_int max_int
    ] ;
  List.iter check_masked_negative
    [
      Int64.min_int
    ; -1L
    ; 0L
    ; 1L
    ; Int64.(shift_right_logical uint62_max 2)
    ; uint62_max
    ; Int64.of_int max_int
    ]
