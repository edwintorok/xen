(** [get_ns ()] returns a monotonic system-wide timestamp in nanoseconds.
    This function doesn't allocate.
 *)
external get_ns: unit -> (int64 [@unboxed]) = "stub_monotonic_clock_get_ns_boxed" "stub_monotonic_clock_get_ns_unboxed" [@@noalloc]

let nsec_per_s = 1_000_000_000L
(** [nsec_per_s] is 1e9ns = 1s *)

(** [to_sec_ns ns] splits a nanosecond timestamp into a seconds and nanoseconds part *)
let to_sec_ns i =
  Int64.unsigned_div i nsec_per_s, Int64.unsigned_rem i nsec_per_s
