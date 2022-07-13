open Ocaml_intrinsics.Perfmon

let tsc_to_ns =
  let t0 = Monotonic_clock.get_ns () in
  let tsc0 = rdtsc () in
  Unix.sleepf 0.1; (* delay doesn't need to be accurate, time is measured *)
  let t1 = Monotonic_clock.get_ns () in
  let tsc1 = rdtsc () in
  let delta_ns = Int64.sub t1 t0 |> Int64.to_float in
  let delta_tsc = Int64.sub tsc1 tsc0 |> Int64.to_float in
  delta_ns /. delta_tsc

let to_ns i =
  float i *. tsc_to_ns |> Int64.of_float

type t = int array
let record t idx =
  (* this is noalloc *)
  Array.unsafe_set t idx (Int64.to_int (rdtsc ()))
  [@@ocaml.inline]

let precision = 8 (* ~20ns overhead *)

let fill t = Array.fill t 0 (Array.length t) 0
let create n = Array.make n 0
let is_valid idx = idx <> 0
let get t idx = t.(idx)
