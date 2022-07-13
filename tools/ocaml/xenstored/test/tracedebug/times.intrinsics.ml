open Ocaml_intrinsics.Perfmon

let calibrate _ =
  let t0 = Unix.gettimeofday () in
  let tsc0 = rdtsc () in
  Unix.sleepf 0.01;
  let t1 = Unix.gettimeofday () in
  let tsc1 = rdtsc () in
  (t1 -. t0) /. Int64.(sub tsc1 tsc0 |> to_float)

let tsc_min_freq, tsc_max_freq, tsc_to_s =
  let a = Array.init 10 calibrate in
  1. /. Array.fold_left Float.max Float.min_float a,
  1. /. Array.fold_left Float.min Float.max_float a,
  Array.fold_left (+.) 0. a /. float (Array.length a)

let to_s i =
  float i *. tsc_to_s

type t = int array
let record t idx =
  (* this is noalloc *)
  Array.unsafe_set t idx (Int64.to_int (rdtsc ()))
  [@@ocaml.inline]

let fill t = Array.fill t 0 (Array.length t) 0
let create n = Array.make n 0
let is_valid idx = idx <> 0
let get t idx = t.(idx)
