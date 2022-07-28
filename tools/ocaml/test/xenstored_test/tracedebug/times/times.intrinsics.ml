(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Ocaml_intrinsics.Perfmon

type t = bytes

(* records timestamps using the processor's TSC.
   assumes constant tsc rate independent of CPU frequency adjustments
   and that TSCs across CPUs are synchronized
*)
let record t idx =
  (* this is noalloc, see times.clock.ml *)
  Bytes.set_int64_ne t (8 * idx) (rdtsc ())
  [@@ocaml.inline]

let measure_tsc_to_ns () =
  let t0 = Monotonic_clock.now () in
  let tsc0 = rdtsc () in
  Unix.sleepf 0.2 ;
  (* delay doesn't need to be accurate, time is measured *)
  let t1 = Monotonic_clock.now () in
  let tsc1 = rdtsc () in
  let delta_ns = Int64.sub t1 t0 |> Int64.to_float in
  let delta_tsc = Int64.sub tsc1 tsc0 |> Int64.to_float in
  (* round value to avoid this changing every time the program is run *)
  1e4 /. (1e4 *. delta_tsc /. delta_ns |> Float.round)

let cycle_duration_ns =
  let env = "TRACEDEBUG_TSC_TO_NS" in
  (* ensure children use consistent values for calibration,
     so that timestamps from parents and children are sortable *)
  match Sys.getenv_opt env with
  | None ->
      let r = measure_tsc_to_ns () in
      Unix.putenv env (Printf.sprintf "%h" r) ;
      r
  | Some s ->
      Float.of_string s

let get_as_ns t idx =
  let i = Bytes.get_int64_ne t (8 * idx) in
  if i = 0L then
    None
  else
    Some (Int64.to_float i *. cycle_duration_ns |> Float.round |> Int64.of_float)

let precision = 8 (* ~10-50ns overhead, drop 1 digit *)

let fill t = Bytes.fill t 0 (Bytes.length t) '\x00'

let create n = Bytes.make (n * 8) '\x00'

let id = Printf.sprintf "RDTSC, frequency: %.4f GHz" (1. /. cycle_duration_ns)
