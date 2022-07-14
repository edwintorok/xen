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

type t = Float.Array.t

let create n = Float.Array.make n Float.nan

let record t i =
  (* we could use unsafe_set here, but the Unix call is slow enough that
     it wouldn't matter, use the regular variant to catch bugs *)
  Float.Array.set t i (Unix.gettimeofday ())

let get_as_ns t i =
  (* more precise if we leave the final multiplication as integer *)
  let f = Float.Array.get t i in
  if Float.is_nan f then
    None
  else
    Some (f *. 1e6 |> Float.round |> Int64.of_float |> Int64.mul 1000L)

let precision = 6
(* gettimeofday returns time in [us], not [ns],
   actual precision ~1us - 100us *)

let fill t = Float.Array.fill t 0 (Float.Array.length t) Float.nan

let id = "gettimeofday"
