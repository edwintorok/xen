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

type t = bytes
(* A bigarray of int64 would allocate when calling the setter,
     even the unsafe variant, so use Bytes.
     Requires 4.08+, but bechamel is only available on 4.08+ already,
   and this module is only built when bechamel is available
   (we use its monotonic clock, which is noalloc, mtime_clock's version currently allocates)
*)

let create n = Bytes.make (n * 8) '\x00'

(* records timestamps using the system-wide monotonic clock *)
let record t i = Bytes.set_int64_ne t (8 * i) (Monotonic_clock.now ())
  [@@ocaml.inline]

let get_as_ns t idx =
  let i = Bytes.get_int64_ne t (8 * idx) in
  if i = 0L then None else Some i

let fill t = Bytes.fill t 0 (Bytes.length t) '\x00'

(* ~600ns-43000ns overhead, drop 2 digits that are completely inaccurate *)
let precision = 7

let id = "monotonic system-wide clock"
