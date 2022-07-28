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

open Tracedebug

let () =
  let t1 = create ~limit_log2:1 StringEvent.empty in
  let t2 = create StringEvent.empty in
  let gc = register_gc () in
  for _i = 1 to 10 do
    record t1 "fixed event"
  done ;
  for i = 1 to 10 do
    recordf t1 (fun () -> string_of_int i) ;
    record t2 ("ith event " ^ string_of_int i)
  done ;
  Gc.full_major () ;
  (* TODO: check that the results are correct! distinct events, etc *)
  pp_events Format.err_formatter
    [dump StringEvent.pp t1; dump StringEvent.pp t2; dump GcEvent.pp gc]
