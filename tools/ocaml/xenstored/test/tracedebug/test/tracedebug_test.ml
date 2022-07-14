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

module StringTrace = Make (StringEvent) (struct let limit_log2 = 1 end)

module StringTrace2 = Make (StringEvent) (struct let limit_log2 = 9 end)

let () =
  let dump_gc = register_gc () in
  for _i = 1 to 10 do
    StringTrace2.record "fixed event"
  done ;
  for i = 1 to 10 do
    StringTrace.recordf (fun () -> string_of_int i) ;
    StringTrace2.record "ith event"
  done ;
  Gc.full_major () ;
  dump Format.err_formatter
    [StringTrace.dump (); StringTrace2.dump (); dump_gc ()]
