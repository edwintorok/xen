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

open Bechamel
open Tracedebug

let times = Tracedebug_times.Times.create 1

let times_record () = Tracedebug_times.Times.record times 0

let t = create StringEvent.empty

let tracedebug_record () = record t "fixed event"
let tracedebug_trace () =
  trace t "formatting an integer: %d" 4

let tracedebug_trace_off () =
  stop t;
  tracedebug_trace ();
  start t

let benchmarks =
  Test.make_grouped ~name:"tracedebug"
    [
      Test.make ~name:"Times.record" (Staged.stage times_record)
    ; Test.make ~name:"Tracedebug.GcEvent.get" (Staged.stage GcEvent.get)
    ; Test.make ~name:"Tracedebug.record" (Staged.stage tracedebug_record)
    ; Test.make ~name:"Tracedebug.trace" (Staged.stage tracedebug_trace)
    ; Test.make ~name:"Tracedebug.trace off" (Staged.stage tracedebug_trace_off)
    ; Test.make_indexed_with_resource ~name:"Tracedebug.recordf"
        ~args:[1; 9; 13]
        Test.multiple (* TODO: Test.uniq segfaults here, bechamel bug *)
        ~allocate:(fun i -> create ~limit_log2:i StringEvent.empty)
        ~free:ignore (* using stop t on free would segfault, bechamel bug *)
        (fun i -> Staged.stage @@ fun t -> recordf t (fun () -> string_of_int i))
    ]

let () =
  Bechamel_simple_cli.cli benchmarks ;
  let gc = register_gc () in
  Tracedebug.pp_events Format.err_formatter [dump GcEvent.pp gc]
