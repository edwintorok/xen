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

module TraceStr = Make (StringEvent) (struct let limit_log2 = 9 end)

let times = Tracedebug_times.Times.create 1

let times_record () = Tracedebug_times.Times.record times 0

let tracedebug_record () = TraceStr.record "fixed event"

module type Tracer = module type of TraceStr

let benchmarks =
  Test.make_grouped ~name:"tracedebug"
    [
      Test.make ~name:"Times.record" (Staged.stage times_record)
    ; Test.make ~name:"Tracedebug.GcEvent.get" (Staged.stage GcEvent.get)
    ; Test.make ~name:"Tracedebug.record" (Staged.stage tracedebug_record)
    ; Test.make_indexed_with_resource ~name:"Tracedebug.recordf"
        ~args:[1; 9; 13]
        Test.multiple (* TODO: Test.uniq segfaults here, bechamel bug *)
        ~allocate:(fun i : (module Tracer) ->
          (module Make (StringEvent) (struct let limit_log2 = i end))
        )
        ~free:ignore
        (fun i ->
          Staged.stage @@ fun (module T : Tracer) ->
          T.recordf (fun () -> string_of_int i)
        )
    ]

let () =
  Bechamel_simple_cli.cli benchmarks ;
  let dump_gc = register_gc ~limit_log2:9 () in
  Tracedebug.dump Format.err_formatter [dump_gc ()]
