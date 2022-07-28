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

let tracedebug_logs () = Logs.debug (fun m -> m "formatting an integer: %d" 4)

let benchmarks =
  Test.make_grouped ~name:"tracedebug"
    [
      Test.make ~name:"Times.record" (Staged.stage times_record)
    ; Test.make ~name:"Tracedebug.GcEvent.get" (Staged.stage GcEvent.get)
    ; Test.make ~name:"Tracedebug.record" (Staged.stage tracedebug_record)
    ; Test.make ~name:"Tracedebug.logs" (Staged.stage tracedebug_logs)
    ; Test.make_indexed_with_resource ~name:"Tracedebug.recordf"
        ~args:[1; 9; 13]
        Test.multiple (* TODO: Test.uniq segfaults here, bechamel bug *)
        ~allocate:(fun i -> create ~limit_log2:i StringEvent.empty)
        ~free:ignore (* using stop t on free would segfault, bechamel bug *)
        (fun i -> Staged.stage @@ fun t -> recordf t (fun () -> string_of_int i))
    ]

let null_formatter =
  Format.formatter_of_out_functions
    {
      Format.out_flush= ignore
    ; Format.out_newline= ignore
    ; Format.out_indent= ignore
    ; Format.out_spaces= ignore
    ; Format.out_string= (fun _ _ _ -> ())
    }

let () =
  Tracedebug_logs.dump_at_exit ~dst:null_formatter ~limit_log2:9 (fun () -> []) ;
  Bechamel_simple_cli.cli benchmarks
