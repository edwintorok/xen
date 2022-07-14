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
    ; Test.make ~name:"Tracedebug.record" (Staged.stage tracedebug_record)
    ; Test.make_indexed_with_resource ~name:"Tracedebug.recordf"
        ~args:[1; 9; 13] Test.uniq
        ~allocate:(fun i : (module Tracer) ->
          (module Make (StringEvent) (struct let limit_log2 = i end))
        )
        ~free:ignore
        (fun i ->
          Staged.stage @@ fun (module T : Tracer) ->
          T.recordf (fun () -> string_of_int i)
        )
    ]

let () = Bechamel_simple_cli.cli benchmarks
