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
