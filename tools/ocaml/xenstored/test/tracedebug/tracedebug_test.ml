open Tracedebug
module StringEvent = Make(StringEvent)(struct
  let limit_log2 = 9
end)

let () =
  for _i = 1 to 10 do
    StringEvent.record "fixed event"
  done;
(* for i = 1 to 10 do
    eventf t (fun () -> string_of_int i)
  done;
  for i = 1 to 10 do
    event1 t string_of_int i
  done;
  dump t print_endline;
  start t;
  dump t print_endline *)
  dump [StringEvent.dump ()] print_endline

