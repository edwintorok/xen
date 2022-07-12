open Tracedebug

let t = create ()

let () =
  for _i = 1 to 10 do
    event t "fixed event"
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
  dump t print_endline

