let reporter ?(app = Format.std_formatter) ?limit_log2 () =
  let empty _ = () in
  let t = Tracedebug.create ?limit_log2 empty in
  let report _src level ~over k msgf =
    let k delayed_formatter =
      if level = Logs.App then
        delayed_formatter app
      else
        Tracedebug.record t delayed_formatter ;
      over () ;
      k ()
    in
    msgf @@ fun ?header ?tags:_ fmt ->
    Format.kdprintf k
      ("%a@[" ^^ fmt ^^ "@]@.")
      Logs_fmt.pp_header (level, header)
  in
  let process formatter delayed_formatter = delayed_formatter formatter in
  let dump () = Tracedebug.dump process t in
  ({Logs.report}, dump)

let dump_at_exit ?app ?(dst = Format.err_formatter) ?(limit_log2 = 16) others =
  Logs.set_level (Some Logs.Debug) ;
  let reporter, dump = reporter ?app ~limit_log2 () in
  Logs.set_reporter reporter ;
  at_exit (fun () ->
      Tracedebug.pp_events dst (dump () :: others ()) ;
      Format.pp_print_newline dst ()
  )
