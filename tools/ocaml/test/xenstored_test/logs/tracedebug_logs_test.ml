let () = Tracedebug_logs.dump_at_exit (fun () -> [])

let () = Logs.debug (fun m -> m "a logs message in the ring: %d" 4)
