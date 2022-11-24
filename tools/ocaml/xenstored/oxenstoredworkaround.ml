(* oxenstored gets called twice by the live update process:
     First with --help to check whether live update is supported, and the
     config file is valid. If this fails the original oxenstored stays running,
     so we want to detect as many problems in this mode as possible and fail.

     Second without --help to actually perform the live update, this MUST NOT
     fail, or we are left with no running oxenstored.
     Worst case we just execv() oxenstored without fixing up anything, that
     will still keeps most VMs running (and a subsequent live update can
     attempt to fix the broken VMs, or fixing VMs can also be done via
     INTRODUCE calls)

     This is a single file to simplify building
   	*)
let allow_exit = Array.mem "--help" Sys.argv

let finally () =
    (* don't fork, just replace current program with oxenstored *)
    Unix.execv "/usr/sbin/oxenstored" Sys.argv

module Errors = struct
	(* exception handling *)

	let syslog level = Printf.ksprintf @@ Syslog.log Syslog.Daemon level

	let error = syslog Syslog.Err
	let info = syslog Syslog.Info

	let log_exception (exn, bt) =
		(* if we exit then log as error, otherwise just a warning: we keep going *)
		let level = if allow_exit then Syslog.Warning else Syslog.Err in
    	syslog level "Exception: %s" (Printexc.to_string exn);
    	syslog level "Backtrace: %s" (Printexc.raw_backtrace_to_string bt)

	let uncaught_handler exn bt =
		if allow_exit then log_exception (exn, bt)
		else (* we MUST NOT exit, if anything goes wrong just execv *)
			Fun.protect ~finally @@ fun () -> log_exception (exn, bt)

	let () =
		Printexc.set_uncaught_exception_handler uncaught_handler

	let wrap f v =
    	try Ok (f v)
    	with e ->
    		let exn_bt = e, Printexc.get_raw_backtrace () in
    		log_exception exn_bt;
			Error exn_bt

	let robust_map ~f = ListLabels.rev_map ~f:(wrap f)

	let keep_ok lst =
		(* if we can exit then stop on error, otherwise best effort,
		   discard all the errors and keep just the successful ones *)
		if allow_exit then ListLabels.rev_map ~f:Result.get_ok lst
		else ListLabels.filter_map ~f:Result.to_option lst

end
	(* TODO: GC.compact on success, with safe logging wrapper over regular code *)

(* event channel querying *)
module IntMap = Map.Make(Int)

let query_store_evtchn xc dominfo =
    (* TODO best effort per domain *)
    if dominfo.Xenctrl.hvm_guest then
        let remote_port = Xenctrl.hvm_param_get xc dominfo.Xenctrl.domid Xenctrl.HVM_PARAM_STORE_EVTCHN in
        Some (dominfo.Xenctrl.domid, remote_port)
    else None

    (* TODO: check cmdline args, and if --help is given run in fail fast mode, to reject known problems early
and when not then in failsafe mode, trying to fix up as many domains as possible and keep going, worst just exec into oxenstored without fixing anything
is better than exiting!
     *)
let best_effort_mode = false

let wrap f v =
    try Ok (f v)
    with e ->
        let bt = Printexc.get_raw_backtrace () in
        log_exception Syslog.Warn e bt;
        Error (e, bt)

let handle_best_effort = function
    | Ok r -> Some r
    | Error (e, bt) ->
        if best_effort_mode then None
        else
            Printexc.raise_with_backtrace e bt

let run () =
    (* in best effort mode we try to catch, log and continue on exceptions,
       if we end up with an uncaught exception that is fatal: the system will not have a running oxenstored anymore
     *)
    let level = if best_effort_mode then Syslog.Emerg else Syslog.Warn;
    Printexc.set_uncaught_exception_handler @@ log_exception level;

    info "OXenstored workaround started";
    Xenctrl.with_intf @@ fun xc ->
        let remote2local =
            Xenctrl.domain_getinfolist xc 1
            |> List.to_seq |> Seq.map @@ wrap @@ query_store_evtchn xc
            |> handle_best_effort
            |> IntMap.of_seq
        in ()

let () =
    if best_effort_mode then
        Fun.protect ~finally @@ fun () ->
            try run ()
            with e ->
                log_exception Syslog.Warn e @@ Printexc.get_raw_backtrace ()
    else begin
        (* --help mode, fail early to prevent live update *)
        run ();
        finally ()
    end
