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
  (* don't call any other functions, even logging functions here,
     assume that anything could fail *)
  Unix.execv "/usr/sbin/oxenstored" Sys.argv

let syslog level = Printf.ksprintf @@ Syslog.log Syslog.Daemon level

let error fmt = syslog Syslog.Err fmt

let info fmt = syslog Syslog.Info fmt

let debug fmt = syslog Syslog.Debug fmt

module Errors = struct
  (* exception handling *)

  let log_exception context (exn, bt) =
    (* if we exit then log as error, otherwise just a warning: we keep going *)
    let level = if allow_exit then Syslog.Warning else Syslog.Err in
    syslog level "%s exception: %s" context (Printexc.to_string exn) ;
    syslog level "%s backtrace: %s" context (Printexc.raw_backtrace_to_string bt)

  let uncaught_handler exn bt =
    if allow_exit then
      log_exception "Uncaught" (exn, bt)
    else (* we MUST NOT exit, if anything goes wrong just execv *)
      Fun.protect ~finally @@ fun () -> log_exception "Uncaught" (exn, bt)

  let () =
    Printexc.set_uncaught_exception_handler uncaught_handler ;
    Sys.catch_break true ;
    (* use regular exception handling cleanup on SIGINT *)
    Sys.set_signal Sys.sigterm @@ Sys.Signal_handle (fun _ -> finally ()) ;
    (* Don't die on logrotate and SIGPIPE *)
    [Sys.sighup; Sys.sigusr1; Sys.sigpipe]
    |> List.iter @@ fun signal -> Sys.set_signal signal Sys.Signal_ignore

  let wrap f v =
    try Ok (f v)
    with e ->
      (* retain input that caused the exception too, for debugging *)
      Error ((e, Printexc.get_raw_backtrace ()), v)

  let exit_on_error =
    Result.iter_error @@ fun _ ->
    error "Exiting on error" ;
    (* we've already logged why *)
    exit 1

  let log_exceptions f () =
    let (_ : (unit, 'a) result) = wrap f () in
    ()

  let wrap_main f =
    if allow_exit then
      wrap f () |> exit_on_error
    else
      Fun.protect ~finally @@ log_exceptions f

  let robust_map ~f = ListLabels.rev_map ~f:(wrap f)

  let keep_ok lst =
    (* if we can exit then stop on error, otherwise best effort,
       discard all the errors and keep just the successful ones *)
    if allow_exit then
      ListLabels.rev_map ~f:Result.get_ok lst
    else
      ListLabels.filter_map ~f:Result.to_option lst
end

module Query = struct
  (* event channel querying *)
  module IntMap = Map.Make (Int)

  type t = {remote_port: int; local_port: int; mfn: nativeint}

  let string_of_evtchnstatus =
    let open Xenctrl in
    function
    | EVTCHNSTAT_interdomain {dom; port} ->
        Printf.sprintf "Interdomain (Connected) - Remote Domain %u, Port %use"
          dom port
    | EVTCHNSTAT_unbound dom ->
        Printf.sprintf "Interdomain (Waiting connection) - Remote Domain %u" dom
    | EVTCHNSTAT_pirq pirq ->
        Printf.sprintf "Physical IRQ %u" pirq
    | EVTCHNSTAT_virq virq ->
        Printf.sprintf "Virtual IRQ %u" virq
    | EVTCHNSTAT_ipi ->
        "IPI"

  let evtchn xc dominfo =
    if not dominfo.Xenctrl.hvm_guest then
      failwith "Only HVM guests are supported for live update fixup" ;
    let domid = dominfo.Xenctrl.domid in
    debug "Querying HVM params for domain %u" domid ;
    let hvm_param_get = Xenctrl.hvm_param_get xc domid in
    let remote_port =
      hvm_param_get Xenctrl.HVM_PARAM_STORE_EVTCHN |> Int64.to_int
    in
    let mfn = hvm_param_get Xenctrl.HVM_PARAM_STORE_PFN in
    debug "Querying evtchn status for domain %u, remote port %u" domid
      remote_port ;
    match Xenctrl.evtchn_status xc domid remote_port with
    | None ->
        failwith "Domain xenstore evtchn port is closed"
    | Some {status= EVTCHNSTAT_interdomain {dom= 0; port= local_port}; _} ->
        (domid, {remote_port; local_port; mfn= Int64.to_nativeint mfn})
    | Some {status= EVTCHNSTAT_virq 3; _} (* 3 is DOM_EXC *) when domid = 0 ->
        (domid, {remote_port; local_port= remote_port; mfn= 0n})
    | Some {status; vcpu} ->
        error "Domain %u, evtchn %u has unexpected state: %s on vcpu %u" domid
          remote_port
          (string_of_evtchnstatus status)
          vcpu ;
        failwith "Unexpected remote evtchn state"
end

let with_file path openfile closefile f =
  let ch = openfile path in
  let finally () = closefile ch in
  Fun.protect ~finally @@ fun () -> f ch

module CloexecFixup = struct
  (* parent oxenstored will be running with the event channel fd open
     CLOEXEC, i.e. it will be closed already when invoking this program.
     Use gdb to change the file descriptor back to keeping it open across
     exec to make live update more reliable (VMs don't notice event channel
     being temporarily unbound that way)
  *)

  let dir_files dirpath =
    with_file dirpath Unix.opendir Unix.closedir @@ fun dir ->
    let files = ref [] in
    try
      while true do
        let name = Unix.readdir dir in
        if name <> "." && name <> ".." then
          files := Filename.concat dirpath name :: !files
      done ;
      assert false
    with End_of_file -> !files

  let fixup_cloexec ~ppid fd =
    debug "Attemping to fix up close-on-exec flag on PID %d FD %d" ppid fd ;
    let f_setfd = 2 in
    (* we could attempt to read flags, remove cloexec and set, but on an
       actual system this will always be '1'  or '0' - i.e. cloexec present
       or absent, so setting to 0 suffices *)
    let args =
      [
        "set logging file /var/run/xenstored/gdb.log"
      ; "set logging enabled on"
      ; Printf.sprintf "call fcntl(%u, %u, 0)" fd f_setfd
      ; "quit"
      ]
      |> List.concat_map (fun s -> ["-ex"; s])
      |> List.append ["-p"; string_of_int ppid; "-batch"]
    in
    let cmd = Filename.quote_command ~stdin:"/dev/null" "gdb" args in
    debug "Executing %s" cmd ;
    let rc = Sys.command cmd in
    if rc <> 0 then
      failwith
        (Printf.sprintf
           "Failed to fixup close-on-exec on pid %u, gdb exited with code %d"
           ppid rc
        )

  let fixup_evtchnfd () =
    let ppid = Unix.getppid () in
    Printf.sprintf "/proc/%u/fd" ppid
    |> dir_files
    |> ( List.filter_map @@ fun file ->
         let fd = int_of_string @@ Filename.basename file in
         match Errors.wrap Unix.readlink file with
         | Ok "/dev/xen/evtchn" ->
             Some fd
         | Ok other ->
             debug "Ignoring fd %d, not evtchn: %s" fd other ;
             None
         | Error (exnbt, _) ->
             Errors.log_exception "readlink(ignored)" exnbt ;
             None
       )
    |> List.iter (fixup_cloexec ~ppid)
end

let dbfile = "/var/run/xenstored/db"

let dbfixup = "/var/run/xenstored/db.fixup"

let foreach_line ch ~f =
  try
    while true do
      f @@ input_line ch
    done
  with End_of_file -> ()

let process_line domains line =
  match String.split_on_char ',' @@ String.trim line with
  | ["dom"; domid; mfn; _remote_port] ->
      let domid = int_of_string domid and mfn = Nativeint.of_string mfn in
      debug "Replacing dom line: %s" line ;
      let replacement = Query.IntMap.find domid domains in
      if replacement.Query.mfn <> mfn then
        syslog Syslog.Warning "MFN for domid doesn't match: %nx vs %nx"
          replacement.Query.mfn mfn ;
      let replacement =
        Printf.sprintf "dom,%u,%nd,%d,%d" domid mfn replacement.remote_port
          replacement.local_port
      in
      debug "Replacement line: %s" line ;
      replacement
  | _ ->
      line
(* keep everything else unmodified *)

let output_line ch line = output_string ch line ; output_char ch '\n'

let log_domain_errors lst =
  let () =
    lst
    |> List.iter
       @@ Result.iter_error
       @@ fun (exn_bt, domaininfo) ->
       let context = Printf.sprintf "Domain %u" domaininfo.Xenctrl.domid in
       Errors.log_exception context exn_bt
  in
  lst

let check_free_space () =
  (* live update should safely abort on ENOSPC, however we also need to
     modify it and create backups,
     check that we have some space available, although for a very large
     xenstore dump this may not be enough.
     In that case we'll fall back to not fixing it up and the user can retry
     after more space is available.
  *)
  let rc =
    Sys.command (Printf.sprintf "/usr/bin/fallocate -l 100MB %s" dbfixup)
  in
  if rc <> 0 then
    failwith
      (Printf.sprintf
         "%s: fallocate exited with code %d, not enough free space?" dbfixup rc
      ) ;
  Unix.unlink dbfixup

let () =
  Errors.wrap_main @@ fun () ->
  if allow_exit then (
    CloexecFixup.fixup_evtchnfd () ;
    check_free_space ()
  ) ;
  Xenctrl.with_intf @@ fun xc ->
  let domains =
    Xenctrl.domain_getinfolist xc 1
    |> Errors.robust_map ~f:(Query.evtchn xc)
    |> log_domain_errors
    |> Errors.keep_ok
    |> List.to_seq
    |> Query.IntMap.of_seq
  in
  if Sys.file_exists dbfile then debug "Opening %s" dbfile ;
  let () =
    let backupfile =
      Printf.sprintf "%s.backup.%.9f" dbfile @@ Unix.gettimeofday ()
    in
    with_file dbfile open_in close_in_noerr @@ fun chin ->
    debug "Writing to %s" dbfixup ;
    with_file dbfixup open_out close_out @@ fun chout ->
    with_file backupfile open_out close_out @@ fun backupch ->
    let f line =
      output_line backupch line ;
      line |> process_line domains |> output_line chout
    in
    foreach_line chin ~f
  in
  debug "Renaming %s to final %s" dbfixup dbfile ;
  Unix.rename dbfixup dbfile
