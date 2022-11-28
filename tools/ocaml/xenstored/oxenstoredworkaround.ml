(** This is a single-file executable with no external dependencies other than
   Xenctrl and Unix.

   Because the code has to be very robust and recover from errors it is better
   to have everything needed explicitly in this file, except the very low level
   code.
*)

(** whether we should fail early on errors, or whether we have to keep running
	no matter what.

   oxenstored gets called twice by the live update process:

     First with --help to check whether live update is supported, and the
     config file is valid. If this fails the original oxenstored stays running,
     so we want to detect as many problems in this mode as possible and fail.

     Second without --help to actually perform the live update, this MUST NOT
     fail, or we are left with no running oxenstored.
     Worst case we just execv() oxenstored without fixing up anything, that
     will still keeps most VMs running (and a subsequent live update can
     attempt to fix the broken VMs, or fixing VMs can also be done via
     INTRODUCE calls)

     This is a single file to simplify building.

     This program can be interposed between 2 oxenstoreds in a live update
     process by writing its path to `/tool/oxenstored`, e.g.:
     xenstore-write /tool/oxenstored /usr/libexec/xen/bin/oxenstoredworkaround

     The old oxenstored will then invoke this program, which is responsible
     with fixing up and forwarding to the real new oxenstored.
*)
let allow_exit = Array.mem "--help" Sys.argv

(** execv() into '/usr/sbin/oxenstored'

	Does not fork, just replaces the current program with oxenstored.
	Any file descriptors we have stay open as they are.

	Does not call any other functions (e.g. no logging), because this is also
	used as a last resort cleanup function in exception handling.
 *)
let finally () = Unix.execv "/usr/sbin/oxenstored" Sys.argv

(** Logs to syslog.

	oxenstored runs under systemd and stderr goes to /dev/null.
	Changing that is not trivial: using StandardError=journal still goes to
	/dev/null (we don't use the journal, and apparently we don't have Dom0
	properly set up to forward stuff that would be written to the journal to
	syslog).

	Some other value such as a file could be used, but best to use syslog
	directly from our code.

	Note that if logging to syslog fails the only place you'd see that is
	stderr, which is not ideal, but if that fails hopefully it fails early
	before the live update passes the point of no return phase.
*)
module Logging = struct
  (** [syslog level fmt ...] logs the message formatted with the format
		string [fmt] and arguments [...] at [level]

		Note that this may leave a stray file descriptor pointing to syslog
		after we execve, because we don't [closelog], but an extra FD that is
		unused should not cause issues.
		*)
  let syslog level = Printf.ksprintf @@ Syslog.log Syslog.Daemon level

  (** [error fmt ...] logs a formatted error message *)
  let error fmt = syslog Syslog.Err fmt

  (** [info fmt ...] logs a formatted info message *)
  let info fmt = syslog Syslog.Info fmt

  (** [debug fmt ...] logs a formatted debug message *)
  let debug fmt = syslog Syslog.Debug fmt
end

open Logging

(** Exception handling

	It also sets up an uncaught exception handler to ensure exceptions are not
	lost on /dev/null
*)
module Errors = struct
  (** [log_exception context (exn, backtrace)] logs the exception [exn] with
  	  [backtrace], prefixed with the string [context]. *)
  let log_exception context (exn, bt) =
    (* if we exit then log as error, otherwise just a warning: we keep going *)
    let level = if allow_exit then Syslog.Warning else Syslog.Err in
    syslog level "%s exception: %s" context (Printexc.to_string exn) ;
    syslog level "%s backtrace: %s" context (Printexc.raw_backtrace_to_string bt)

  (** [uncaught_handler exn bt] logs the [exn] with [bt]

		In [allow_exit] mode that is all it does, and allows the runtime to
		exit as usual (and if the logger raises any exceptions those will get
		printed to stderr as usual).

		However if we are not allowed to exit then it always calls [finally] to avoid
		exiting the process.
  *)
  let uncaught_handler exn bt =
    if allow_exit then
      log_exception "Uncaught" (exn, bt)
    else (* we MUST NOT exit, if anything goes wrong just execv *)
      Fun.protect ~finally @@ fun () -> log_exception "Uncaught" (exn, bt)

  (** [stay_alive ()] sets up exception and signal handlers

	It ensures that exceptions cannot take down the program if
	[allow_exit = false].
	Note that on OutOfMemory there may not be enough resources available to
	ensure that.

	Ctrl-C is changed to raise an exception (and then usual exception handling
	rules apply from there) instead of taking down the program immediately.

	The signal used by logrotate is ignored (instead of taking down the
	program, which would be the default action).
	There is a small race condition window where a logrotate signal gets
	delivered during live update at the wrong time and kills this process or
	the new oxenstored, but that is unavoidable at this level.

	SIGTERM is translated into an immediate call to [finally], it can be used
	to escape any infinite loops or stuck/long delays this process has got
	caught in.
  *)
  let stay_alive () =
    Printexc.set_uncaught_exception_handler uncaught_handler ;
    (* Don't die on logrotate and SIGPIPE *)
    [Sys.sighup; Sys.sigusr1; Sys.sigpipe]
    |> List.iter @@ fun signal ->
       Sys.set_signal signal Sys.Signal_ignore ;
       Sys.catch_break true ;
       (* use regular exception handling cleanup on SIGINT *)
       Sys.set_signal Sys.sigterm @@ Sys.Signal_handle (fun _ -> finally ())

  (** entrypoint 1 *)
  let () = stay_alive ()

  (** [error_out_of_memory] a preallocated out-of-memory error value,
  	  since we may not have enough resources to actually construct one
   *)
  let error_out_of_memory =
    Error ((Out_of_memory, Printexc.get_callstack 0), None)

  (** [wrap f v] catches any exceptions raised by [f]
  	  and construct an error value containing the exception, the backtrace, and
  	  the input causing the error. On success constructs an [Ok v] value. *)
  let wrap f v =
    try Ok (f v)
    with e -> (
      (* retain input that caused the exception too, for debugging *)
      try Error ((e, Printexc.get_raw_backtrace ()), Some v)
      with Out_of_memory -> error_out_of_memory
    )

  (** [exit_on_error err] logs a static error message and exits with code 1.
  	  It never returns.
   *)
  let exit_on_error =
    Result.iter_error @@ fun _ ->
    error "Exiting on error" ;
    (* we've already logged why *)
    exit 1

  (** [log_and_ignore_exceptions f ()] runs [f ()] and logs any exceptions that escape,
  	  and then ignores them and returs unit.
   *)
  let log_and_ignore_exceptions f () =
    let (_ : (unit, 'a) result) = wrap f () in
    ()

  let wrap_main f =
    if allow_exit then
      wrap f () |> exit_on_error
    else
      Fun.protect ~finally @@ log_and_ignore_exceptions f

  (** [robust_map ~f lst] is like [ListLabels.map ~f], but wraps [f] in a
  	  result type using [wrap], ensuring that we process the entire list even
  	  if some elements fail. *)
  let robust_map ~f = ListLabels.rev_map ~f:(wrap f)

  (** [keep_ok result_lst] given a [result_lst] list of [result] values,
  	  it keeps just the successful ones.

	  It assumes that exceptions have already been logged, so it doesn't log
	  them again.

  	  On [allow_exit] it fails on the first [Error _] it finds,
  	  otherwise it just filters out errors.
   *)
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

  (** [string_of_evtchnstatus status] is a string in the style of [lsevtchn] *)
  let string_of_evtchnstatus =
    let open Xenctrl in
    function
    | EVTCHNSTAT_interdomain {dom; port} ->
        Printf.sprintf "Interdomain (Connected) - Remote Domain %u, Port %u" dom
          port
    | EVTCHNSTAT_unbound dom ->
        Printf.sprintf "Interdomain (Waiting connection) - Remote Domain %u" dom
    | EVTCHNSTAT_pirq pirq ->
        Printf.sprintf "Physical IRQ %u" pirq
    | EVTCHNSTAT_virq virq ->
        Printf.sprintf "Virtual IRQ %u" virq
    | EVTCHNSTAT_ipi ->
        "IPI"

  (** [evtchn xc dominfo] returns xenstore event channel port information for
  	  [dominfo] if possible.

  	  Only works on HVM domains, it is expected that this is NOT called on
  	  Dom0.
  	  Any PV guests other than Dom0 will abort the live update early, or be
  	  ignored in the second phase.
  	*)
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

(** [with_file path openfile closefile f] opens [path] using [openfile], calls
 [f] and always calls [closefile].
 If [closefile] raises (e.g. close_out on NFS) then the exception will be
 wrapped in a FinallyRaised.
 *)
let with_file path openfile closefile f =
  let ch = openfile path in
  let finally () = closefile ch in
  Fun.protect ~finally @@ fun () -> f ch

(** Close-on-exec fixup for running oxenstored.
	The old running oxenstored will have close-on-execute set on the event
	channel file descriptor (because that is what the default was, and there
	wasn't an API at the time to override it).

	There was some last-minute discovery that this may affect Windows guests
	which reset the event channel, but it was not clear at the time this would
	happen all the time on migration. The only reason it did was that we still
	have a certain CA workaround active in xenopsd.
	Also all that discussion was on the private security thread, the archives
	of which got eaten by corporate email retention policy.

	Would've been better if we implemented this evtchn fixup years ago...
 *)
module CloexecFixup = struct
  (* parent oxenstored will be running with the event channel fd open
     CLOEXEC, i.e. it will be closed already when invoking this program.
     Use gdb to change the file descriptor back to keeping it open across
     exec to make live update more reliable (VMs don't notice event channel
     being temporarily unbound that way)
  *)

  (** [dir_files dir] returns the full path to all files under [dir] *)
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

  (** [fixup_cloexec ~ppid fd] removes CLOEXEC from [fd] in [ppid].

	It currently uses [gdb] to do this. There was a pure ptrace based
	[setflags.c], but it was not very reliable: it would not work on
	oxenstored, just on some more simplified test programs.

	Avoid using unreliable code here, since the effect of this fixup not working might be
	that oxenstored gets accidentally killed (e.g. by a stray SIGTRAP).

	Although 'gdb' itself is not guaranteed to be 100% reliable either, it is
	very rare that a gdb crash actually kills the attached process.
   *)
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

  (** fixup_evtchnfd () finds all file descriptors in the parent
  	  that refer to '/dev/xen/evtchn' and fixes them with [fixup_cloexec].
  	  Note that this HAS to be the parent, because a CLOEXEC fd would by
  	  definition have been closed at the time this program executes, so there
  	  would be nothing to fixup in searching in our own open FD list.
   *)
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

(** where oxenstored dumps its "live" state during live update, expecting the
	new oxenstored to load it from here *)
let dbfile = "/var/run/xenstored/db"

(** temporary file storing the modified state,
	this is in the same directory to allow atomic rename
	(a different dir may be on a different FS at which point atomic rename
	would no longer be possible)
 *)
let dbfixup = "/var/run/xenstored/db.fixup"

(** [foreach_line ch f] calls [f] on each line from [f].

	 Stop on [End_of_file] and ignores the exception.*)
let foreach_line ch ~f =
  try
    while true do
      f @@ input_line ch
    done
  with End_of_file -> ()

(** [process_line domains line] processes [line] using the map in [domains].

	If this is a [dom] line and the domid is present in [domains] then the
	information from the [domains] map is used instead of [line].
	Otherwise the line is kept as is.

	Note: the old dom line may contain incorect information here for anything
	except the domid itself in a [dom] line if the guest has rebound its event
	channel. The MFN (an address) is expected to stay the same though, if it is
	different we likely got a bug somewhere (in our bindings?), so warn.
 *)
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

(** [output_line ch line] is like [print_endline] but for [ch] instead of
	[stdout]. *)
let output_line ch line = output_string ch line ; output_char ch '\n'

(** [log_domains_errors lst] returns [lst] unchanged after logging any
	exceptions contained in it, prefixing it with the domain the exception
	applies to.
	This helps debugging, ensuring that any exceptions that escape per-domain
	processing is prefix with the domain id.
	*)
let log_domain_errors lst =
  let () =
    lst
    |> List.iter
       @@ Result.iter_error
       @@ fun (exn_bt, domaininfo_opt) ->
       let context =
         match domaininfo_opt with
         | None ->
             "??"
         | Some domaininfo ->
             Printf.sprintf "Domain %u" domaininfo.Xenctrl.domid
       in
       Errors.log_exception context exn_bt
  in
  lst

(** [check_free_space ()] an approximate check that we have some free space
	available.

	Should be called only when [allow_exit = true]. It uses [fallocate] instead
	of [truncate], because [truncate] allows you to create any file size (as
	long as the size is supported by the underlying FS), even if you don't have
	that much space available, whereas [fallocate] should fail if it can't
	actually allocate that much space.

	"some" is 100MiB here, we don't have a more accurate way of estimating
	space needed (the 1st time we are called [db] doesn't exist yet, and the
	2nd time it is too late, best to go ahead and see whether we get an
	[ENOSPC] for real)
	*)
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

(** entrypoint 2, there MUST be no other toplevel 'let () = ' or side-effecting
	values before this.
	The only exception is [Errors.stay_alive ()] which sets up fallback
	uncaught exception handlers and signal handlers.
 *)
let () =
  Errors.wrap_main @@ fun () ->
  if allow_exit then (
    CloexecFixup.fixup_evtchnfd () ;
    (* can only attempt fixup 1st time, when
       we get exec-ed for real the 2nd time it is too late - the fd is already
       closed, and there is no "parent" oxenstored anymore (our parent if any will
       be systemd) *)
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
