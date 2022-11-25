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

  let log_exception (exn, bt) =
    (* if we exit then log as error, otherwise just a warning: we keep going *)
    let level = if allow_exit then Syslog.Warning else Syslog.Err in
    syslog level "Exception: %s" (Printexc.to_string exn);
    syslog level "Backtrace: %s" (Printexc.raw_backtrace_to_string bt)

  let uncaught_handler exn bt =
    if allow_exit then log_exception (exn, bt)
    else
      (* we MUST NOT exit, if anything goes wrong just execv *)
      Fun.protect ~finally @@ fun () -> log_exception (exn, bt)

  let () =
    Printexc.set_uncaught_exception_handler uncaught_handler;
    Sys.catch_break true;
    (* use regular exception handling cleanup on SIGINT *)
    Sys.set_signal Sys.sigterm @@ Sys.Signal_handle (fun _ -> finally ());
    (* Don't die on logrotate and SIGPIPE *)
    [ Sys.sighup; Sys.sigusr1; Sys.sigpipe ]
    |> List.iter @@ fun signal -> Sys.set_signal signal Sys.Signal_ignore

  let wrap f v =
    try Ok (f v)
    with e ->
      let exn_bt = (e, Printexc.get_raw_backtrace ()) in
      log_exception exn_bt;
      Error exn_bt

  let exit_on_error =
    Result.iter_error @@ fun _ ->
    error "Exiting on error";
    (* we've already logged why *)
    exit 1

  let log_exceptions f () =
    let (_ : (unit, 'a) result) = wrap f () in
    ()

  let wrap_main f =
    if allow_exit then wrap f () |> exit_on_error
    else Fun.protect ~finally @@ log_exceptions f

  let robust_map ~f = ListLabels.rev_map ~f:(wrap f)

  let keep_ok lst =
    (* if we can exit then stop on error, otherwise best effort,
       discard all the errors and keep just the successful ones *)
    if allow_exit then ListLabels.rev_map ~f:Result.get_ok lst
    else ListLabels.filter_map ~f:Result.to_option lst
end

module Query = struct
  (* event channel querying *)
  module IntMap = Map.Make (Int)

  type t = { remote_port : int; local_port : int; mfn : nativeint }

  let string_of_evtchnstatus =
    let open Xenctrl in
    function
    | EVTCHNSTAT_interdomain { dom; port } ->
        Printf.sprintf "Interdomain (Connected) - Remote Domain %u, Port %use"
          dom port
    | EVTCHNSTAT_unbound dom ->
        Printf.sprintf "Interdomain (Waiting connection) - Remote Domain %u" dom
    | EVTCHNSTAT_pirq pirq -> Printf.sprintf "Physical IRQ %u" pirq
    | EVTCHNSTAT_virq virq -> Printf.sprintf "Virtual IRQ %u" virq
    | EVTCHNSTAT_ipi -> "IPI"

  let evtchn xc dominfo =
    if not dominfo.Xenctrl.hvm_guest then
      failwith "Only HVM guests are supported for live update fixup";
    let domid = dominfo.Xenctrl.domid in
    debug "Querying HVM params for domain %u" domid;
    let hvm_param_get = Xenctrl.hvm_param_get xc domid in
    let remote_port =
      hvm_param_get Xenctrl.HVM_PARAM_STORE_EVTCHN |> Int64.to_int
    in
    let mfn = hvm_param_get Xenctrl.HVM_PARAM_STORE_PFN in
    debug "Querying evtchn status for domain %u, remote port %u" domid
      remote_port;
    match Xenctrl.evtchn_status xc domid remote_port with
    | None -> failwith "Domain xenstore evtchn port is closed"
    | Some { status = EVTCHNSTAT_interdomain { dom = 0; port = local_port }; _ }
      ->
        (domid, { remote_port; local_port; mfn = Int64.to_nativeint mfn })
    | Some { status = EVTCHNSTAT_virq 3; _ } (* 3 is DOM_EXC *) when domid = 0
      ->
        (domid, { remote_port; local_port = remote_port; mfn = 0n })
    | Some { status; vcpu } ->
        error "Domain %u, evtchn %u has unexpected state: %s on vcpu %u" domid
          remote_port
          (string_of_evtchnstatus status)
          vcpu;
        failwith "Unexpected remote evtchn state"
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
  | [ "dom"; domid; mfn; _remote_port ] ->
      let domid = int_of_string domid and mfn = Nativeint.of_string mfn in
      debug "Replacing dom line: %s" line;
      let replacement = Query.IntMap.find domid domains in
      if replacement.Query.mfn <> mfn then
        syslog Syslog.Warning "MFN for domid doesn't match: %nx vs %nx"
          replacement.Query.mfn mfn;
      let replacement =
        Printf.sprintf "dom,%u,%nd,%d,%d" domid mfn replacement.remote_port
          replacement.local_port
      in
      debug "Replacement line: %s" line;
      replacement
  | _ -> line
(* keep everything else unmodified *)

let with_file path openfile closefile f =
  let ch = openfile path in
  let finally () = closefile ch in
  Fun.protect ~finally @@ fun () -> f ch

let output_line ch line =
  output_string ch line;
  output_char ch '\n'

let () =
  Errors.wrap_main @@ fun () ->
  Xenctrl.with_intf @@ fun xc ->
  (* TODO: wrap errors with the domid for better troubleshooting *)
  let domains =
    Xenctrl.domain_getinfolist xc 1
    |> Errors.robust_map ~f:(Query.evtchn xc)
    |> Errors.keep_ok |> List.to_seq |> Query.IntMap.of_seq
  in
  if Sys.file_exists dbfile then debug "Opening %s" dbfile;
  let () =
    with_file dbfile open_in close_in_noerr @@ fun chin ->
    debug "Writing to %s" dbfixup;
    with_file dbfixup open_out close_out @@ fun chout ->
    let f line = line |> process_line domains |> output_line chout in
    foreach_line chin ~f
  in
  debug "Renaming %s to final %s" dbfixup dbfile;
  Unix.rename dbfixup dbfile
