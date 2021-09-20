let is_output_devnull = Unix.stat "/dev/null" = Unix.fstat Unix.stdout

let () =
  if not is_output_devnull then (
    Printexc.record_backtrace true ;
    Fmt_tty.setup_std_outputs () ;
    try
      let cols =
        let ch = Unix.open_process_in "tput cols" in
        Stdext.finally
          (fun () -> input_line ch |> int_of_string)
          (fun () -> Unix.close_process_in ch)
      in
      Format.set_margin cols
    with _ -> () )

let devnull () = Unix.openfile "/dev/null" [] 0

let xb = Xenbus.Xb.open_fd (devnull ())

module Command = struct
  type path = Store.Path.t

  type value = string

  type token = string

  type domid = int

  type t = Xenbus.Packet.t

  open Xenstore.Queueop

  let cmd f =
    Queue.clear xb.pkt_out ;
    let () = f xb in
    let p = Xenbus.Xb.peek_output xb in
    Queue.clear xb.pkt_out ; p

  let pathcmd f pathgen tid state = cmd @@ f tid @@ pathgen state

  let cmd_read gen tid state = pathcmd read gen tid state

  let cmd_write pathgen v tid state = cmd @@ write tid (pathgen state) v

  let cmd_mkdir g t s = pathcmd mkdir g t s

  let cmd_rm g t s = pathcmd rm g t s

  let cmd_directory g t s = pathcmd directory g t s

  let cmd_getperms g t s = pathcmd getperms g t s

  let cmd_setperms pathgen vgen tid state =
    cmd @@ setperms tid (pathgen state) (Perms.Node.to_string @@ vgen state)

  let cmd_watch pathgen token _ state = cmd @@ watch (pathgen state) token

  let cmd_unwatch pathgen token _ state = cmd @@ unwatch (pathgen state) token

  let cmd_reset_watches tid _state =
    let open Xenbus in
    cmd
    @@ fun con ->
    Xenbus.Xb.queue con
      (Xenbus.Xb.Packet.create 0 0 Xenbus.Xb.Op.Reset_watches "")

  let cmd_transaction_start _ _ = cmd @@ transaction_start

  let cmd_transaction_end commit tid _ = cmd @@ transaction_end tid commit

  let domcmd f idgen _ state = cmd @@ f @@ idgen state

  let cmd_release idgen state = domcmd release idgen state

  let cmd_getdomainpath i s = domcmd getdomainpath i s

  let cmd_isintroduced i t s =
    domcmd
      (fun d con ->
        let open Xenbus in
        Xenbus.Xb.queue con
          (Xenbus.Xb.Packet.create 0 0 Xenbus.Xb.Op.Isintroduced
             (string_of_int d)))
      i t s

  let cmd_set_target idgen1 idgen2 _ state =
    let d = idgen1 state in
    let t = idgen2 state in
    cmd
    @@ fun con ->
    Xenbus.Xb.queue con
      (Xenbus.Xb.Packet.create 0 0 Xenbus.Xb.Op.Isintroduced
         (String.concat "\x00" [string_of_int d; string_of_int t]))

  let cmd_liveupdate _ _ = cmd @@ debug ["live-update"; "-s"]

  let cmd_introduce id port _ state = cmd @@ introduce id 0n port

  let pp_dump = Types.pp_dump_packet

  let precond cmd _state =
    match cmd with
    | {Xenbus.Packet.ty= Xenbus.Xb.Op.Release; data= "0\000"} ->
        false
        (* can't release Dom0 in the tests, or we crash due to shared dom0 backend *)
    | {ty= Xenbus.Xb.Op.Rm; data= ""} ->
        (* this is expected to cause inconsistencies on pre-created paths like /local *)
        false
    | _ ->
        true
end

let with_logger ~on_exn f =
  if is_output_devnull then f ()
  else
    let old = (!Logging.xenstored_logger, !Logging.access_logger) in
    let logs = ref [] in
    let write ?(level = Logging.Debug) s =
      let msg = Printf.sprintf "%s %s" (Logging.string_of_level level) s in
      logs := msg :: !logs
    in
    let logger =
      Some {Logging.stop= ignore; restart= ignore; rotate= ignore; write}
    in
    Logging.xenstored_logger := logger ;
    Logging.access_logger := logger ;
    Stdext.finally
      (fun () ->
        try f ()
        with e ->
          let bt = Printexc.get_raw_backtrace () in
          on_exn e bt (List.rev !logs))
      (fun () ->
        Logging.xenstored_logger := fst old ;
        Logging.access_logger := snd old)

type t =
  { store: Store.t
  ; cons: Connections.t
  ; doms: Domains.domains
  ; mutable anon: Unix.file_descr option
  ; live_update: bool
  ; txidtbl: (int, int) Hashtbl.t }

let () =
  Logging.xenstored_log_level := Logging.Debug ;
  Logging.access_log_special_ops := true ;
  Logging.access_log_transaction_ops := true ;
  let name, f = Filename.open_temp_file "xenstored" "port" in
  Domains.xenstored_port := name ;
  Stdext.finally (fun () -> Printf.fprintf f "%d" 1) (fun () -> close_out f) ;
  Domains.xenstored_kva := "/dev/zero" ;
  (* entries from a typical oxenstored.conf *)
  Transaction.do_coalesce := true ;
  Perms.activate := true ;
  Quota.activate := true ;
  Quota.maxent := 8192 ;
  Quota.maxsize := 2048 ;
  Define.maxwatch := 512 ;
  Define.maxtransaction := 10 ;
  Define.maxrequests := 1024

(* we MUST NOT release dom0, or we crash,
   this is shared between multiple tests, because
   it keeps an FD open, and we want to avoid EMFILE
*)

let create ?(live_update = false) () =
  let store = Store.create () in
  let cons = Connections.create () in
  let gnt = Gnt.Gnttab.interface_open () in (* dummy *)
  let doms = Domains.init (Event.init ()) gnt ignore in
  let dom0 = Domains.create0 doms in
  let txidtbl = Hashtbl.create 47 in
  Connections.add_domain cons dom0 ;
  {store; cons; doms; anon= None; live_update; txidtbl}

let cleanup t = Connections.iter t.cons Connection.close

let init t =
  let local = Store.Path.of_string "/local" in
  let con = Perms.Connection.create 0 in
  Store.mkdir t.store con local ;
 (* Store.mkdir t.store con (Store.Path.of_string "/tool") ;*)
  let fd = devnull () in
  t.anon <- Some fd ;
  Connections.add_anonymous t.cons fd

let dump_load s =
  let tmp = Filename.temp_file "xenstored" "qcheck.dump" in
  Stdext.finally
    (fun () ->
      Xenstored.DB.to_file None s.store s.cons tmp ;
      let s' = create () in
      (* preserve FD *)
      s'.anon <- s.anon ;
      s.anon <- None ;
      let _fds', errors =
        Xenstored.DB.from_file ~live:true s'.store s'.doms s'.cons tmp
      in
      if errors > 0 then
        failwith (Printf.sprintf "Errors during live update: %d" errors) ;
      s')
    (fun () -> Sys.remove tmp)

let is_live_update = function
  | {Xenbus.Packet.ty= Xenbus.Xb.Op.Debug; data= "live-update\000-s\000"} ->
      true
  | _ ->
      false

let is_tx_start p = p.Xenbus.Packet.ty = Xenbus.Xb.Op.Transaction_start

let with_tmpfile prefix write f =
  let name, ch = Filename.open_temp_file prefix ".txt" in
  Stdext.finally
    (fun () ->
      Stdext.finally (fun () -> write ch) (fun () -> close_out ch) ;
      f name)
    (fun () -> Sys.remove name)

let with_pp_to_file prefix pp x f =
  let write ch =
    let ppf = Format.formatter_of_out_channel ch in
    Format.pp_set_margin ppf @@ Format.get_margin () ;
    pp ppf x ;
    Fmt.flush ppf ()
  in
  with_tmpfile prefix write f

let run_cmd_get_output ?(ok_codes = [0]) cmd =
  let cmd = Array.of_list cmd in
  let ch = Unix.open_process_args_in cmd.(0) cmd in
  Stdext.finally
    (fun () ->
      let lines = ref [] in
      try
        while true do
          lines := input_line ch :: !lines
        done ;
        assert false
      with End_of_file -> List.rev !lines |> String.concat "\n")
    (fun () ->
      match Unix.close_process_in ch with
      | Unix.WEXITED code when List.mem code ok_codes ->
          ()
      | status ->
          Crowbar.failf "%a %a" (Fmt.array Fmt.string) cmd
            Types.pp_process_status status)

let call_diff x y =
  let ok_codes = [0; 1] in
  run_cmd_get_output ~ok_codes
    [ "/usr/bin/git"
    ; "diff"
    ; "-U10000" (* we want to see the entire state, where possible *)
    ; "--no-index"
    ; ( "--word-diff="
      ^ if Fmt.style_renderer Fmt.stdout = `Ansi_tty then "color" else "plain"
      )
    ; "--color-moved=dimmed-zebra"
    ; x
    ; y ]

let check_eq_exn prefix ~pp ~eq x y =
  if not @@ eq x y then
    if is_output_devnull then failwith "different"
    else
      with_pp_to_file "expected" pp x
      @@ fun xfile ->
      with_pp_to_file "actual" pp y
      @@ fun yfile ->
      failwith
      @@ Printf.sprintf "%s agrement: %s" prefix (call_diff xfile yfile)

let run next_tid t (domid, cmd) =
  let con =
    match domid with
    | 0 ->
        Connections.find !t.cons (Option.get !t.anon)
    | id ->
        Connections.find_domain !t.cons domid
  in
  (* clear out any watch events, TODO: don't  *)
  Connections.iter !t.cons (fun con -> Queue.clear con.xb.pkt_out) ;
  (* TODO: use the global live update state that processing the command sets, but remember to reset it *)
  if is_live_update cmd then
    if !t.live_update then (
      let t0 = !t in
      let t' = dump_load t0 in
      Connections.iter t'.cons (fun con ->
          Connection.iter_transactions con
          @@ fun _ tx ->
            (*  if tx.Transaction.operations <> [] then TODO: only if we dump snapshot state
                correctly *)
             Transaction.mark_failed tx) ;
      Logging.info "store" "store: %s" (Fmt.to_to_string Types.pp_dump_store t'.store);
      Logging.info "store" "store: %s" (Fmt.to_to_string Types.pp_dump_store t0.store);
      check_eq_exn "store" ~pp:Types.pp_dump_store ~eq:Types.equal_store
        t0.store t'.store ;
      (* TODO: now we have a disagreement here... so we can't test this until TX state is restored *)
      (*check_eq_exn "connections" ~pp:Types.pp_dump_connections
        ~eq:Types.equal_connections t0.cons t'.cons ;*)
      check_eq_exn "domains" ~pp:Types.pp_dump_domains ~eq:Types.equal_domains
        t0.doms t'.doms ;
      (* avoid double close on anonymous conn *)
      Connections.iter_domains t0.cons Connection.close ;
      t := {t' with txidtbl= !t.txidtbl} )
    else begin
      Logging.debug "testable" "BEFORE TXMARK";
      Connections.iter !t.cons (fun con ->
          Connection.iter_transactions con
          @@ fun txid tx ->
             Logging.debug "testable" "marking to fail %d" txid; 
             (* if tx.Transaction.operations <> [] then see above TODO *)
             Transaction.mark_failed tx) 
    end;
  let run_packet packet =
    let tid, rid, ty, data = Xenbus.Xb.Packet.unpack packet in
    Logging.debug "testable" "tid: %d" tid ;
    let tid = if tid <> 0 then Hashtbl.find !t.txidtbl tid else tid in
    let req : Packet.request =
      {Packet.tid; Packet.rid; Packet.ty; Packet.data}
    in
    Process.process_packet ~store:!t.store ~cons:!t.cons ~doms:!t.doms ~con ~req ;
    Process.write_access_log ~ty ~tid ~con:(Connection.get_domstr con) ~data ;
    let packet = Connection.peek_output con in
    if ty = Xenbus.Xb.Op.Transaction_start then (
      Logging.debug "testable" "Adding mapping for tid %d" next_tid ;
      Hashtbl.add !t.txidtbl next_tid (con.Connection.next_tid - 1) ) ;
    let tid, _rid, ty, data = Xenbus.Xb.Packet.unpack packet in
    Process.write_answer_log ~ty ~tid ~con:(Connection.get_domstr con) ~data
  in
  (* TODO: also a Nodes command with multiple packets *)
  run_packet cmd ; (* TODO: act on and clear watches? *)
                   con

let is_tx_marked_fail con p =
  let tid = p.Xenbus.Packet.tid in
  if tid = 0 then false
  else begin
    let r = try (Connection.get_transaction con tid).must_fail
    with Not_found -> false in
    Logging.info "testable" "TXI %d: %b" tid r;
    r
  end

let run2 next_tid t t' (domid, cmd) =
  let con = run next_tid t (domid, cmd) in
  let con' = run next_tid t' (domid, cmd) in
  (* TODO: ignore txid mismatches on transactions *)
  if not @@ (is_tx_start cmd || is_tx_marked_fail con cmd) then
    (* TODO: ignore disagreements when transactions are marked as failed *)
    check_eq_exn "reply packets" ~pp:Types.pp_dump_xb ~eq:Types.equal_xb_pkts
      con.xb con'.xb ;
  Queue.clear con'.xb.pkt_out ;
  Queue.clear con.xb.pkt_out

module type S = sig
  type cmd

  type state

  type sut

  val init_state : state

  val next_state : cmd -> state -> state

  val init_sut : unit -> sut

  val cleanup : sut -> unit

  val run_cmd : cmd -> state -> sut -> bool

  val precond : cmd -> state -> bool

  val pp : cmd Fmt.t
end
