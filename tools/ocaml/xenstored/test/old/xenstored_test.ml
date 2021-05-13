open Stdext
open QCheck
open Arbitrary

let () =
  (* Logging.access_log_nb_files := 1 ;
     Logging.access_log_transaction_ops := true ;
     Logging.access_log_special_ops := true ;
     Logging.access_log_destination := File "/tmp/log" ;
     Logging.init_access_log ignore ;
     Logging.set_xenstored_log_destination "/dev/stderr";
     Logging.init_xenstored_log (); *)
  Domains.xenstored_port := "xenstored-port" ;
  let f = open_out !Domains.xenstored_port in
  Printf.fprintf f "%d" 1 ;
  close_out f ;
  Domains.xenstored_kva := "/dev/zero"

module Command = struct
  type value = string

  let value = binary

  type token = string

  type txid = int

  type domid = Xenctrl.domid

  type t =
    | Read of Store.Path.t
    | Write of Store.Path.t * value
    | Mkdir of Store.Path.t
    | Rm of Store.Path.t
    | Directory of Store.Path.t
    (* | Directory_part not implemented *)
    | Get_perms of Store.Path.t
    | Set_perms of Store.Path.t * Perms.Node.t
    | Watch of Store.Path.t * token
    | Unwatch of Store.Path.t * token
    | Reset_watches
    | Transaction_start
    | Transaction_end of bool
    | Introduce of domid * nativeint * int
    | Release of int
    | Get_domain_path of domid
    | Is_domain_introduced of domid
    | Set_target of domid * domid
    | LiveUpdate

  type state =
    { store: Store.t
    ; doms: Domains.domains
    ; cons: Connections.t
    ; domids: int array }

  let path = list path_element

  let token = printable_string

  let domid state = oneofa ~print:Print.int state.domids

  let cmd state =
    let domid = domid state in
    let cmd_read = Case.make "READ" path (fun path -> Read path) in
    let cmd_write =
      Case.make "WRITE" (pair path value) (fun (path, value) ->
          Write (path, value))
    in
    let cmd_mkdir = Case.make "MKDIR" path (fun path -> Mkdir path) in
    let cmd_rm = Case.make "RM" path (fun path -> Rm path) in
    let cmd_directory =
      Case.make "DIRECTORY" path (fun path -> Directory path)
    in
    let cmd_get_perms =
      Case.make "GET_PERMS" path (fun path -> Get_perms path)
    in
    let cmd_set_perms =
      Case.make "SET_PERMS"
        (pair path (perms domid))
        (fun (path, perms) -> Set_perms (path, perms))
    in
    let cmd_watch =
      Case.make "WATCH" (pair path token) (fun (path, token) ->
          Watch (path, token))
    in
    let cmd_unwatch =
      Case.make "UNWATCH" (pair path token) (fun (path, token) ->
          Unwatch (path, token))
    in
    let cmd_reset_watches =
      Case.make "RESET_WATCHES" unit (fun () -> Reset_watches)
    in
    let cmd_tx_start =
      Case.make "TRANSACTION_START" unit (fun () -> Transaction_start)
    in
    let cmd_tx_end =
      Case.make "TRANSACTION_END" bool (fun commit -> Transaction_end commit)
    in
    let cmd_introduce =
      Case.make "INTRODUCE" (triple domid int int) (fun (domid, gfn, port) ->
          Introduce (domid, Nativeint.of_int gfn, port))
    in
    let cmd_release = Case.make "RELEASE" domid (fun domid -> Release domid) in
    let cmd_get_domain_path =
      Case.make "GET_DOMAIN_PATH" domid (fun domid -> Get_domain_path domid)
    in
    let cmd_is_domain_introduced =
      Case.make "IS_DOMAIN_INTRODUCED" domid (fun domid ->
          Is_domain_introduced domid)
    in
    let cmd_set_target =
      Case.make "SET_TARGET" (pair domid domid) (fun (domid, tdomid) ->
          Set_target (domid, tdomid))
    in
    let cmd_live_update =
      Case.make "CONTROL live-update" unit (fun () -> LiveUpdate)
    in
    let rev = function
      | Read a ->
          Case.call cmd_read a
      | Write (p, v) ->
          Case.call cmd_write (p, v)
      | Mkdir a ->
          Case.call cmd_mkdir a
      | Rm a ->
          Case.call cmd_rm a
      | Directory a ->
          Case.call cmd_directory a
      | Get_perms a ->
          Case.call cmd_get_perms a
      | Set_perms (p, v) ->
          Case.call cmd_set_perms (p, v)
      | Watch (p, t) ->
          Case.call cmd_watch (p, t)
      | Unwatch (p, t) ->
          Case.call cmd_unwatch (p, t)
      | Reset_watches ->
          Case.call cmd_reset_watches ()
      | Transaction_start ->
          Case.call cmd_tx_start ()
      | Transaction_end a ->
          Case.call cmd_tx_end a
      | Introduce (d, g, p) ->
          Case.call cmd_introduce (d, Nativeint.to_int g, p)
      | Release a ->
          Case.call cmd_release a
      | Get_domain_path a ->
          Case.call cmd_get_domain_path a
      | Is_domain_introduced a ->
          Case.call cmd_is_domain_introduced a
      | Set_target (d, t) ->
          Case.call cmd_set_target (d, t)
      | LiveUpdate ->
          Case.call cmd_live_update ()
    in
    let open Case in
    sum ~rev
      [ to_sum cmd_read
      ; to_sum cmd_write
      ; to_sum cmd_mkdir
      ; to_sum cmd_rm
      ; to_sum cmd_directory
      ; to_sum cmd_get_perms
      ; to_sum cmd_set_perms
      ; to_sum cmd_watch
      ; to_sum cmd_unwatch
      ; to_sum cmd_reset_watches
      ; to_sum cmd_tx_start
      ; to_sum cmd_tx_end
      ; to_sum cmd_introduce
      ; to_sum cmd_release
      ; to_sum cmd_get_domain_path
      ; to_sum cmd_is_domain_introduced
      ; to_sum cmd_set_target
      ; to_sum cmd_live_update ]

  let run tid =
    let open Xenstore.Queueop in
    function
    | Read p ->
        read tid Store.Path.(to_string p)
    | Write (p, v) ->
        write tid Store.Path.(to_string p) v
    | Mkdir p ->
        mkdir tid Store.Path.(to_string p)
    | Rm p ->
        rm tid Store.Path.(to_string p)
    | Directory p ->
        directory tid Store.Path.(to_string p)
    | Get_perms p ->
        getperms tid Store.Path.(to_string p)
    | Set_perms (p, v) ->
        setperms tid Store.Path.(to_string p) Perms.Node.(to_string v)
    | Watch (p, t) ->
        watch Store.Path.(to_string p) t
    | Unwatch (p, t) ->
        unwatch Store.Path.(to_string p) t
    | Reset_watches ->
        let open Xenbus in
        fun con -> Xb.queue con (Xb.Packet.create 0 0 Xb.Op.Reset_watches "")
    | Transaction_start ->
        transaction_start
    | Transaction_end c ->
        transaction_end tid c
    | Release d ->
        release d
    | Get_domain_path d ->
        getdomainpath d
    | Is_domain_introduced d ->
        let open Xenbus in
        fun con ->
          Xb.queue con
            (Xb.Packet.create 0 0 Xb.Op.Isintroduced (string_of_int d))
    | Set_target (d, t) ->
        let open Xenbus in
        fun con ->
          Xb.queue con
            (Xb.Packet.create 0 0 Xb.Op.Isintroduced
               (String.concat "\x00" [string_of_int d; string_of_int t]))
    | LiveUpdate ->
        debug ["live-update"; "-s"]
    | Introduce (d, g, p) ->
        introduce d g p
end

module Spec = struct
  type cmd = New | Cmd of Command.domid * int option * Command.t

  type state =
    { xb: Xenbus.Xb.t
    ; cnt: int
    ; cmdstate: Command.state ref option
    ; failure: (exn * string) option }

  type sut = state ref

  let doms = Domains.init (Event.init ()) ignore

  let dom0 = Domains.create0 doms

  let new_state () =
    let cons = Connections.create () in
    Connections.add_domain cons dom0 ;
    let store = Store.create () in
    let con = Perms.Connection.create 0 in
    Store.mkdir store con ["tool"] ;
    {Command.store; doms; cons; domids= [|0|]}

  let print = function
    | New ->
        "NEW"
    | Cmd (d, t, c) ->
        let s = new_state () in
        let cmd = Command.cmd s in
        (Option.get (triple (Command.domid s) (option int) cmd).print) (d, t, c)

  let shrink = function
    | New ->
        Iter.empty
    | Cmd (d, t, c) ->
        let s = new_state () in
        let cmd = Command.cmd s in
        Iter.map (fun (d, t, c) -> Cmd (d, t, c))
        @@ (Option.get (triple (Command.domid s) (option int) cmd).shrink)
             (d, t, c)

  let arb_cmd state =
    ( match state.cmdstate with
    | None ->
        always New
    | Some s ->
        let cmd = Command.cmd !s in
        QCheck.map
          (fun (d, t, c) -> Cmd (d, t, c))
          ~rev:(fun (Cmd (d, t, c)) -> (d, t, c))
        @@ triple (Command.domid !s) (option int) cmd )
    |> set_print print |> set_shrink shrink

  (*    |> set_collect (fun (_, _, c) -> (Option.get cmd.QCheck.collect) c)*)

  let init_state =
    {cnt= 0; xb= Xenbus.Xb.open_fd Unix.stdout; cmdstate= None; failure= None}

  let precond cmd s =
    match (cmd, s.cmdstate) with
    | New, None ->
        true
    | New, _ ->
        false
    | Cmd _, None ->
        false
    | Cmd (_, _, Command.Release 0), _ ->
        false
    | _ ->
        true

  let next_state cmd state =
    { ( try
          assume (precond cmd state) ;
          match cmd with
          | New ->
              {state with cmdstate= Some (ref @@ new_state ())}
          | Cmd (domid, tid, cmd) ->
              let tid = match tid with None -> 0 | Some id -> 1 + id in
              Command.run tid cmd state.xb ;
              let s = !(Option.get state.cmdstate) in
              let con = Connections.find_domain s.Command.cons domid in
              Queue.clear con.xb.pkt_out ;
              let run_packet packet =
                let tid, rid, ty, data = Xenbus.Xb.Packet.unpack packet in
                let req = {Packet.tid; Packet.rid; Packet.ty; Packet.data} in
                Process.process_packet ~store:s.Command.store
                  ~cons:s.Command.cons ~doms:s.Command.doms ~con ~req ;
                Process.write_access_log ~ty ~tid
                  ~con:(Connection.get_domstr con)
                  ~data ;
                let packet = Connection.peek_output con in
                let tid, _rid, ty, data = Xenbus.Xb.Packet.unpack packet in
                Process.write_answer_log ~ty ~tid
                  ~con:(Connection.get_domstr con)
                  ~data
              in
              Queue.iter run_packet state.xb.pkt_out ;
              Queue.clear state.xb.pkt_out ;
              state
        with e ->
          let bt = Printexc.get_backtrace () in
          {state with failure= Some (e, bt)} )
      with
      cnt= state.cnt + 1 }

  let init_sut () = ref init_state

  let cleanup _ = ()

  module P = struct
    type t = string list

    let compare = compare
  end

  module PathMap = Map.Make (P)

  module DomidMap = Map.Make (struct
    type t = Xenctrl.domid

    let compare = compare
  end)

  module IntMap = Map.Make (struct
    type t = int

    let compare = compare
  end)

  module FDMap = Map.Make (struct
    type t = Unix.file_descr

    let compare = compare
  end)

  let map_of_store s =
    let m = ref PathMap.empty in
    Store.dump_fct s (fun path node -> m := PathMap.add path node !m) ;
    !m

  let node_equiv n n' =
    Perms.equiv (Store.Node.get_perms n) (Store.Node.get_perms n')
    && Store.Node.get_name n = Store.Node.get_name n'
    && Store.Node.get_value n = Store.Node.get_value n'

  let store_root_equiv s s' =
    if not (PathMap.equal node_equiv (map_of_store s) (map_of_store s')) then
      let b = Store.dump_store_buf s.root in
      let b' = Store.dump_store_buf s'.root in
      Test.fail_reportf "Store trees are not equivalent:\n %s\n <>\n %s"
        (Buffer.contents b) (Buffer.contents b')
    else true

  let map_of_domid_table tbl = Hashtbl.fold DomidMap.add tbl DomidMap.empty

  let map_of_quota q = map_of_domid_table q.Quota.cur

  let store_quota_equiv root root' q q' =
    let _ =
      DomidMap.merge
        (fun domid q q' ->
          let q = Option.value ~default:(-1) q in
          let q' = Option.value ~default:(-1) q' in
          if q <> q' then
            let b = Store.dump_store_buf root in
            let b' = Store.dump_store_buf root' in
            Test.fail_reportf "quota mismatch on %d: %d <> %d\n%s\n%s\n" domid q
              q' (Buffer.contents b) (Buffer.contents b')
          else Some q)
        (map_of_quota q) (map_of_quota q')
    in
    true

  let store_equiv s s' =
    store_root_equiv s s'
    && store_quota_equiv s.root s'.root (Store.get_quota s) (Store.get_quota s')

  let map_of_domains d = map_of_domid_table d.Domains.table

  let domain_equiv d d' =
    Domain.get_id d = Domain.get_id d'
    && Domain.get_remote_port d = Domain.get_remote_port d'

  let domains_equiv d d' =
    DomidMap.equal domain_equiv (map_of_domains d) (map_of_domains d')

  let map_of_fd_table tbl = Hashtbl.fold FDMap.add tbl FDMap.empty

  let map_of_int_table tbl = Hashtbl.fold IntMap.add tbl IntMap.empty

  let list_of_queue q = Queue.fold (fun acc e -> e :: acc) [] q

  let connection_equiv c c' =
    let l = list_of_queue c.Connection.xb.pkt_out in
    let l' = list_of_queue c'.Connection.xb.pkt_out in
    if List.length l <> List.length l' || List.exists2 ( <> ) l l' then (
      let print_packets l =
        l
        |> List.rev_map (fun p ->
               let tid, rid, ty, data = Xenbus.Packet.unpack p in
               let tystr = Xenbus.Xb.Op.to_string ty in
               Printf.sprintf "tid=%d, rid=%d, ty=%s, data=%s" tid rid tystr
                 (String.escaped data))
        |> String.concat "\n"
      in
      let r = print_packets l in
      let r' = print_packets l' in
      Test.fail_reportf "Replies not equal:\n%s\n <>\n %s" r r' )
    else
      let n = Connection.number_of_transactions c in
      let n' = Connection.number_of_transactions c' in
      if n <> n' then Test.fail_reportf "TX count mismatch: %d <> %d" n n'
      else true

  let connections_equiv c c' =
    FDMap.equal connection_equiv
      (map_of_fd_table c.Connections.anonymous)
      (map_of_fd_table c'.Connections.anonymous)
    && IntMap.equal connection_equiv
         (map_of_int_table c.Connections.domains)
         (map_of_int_table c'.Connections.domains)

  let dump_load s =
    let tmp = Filename.temp_file "xenstored" "qcheck.dump" in
    finally
      (fun () ->
        let fds = {Xenstored.DB.rw_sock= None; ro_sock= None} in
        Xenstored.DB.to_file fds !s.Command.store !s.Command.cons tmp ;
        s := new_state () ;
        let _fds', errors =
          Xenstored.DB.from_file ~live:true !s.Command.store !s.Command.doms
            !s.Command.cons tmp
        in
        if errors > 0 then
          Test.fail_reportf "Errors during live update: %d" errors)
      (fun () -> Sys.remove tmp)

  let run_cmd cmd state sut =
    ( match state.failure with
    | None ->
        true
    | Some (e, bt) ->
        Test.fail_reportf "Exception %s, backtrace: %s" (Printexc.to_string e)
          bt )
    &&
    match cmd with
    | New ->
        sut := next_state cmd !sut ;
        true
    | Cmd (0, _, Command.LiveUpdate) ->
        let s = !sut.cmdstate in
        let store1 = Store.copy !(Option.get s).store in
        let doms1 = !(Option.get s).doms in
        dump_load (Option.get s) ;
        (* reply is expected not to be equivalent, since after live update we got an empty reply queue,
           so don't compare connections
        *)
        store_equiv store1 !(Option.get s).store
        && domains_equiv doms1 !(Option.get s).doms
    | Cmd(_, _, cmd') -> (
        (* TODO: also got same reply, and check for equivalence on the actual Live Update *)
        sut := next_state cmd !sut ;
        let ids = Hashtbl.create 47 in
        Connections.iter !(Option.get state.cmdstate).cons (fun con ->
            Hashtbl.add ids (Connection.get_id con) con.next_tid) ;
        let state = next_state cmd state in
        match (!sut.failure, state.cmdstate, !sut.cmdstate) with
        | None, Some s, Some s' ->
            let r = cmd' = Command.Transaction_start (* txid can change *) || 
               connections_equiv !s.cons !s'.cons in
            Connections.iter !(Option.get state.cmdstate).cons (fun con ->
                let tid = Hashtbl.find ids (Connection.get_id con) in
                if con.next_tid <> tid then (
                  let (_ : bool) = Connection.end_transaction con tid None in
                  () ;
                  con.next_tid <- tid )) ;
            r
        | None, None, None ->
            true
        | None, None, _ ->
            Test.fail_report "state uninit"
        | None, _, None ->
            Test.fail_report "sut uninit"
        | Some (e, bt), _, _ ->
            Test.fail_reportf "Exception %s, backtrace: %s"
              (Printexc.to_string e) bt )
end

module States = QCSTM.Make (Spec)

(* && watches_equiv c c' *)

let test = States.agree_test ~count:100 ~name:"live-update"

let test =
  Test.make ~name:"live-update" ~count:100
    (States.arb_cmds Spec.init_state)
    States.agree_prop

let () = QCheck_base_runner.run_tests_main [test]
