open Xenstored_test

let () = Tracedebug_logs.dump_at_exit (fun () -> [])

let log_write ?(level = Logging.Debug) s =
  Logs.info @@ fun m -> m "%s %s\n" (Logging.string_of_level level) s

let logger =
  Logging.{stop= ignore; restart= ignore; rotate= ignore; write= log_write}

let () =
  (* by default we don't print xenstore access/debug logs *)
  Printexc.record_backtrace true ;
  Logging.access_log_special_ops := true ;
  Logging.access_log_transaction_ops := true ;
  (* Logging.xenstored_log_level := Logging.Debug *)
  Logging.xenstored_log_level := Logging.Info

let shm_name =
  let base = Sys.executable_name |> Filename.basename in
  let pid = Unix.getpid () in
  (* can contain only 1 slash *)
  Printf.sprintf "/%s-pid-%d" base pid

let shm_fd =
  Printf.eprintf "Creating %s\n%!" shm_name ;
  Shm.shm_open shm_name true 0o600

let size = 4096

let () =
  Define.maxdomumemory := 4 * 1024 * 1024 ;
  Unix.ftruncate shm_fd size ;
  at_exit (fun () ->
      Unix.close shm_fd ;
      Printf.eprintf "Unlinking %s\n" shm_name ;
      Shm.shm_unlink shm_name ;
      flush stderr
  )

let spawn_client name domid rd wr =
  let (_0 :: _config :: cmd :: rest) = Sys.argv |> Array.to_list in
  let pid =
    Unix.create_process cmd
      (Array.of_list
         ([
            Printf.sprintf "xenstore-client %d" domid
          ; "--client"
          ; "--shm"
          ; name (*  "--debug" *)
          ]
         @ rest
         )
      )
      rd wr Unix.stderr
  in
  at_exit (fun () -> Unix.kill pid 15)

let override_defaults () =
  (* defaults used in production, test that memory quotas can cope with the
     increased defaults *)
  Quota.maxent := 8192;
  Define.maxwatch := 512;
  Logging.xenstored_log_level := Logging.Debug


let debug = Logging.debug "memory_test_server"

let print_defaults () =
  let log (key, intval) =
    debug "%s = %d" key intval
  in
  List.iter log
  [
	("quota-maxwatch", !Define.maxwatch);
	("quota-transaction", !Define.maxtransaction);
	("quota-domu-memory", !Define.maxdomumemory);
	("quota-maxentity", !Quota.maxent);
	("quota-maxsize", !Quota.maxsize);
	("quota-maxrequests", !Define.maxrequests);
	("quota-path-max", !Define.path_max);
  ] 

let on_startup cons doms store eventchn =
  Logging.set_xenstored_logger logger ;
  Logging.access_logger := Some logger ;
  override_defaults ();
  print_defaults ();
  let make_dom domid =
    (* TODO: per domid shm *)
    let evt = eventchn.Event.handle in
    prerr_endline "Spawning client" ;
    spawn_client shm_name domid evt.Xeneventchn.client_recv
      evt.Xeneventchn.client_send ;
    (* TODO: duplicate code with add_domain *)
    let mapping = Xenmmap.mmap shm_fd Xenmmap.RDWR Xenmmap.SHARED size 0 in
    let dom = Domain.make domid 0n 0 mapping eventchn in
    Hashtbl.add doms.Domains.table domid dom ;
    Domain.bind_interdomain dom ;
    (* TODO: like xenopsd would... *)
    let con = Perms.Connection.create 0 in
    let path =
      Printf.sprintf "/local/domain/%d/data" domid |> Store.Path.of_string
    in
    (path
    |> Store.Path.get_hierarchy
    |> List.iter @@ fun dir ->
       if not (Store.path_exists store dir) then
         Store.mkdir store con dir
    ) ;
    let perms = Perms.Node.create domid Perms.NONE [] in
    Store.setperms store con path perms ;
    dom
  in
  let dom = make_dom 1 in
  Connections.add_domain cons dom

let () =
  Logging.set_xenstored_log_destination "/dev/stderr" ;
  Logging.set_access_log_destination "/dev/stderr" ;
  Logging.set_xenstored_logger logger ;
  let portfile = Filename.temp_file "xenstored_port" "txt" in
  at_exit (fun () -> Unix.unlink portfile) ;
  let ch = open_out portfile in
  output_string ch "0" ;
  close_out ch ;
  Domains.xenstored_port := portfile ;
  Domains.xenstored_kva := "/dev/zero" ;
  let argv =
    [|
       "oxenstored"
     ; "--test"
     ; "--disable-socket"
     ; "--no-fork"
     ; "--config-file=" ^ Sys.argv.(1)
       (* "--no-domain-init" do not use this as it turns off eventchn processing! *)
    |]
  in
  let (_ : Sys.signal_behavior) =
    Sys.signal Sys.sigchld
      (Sys.Signal_handle
         (fun _ ->
           prerr_endline "Child quit, exiting" ;
           exit 2
         )
      )
  in
  prerr_endline "launching main" ;
  Xenstored.main ~argv ~on_startup ()
