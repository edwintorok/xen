open Xenstored_test

let log_write ?(level=Logging.Debug) s =
  Printf.eprintf "%s %s\n" (Logging.string_of_level level) s

let logger =
  Logging.{ stop = ignore
               ; restart = ignore
               ; rotate = ignore
               ; write = log_write }
let () =
  (* by default we don't print xenstore access/debug logs *)
  Printexc.record_backtrace true;
  Logging.set_xenstored_logger logger;
  Logging.access_logger := Some logger;
  Logging.access_log_special_ops := true;
  Logging.access_log_transaction_ops := true ;
  Logging.xenstored_log_level := Logging.Debug

let cons = Connections.create ()
let store = Store.create ()
let eventchn = Event.init ()
let doms = Domains.init eventchn ignore

let shm_name =
  let base = Sys.executable_name |> Filename.basename in
  let pid = Unix.getpid () in
  (* can contain only 1 slash *)
  Printf.sprintf "/%s-pid-%d" base pid

let shm_fd =
  Printf.eprintf "Creating %s\n%!" shm_name;
  Shm.shm_open shm_name true 0o600

let size = 4096
let () =
  Unix.ftruncate shm_fd size;
  at_exit (fun () ->
    Unix.close shm_fd;
    Shm.shm_unlink shm_name;
    flush stderr
  )

let spawn_client name domid rd wr =
  let pid = Unix.create_process Sys.argv.(1)
    [| Printf.sprintf "xenstore-client %d" domid
     ; "--client"
     ; "--shm"; name
     ; "--debug"
    |]
    rd wr Unix.stderr in
  at_exit (fun () -> Unix.kill pid 15)

let make_dom domid =
  (* TODO: per domid shm *)
  let evt = eventchn.Event.handle in
  prerr_endline "Spawning client";
  spawn_client shm_name domid  evt.Xeneventchn.client_recv evt.Xeneventchn.client_send;
  (* TODO: duplicate code with add_domain *)
  let mapping = Xenmmap.mmap shm_fd Xenmmap.RDWR Xenmmap.SHARED size 0 in
  let dom = Domain.make domid 0n 0 mapping eventchn in
  Hashtbl.add doms.Domains.table domid dom;
  Domain.bind_interdomain dom;
  (* TODO: like xenopsd would... *)
  let con = Perms.Connection.create 0 in
  let path = Printf.sprintf "/local/domain/%d/data" domid |> Store.Path.of_string in
  (path |> Store.Path.get_hierarchy
  |> List.iter @@ Store.mkdir store con);
  let perms = Perms.Node.create domid Perms.NONE [] in
  Store.setperms store con path perms;
  dom

let dom = make_dom 1

let process_domains store cons domains =
   let do_io_domain domain =
       if Domain.is_bad_domain domain
       || Domain.get_io_credit domain <= 0
       || Domain.is_paused_for_conflict domain
       then () (* nothing to do *)
       else (
         prerr_endline "here";
           let con = Connections.find_domain cons (Domain.get_id domain) in
           Process.do_input store cons domains con;
           Process.do_output store cons domains con;
           Domain.decr_io_credit domain;
           prerr_endline "done"
       ) in
   Domains.iter domains do_io_domain
   (* TODO: reuse from xenstored *)

let tmp = Bytes.make 1 ' '
let () =
  Connections.add_domain cons dom;
  let n = ref 0 in
  let t = ref @@ Unix.gettimeofday () in
  while true do
    Domain.incr_io_credit dom; (* hack *)
    let is_peaceful c =
      match Connection.get_domain c with
                        | None -> true (* Treat socket-connections as exempt, and free to conflict. *)
                        | Some dom -> not (Domain.is_paused_for_conflict dom)
    in
    let mw = Connections.has_more_work cons in
    let peaceful_mw = List.filter is_peaceful mw in
    List.iter
                      (fun c ->
                        match Connection.get_domain c with
                       | None -> () | Some d -> Domain.incr_io_credit d)
                      peaceful_mw;
    let inset, outset = Connections.select cons in
    if (List.length peaceful_mw > 0) then
      process_domains store cons doms;
    let rset, wset, _ = Poll.poll_select (Event.fd eventchn :: inset) outset [] 5. in
    incr n;
    let now = Unix.gettimeofday () in
    if now > !t +. 1.  then begin
      Printf.eprintf "Requests: %d in %.2f\n" !n (!t -. now);
      (* TODO: detect if no requests: means stuck processing and bug *)
      flush stderr;
      n := 0;
      t := now
    end;
    if List.length rset = 0 && List.length wset = 0 then begin
      prerr_endline "Packet processing stuck: no more input available\n";
      exit 4
    end;
    List.iter (fun fd ->
      try Process.do_input store cons doms @@ Connections.find cons fd
      with Not_found ->
        if fd = Event.fd eventchn then begin
          prerr_endline "consumed notification";
          let (_:int) = Unix.read fd tmp 0 1 in
          ()
        end
    ) rset;
    List.iter (fun fd -> Process.do_output store cons doms @@ Connections.find cons fd) wset;
    process_domains store cons doms
  done
