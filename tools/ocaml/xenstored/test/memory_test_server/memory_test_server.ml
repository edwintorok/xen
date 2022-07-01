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
    Printf.eprintf "Unlinking %s\n" shm_name;
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

let on_startup cons doms store eventchn =
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
    |> List.iter @@ fun dir ->
        if not (Store.path_exists store dir) then
          Store.mkdir store con dir);
    let perms = Perms.Node.create domid Perms.NONE [] in
    Store.setperms store con path perms;
    dom
  in
  let dom = make_dom 1
  in
  Connections.add_domain cons dom

let () =
  Logging.set_xenstored_log_destination "/dev/stderr";
  Logging.set_access_log_destination "/dev/stderr";
  let portfile = Filename.temp_file "xenstored_port" "txt" in
  at_exit (fun () -> Unix.unlink portfile);
  let ch = open_out portfile in
  output_string ch "0";
  close_out ch;
  Domains.xenstored_port := portfile;
  Domains.xenstored_kva := "/dev/zero";
  let argv = [|
    "oxenstored";
    "--test";
    "--disable-socket";
    "--no-fork";
    (* "--no-domain-init" do not use this as it turns off eventchn processing! *) 
  |] in
  prerr_endline "launching main";
  Xenstored.main ~argv ~on_startup ()
