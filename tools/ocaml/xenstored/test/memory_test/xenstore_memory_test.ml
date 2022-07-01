open Bos_setup

let size = 4096
let shm_create name =
  Logs.debug (fun m -> m "Creating new %s" name);
  let fd = Shm.shm_open name true 0o600 in
  Unix.ftruncate fd size;
  fd

let shm_existing name =
  Logs.debug (fun m -> m "opening existing %s" name);
  Shm.shm_open name false 0

let absent =
  let base = Sys.executable_name |> Filename.basename in
  let pid = Unix.getpid () in
  (* can contain only 1 slash *)
  Printf.sprintf "/%s-pid-%d" base pid

let name = OS.Arg.(opt ["shm"] ~doc:"shared memory name of xenstore ring passed to shm_open" string ~absent)
let debug = OS.Arg.flag ["d";"debug"] ~doc:"set log level to debug"

let map_buffer fd =
  Logs.debug (fun m -> m "mapping buffer of size %d" size);
  Unix.map_file fd Bigarray.char Bigarray.c_layout true [|size|]
    |> Bigarray.array1_of_genarray
    |> Cstruct.of_bigarray

    module type Notification = sig
      type t
      val create: unit -> t
  val notify_other_end: t -> unit
  val wait_for_other_end: t -> unit
    end

let flag_client = OS.Arg.(flag ["client"] ~doc:"run in client mode")

module Notify = struct
  type t = { fd_send: Unix.file_descr; fd_recv: Unix.file_descr; tmp: bytes }

  let spawn_client name domid rd wr =
    Logs.debug (fun m -> m "Spawning client");
      let pid = Unix.create_process Sys.executable_name
      (Array.append [| Printf.sprintf "xenstore-client %d" domid
       ; "--client"
       ; "--shm"; name
|] (if debug then [|"--debug"|] else [||]))
      rd wr Unix.stderr in
      Logs.debug (fun m -> m "Spawned client PID %d" pid);
      at_exit (fun () -> Unix.kill pid 15)

  let create () =
    let fd_recv, fd_send =
      if flag_client then Unix.stdin, Unix.stdout else
        begin
          let pipe1_rd, pipe1_wr = Unix.pipe ~cloexec:false () in
          let pipe2_rd, pipe2_wr = Unix.pipe ~cloexec:false () in
          spawn_client name 1 pipe2_rd pipe1_wr;
          pipe1_rd, pipe2_wr
    end
          in
      { fd_recv; fd_send; tmp = Bytes.make 1 ' ' }

  let notify_other_end t = 
    Logs.debug (fun m -> m "ring: notifying other end");
      let (_:int) = Unix.write_substring t.fd_send "." 0 1 in
      ()

  let wait_for_other_end t =
    Logs.debug (fun m -> m "ring: waiting for event");
      let (_:int) = Unix.read t.fd_recv t.tmp 0 1 in
      Logs.debug (fun m -> m "ring: received event")

end

module MakeIO(R: Ring.S)(Notify: Notification) = struct
  type 'a t = 'a
  let return x = x
  let (>>=) x f = f x

  type channel = {
    fd: Unix.file_descr;
    buffer: Cstruct.t;
    notif: Notify.t;
    tmp: bytes }

  let debug_ring ch =
    Logs.debug (fun m ->
      let dbg = Xenstore_ring.Ring.to_debug_map ch.buffer in
      m "ring state: %a"
      Fmt.(Dump.list @@ pair string string) dbg
      )

  let create () =
    Logs.debug (fun m -> m "opening shared memory page");
    let fd =
      if flag_client then shm_existing name
      else shm_create name in
    let buffer = map_buffer fd in
    if not flag_client then begin
      Logs.debug (fun m -> m "Initializing ring");
      Xenstore_ring.Ring.init buffer;
    end;
    let notif = Notify.create () in
    if not flag_client then begin
      (* TODO: multiple domid support, each with its own ring *)
    end;
    let t = { buffer; fd;
      notif; tmp = Bytes.create 1 } in
    debug_ring t;
    t

  let destroy ch =
    Logs.debug (fun m -> m "closing shared memory file descriptor");
    Unix.close ch.fd;
    if not flag_client then Shm.shm_unlink name

  let rec read ch buf ofs len =
    let n = R.read ch.buffer buf ofs len in
    debug_ring ch;
    if n = 0 then begin
      Notify.wait_for_other_end ch.notif;
      read ch buf ofs len
    end else n

  let rec write ch buf ofs len =
    let n = R.write ch.buffer buf ofs len in
    if n > 0 then Notify.notify_other_end ch.notif;
    if n < len then begin
      debug_ring ch;
      Notify.wait_for_other_end ch.notif;
      write ch buf (ofs + n) (len - n)
    end
    end

module Client = Xs_client_unix.Client(MakeIO(Xenstore_ring.Ring.Front)(Notify))

module ServerIO = MakeIO(Xenstore_ring.Ring.Back)(Notify)
module PS = Xs_protocol.PacketStream(ServerIO)

let pp_packet = Fmt.using Xs_protocol.Request.prettyprint Fmt.string

let rec loop_forever ps =
  match PS.recv ps with
  | Exception exn ->
      Logs.warn (fun m -> m "Cannot parse request: %a" Fmt.exn exn)
  | Ok req ->
      Logs.debug (fun m -> m
    "got command: %a" pp_packet req);
  let tid = Xs_protocol.get_tid req in
  let rid = Xs_protocol.get_rid req in
  (Xs_protocol.Response.(print (Error "ENOTSUP") tid rid)
  |> PS.send ps);
  loop_forever ps

let server () =
  Logs.info (fun m -> m "Waiting for commands on %s" name);
  let io = ServerIO.create () in
  let ps = PS.make io in
  at_exit (fun () -> ServerIO.destroy io);
  loop_forever ps

let client () =
  Logs.debug (fun m -> m "initializing client");
  let c = Client.make () in
  Client.set_logger (fun s -> Logs.debug @@ fun m -> m "xenstore-client: %s" s);
  Client.immediate c (fun h ->
    Logs.debug (fun m -> m "domainpath: %s" (Client.getdomainpath h 1)
    )
  );
  (* due to Bos we have Astring here *)
  let key_prefix = String.v ~len:500 (fun _ -> 'y') in
  let value_prefix = String.v ~len:600 (fun _ -> 'v') in
  let i = ref 0 in
  Client.transaction_one_try c (fun h ->
    (* we could probe size here? *)
    while true do
      incr i;
      (* TODO: discover writable paths basedon list *)
      let key = Printf.sprintf "data/%s%d" key_prefix !i in
      let key_enoent = Printf.sprintf "data/no%s%d" key_prefix !i in
      let value = Printf.sprintf "%s%d" value_prefix !i in
      Client.write h key value;
      (* this will have to get stored in the replay queue so that after a 
         conflict reply can be checked to match *)
      (try let (_:string) = Client.read h key_enoent in ()
      with Xs_protocol.Enoent _ -> ());
      Client.rm h key;
    done
  )

let () =
  Sys.catch_break true;
  OS.Arg.parse_opts ();
  if debug then Logs.set_level (Some Logs.Debug);
  if flag_client then client () else server ()
