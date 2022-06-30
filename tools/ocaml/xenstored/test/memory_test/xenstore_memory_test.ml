open Bos_setup

let size = 4096
let shm_create name =
  let fd = Shm.shm_open name true 0o500 in
  Unix.ftruncate fd size;
  fd

let shm_existing name = Shm.shm_open name false 0

let absent =
  let base = Sys.executable_name |> Filename.basename in
  let pid = Unix.getpid () in
  (* can contain only 1 slash *)
  Printf.sprintf "/%s-pid-%d" base pid

let name = OS.Arg.(opt ["shm"] ~doc:"shared memory name of xenstore ring passed to shm_open" string ~absent)

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
    type t = { fd: Unix.file_descr; tmp: bytes }

    let create () =
      let fd =
        if flag_client then Unix.stdin else
        let server, _client = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        (* TODO: pass client somewhere *)
        server
      in
      { fd; tmp = Bytes.make 1 ' ' }

    let notify_other_end t = 
      Logs.debug (fun m -> m "ring: notifying other end");
      let (_:int) = Unix.send_substring t.fd "." 0 1 [] in
      ()

    let wait_for_other_end t =
      Logs.debug (fun m -> m "ring: waiting for event");
      let (_:int) = Unix.recv t.fd t.tmp 0 1 [] in ()

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
    Logs.debug (fun m ->
      let dbg = Xenstore_ring.Ring.to_debug_map buffer in
      m "ring state: %a"
      Fmt.(Dump.list @@ pair string string) dbg
    );
    { buffer; fd;
      notif = Notify.create (); tmp = Bytes.create 1 }

  let destroy ch =
    Logs.debug (fun m -> m "closing shared memory file descriptor");
    Unix.close ch.fd;
    if not flag_client then Shm.shm_unlink name

  let rec read ch buf ofs len =
    let n = R.read ch.buffer buf ofs len in
    if n = 0 then begin
      Notify.wait_for_other_end ch.notif;
      read ch buf ofs len
    end else n

  let rec write ch buf ofs len =
    let n = R.write ch.buffer buf ofs len in
    if n > 0 then Notify.notify_other_end ch.notif;
    if n < len then begin
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
    Logs.debug (fun m -> m "directory /: %a" Fmt.(Dump.list string)
    (Client.directory h "/")
    )
  )

let debug = OS.Arg.flag ["d";"debug"] ~doc:"set log level to debug"

let () =
  Sys.catch_break true;
  OS.Arg.parse_opts ();
  if debug then Logs.set_level (Some Logs.Debug);
  if flag_client then client () else server ()
