(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let string_make = String.make

open Bos_setup
open Tracedebug

let tracer = create StringEvent.empty

module RingEvent = struct
  type t = (string * string) list (* ring snapshot *)

  let tracer = create []

  let record ring = record1 tracer Xenstore_ring.Ring.to_debug_map ring

  let pp ppf debugmap =
    Fmt.pf ppf "ring state: %a"
      (Fmt.hbox Fmt.(list ~sep:Fmt.sp @@ pair ~sep:Fmt.sp string string))
      debugmap
end

let () =
(*
downside: registering a logger that only prints at exit will print nothing
if things hangs as there are no timeouts, yet...
Tracedebug_logs.dump_at_exit @@ fun () ->
  [
    Tracedebug.dump RingEvent.pp RingEvent.tracer
  ; Tracedebug.dump StringEvent.pp tracer
  ] *) ()

let size = 4096

let shm_create name =
  Logs.debug (fun m -> m "Creating new %s" name) ;
  let fd = Shm.shm_open name true 0o600 in
  Unix.ftruncate fd size ; fd

let shm_existing name =
  Logs.debug (fun m -> m "Opening existing %s" name) ;
  Shm.shm_open name false 0

let absent =
  let base = Sys.executable_name |> Filename.basename in
  let pid = Unix.getpid () in
  (* can contain only 1 slash *)
  Printf.sprintf "/%s-pid-%d" base pid

let name =
  OS.Arg.(
    opt ["shm"] ~doc:"shared memory name of xenstore ring passed to shm_open"
      string ~absent
  )

let debug = OS.Arg.flag ["d"; "debug"] ~doc:"set log level to debug"

let map_buffer fd =
  Logs.debug (fun m -> m "mapping buffer of size %d" size) ;
  Unix.map_file fd Bigarray.char Bigarray.c_layout true [|size|]
  |> Bigarray.array1_of_genarray
  |> Cstruct.of_bigarray

module type Notification = sig
  type t

  val create : unit -> t

  type event

  val prepare : t -> event

  val wait_for_other_end : t -> event -> string -> event

  val notify_other_end : t -> string -> unit
end

let flag_client = OS.Arg.(flag ["client"] ~doc:"run in client mode")

let flag_memuse =
  OS.Arg.(flag ["memuse"] ~doc:"memory (fragmentation) usage (client mode)")

module Notify = struct
  type t = {
      fd_send: Unix.file_descr
    ; fd_recv: Unix.file_descr
    ; tmp: bytes
    ; (* todo: activations like interface *)
      mutex: Mutex.t (* xc_client_unix is multithreaded *)
    ; mutex_fd: Mutex.t
    ; cond_fd: Condition.t
    ; mutable events: int
  }

  let spawn_client name domid rd wr =
    (* TODO: tmpfile *)
    Logs.debug (fun m -> m "Spawning client") ;
    let pid =
      Unix.create_process Sys.executable_name
        (Array.append
           [|
              Printf.sprintf "xenstore-client %d" domid
            ; "--client"
            ; "--shm"
            ; name
           |]
           (if debug then [|"--debug"|] else [||])
        )
        rd wr Unix.stderr
    in
    Logs.debug (fun m -> m "Spawned client PID %d" pid) ;
    at_exit (fun () -> Unix.kill pid 15)

  let create () =
    let fd_recv, fd_send =
      if flag_client then
        (Unix.stdin, Unix.stdout)
      else
        let pipe1_rd, pipe1_wr = Unix.pipe ~cloexec:false () in
        let pipe2_rd, pipe2_wr = Unix.pipe ~cloexec:false () in
        spawn_client name 1 pipe2_rd pipe1_wr ;
        (pipe1_rd, pipe2_wr)
    in
    {
      fd_recv
    ; fd_send
    ; tmp= Bytes.make 1 ' '
    ; mutex= Mutex.create ()
    ; mutex_fd= Mutex.create ()
    ; cond_fd= Condition.create ()
    ; events= 0
    }

  let notify_other_end t msg =
    (* TODO: record without allocating with multiple tracers *)
    recordf tracer (fun () -> Printf.sprintf "%s: ring: notify other end" msg) ;
    let (_ : int) = Unix.write_substring t.fd_send "." 0 1 in
    ()

  let with_mutex t f =
    Mutex.lock t.mutex ;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () -> f t)

  type event = int

  let prepare t = with_mutex t @@ fun t -> t.events

  let wait_for_other_end t event msg =
    recordf tracer (fun () ->
        Printf.sprintf "%s: ring: waiting for event <> %d" msg event
    ) ;
    let next =
      with_mutex t @@ fun t ->
      (* it can also be < if it wraps *)
      if t.events = event then (
        (* do  not release mutex here, other threads will wait to enter,
           and when we finally release the mutex they'll see event count changed
           and exit
           NO it can deadlock if we wait on smaller event...
        *)
        if Mutex.try_lock t.mutex_fd then (
          Mutex.unlock t.mutex ;
          let (_ : int) = Unix.read t.fd_recv t.tmp 0 1 in
          Mutex.lock t.mutex ;
          t.events <- t.events + 1 ;
          Condition.broadcast t.cond_fd ;
          Mutex.unlock t.mutex_fd
        ) else
          Condition.wait t.cond_fd t.mutex ;
        t.events
      ) else
        t.events
    in
    recordf tracer (fun () ->
        Printf.sprintf "%s: ring: received event %d" msg next
    ) ;
    next
end

module MakeIO (R : Ring.S) (Notify : Notification) = struct
  type 'a t = 'a

  let return x = x

  let ( >>= ) x f = f x

  type channel = {
      fd: Unix.file_descr
    ; buffer: Cstruct.t
    ; notif: Notify.t
    ; tmp: bytes
  }

  let debug_ring ch msg =
    recordf RingEvent.tracer (fun () ->
        Xenstore_ring.Ring.to_debug_map ch.buffer
    ) ;
    record tracer msg

  exception Xs_ring_error of int

  let check_ring_error_exn ch =
    (* TODO: modify upstream xenstore_ring.ml in shared-memory-ring *)
    let features_offset = 2048 + (4 * 4) in
    (* see xs_wire.h *)
    let error_offset = features_offset + (2 * 4) in
    let get_u32 off = Cstruct.HE.get_uint32 ch.buffer off |> Int32.to_int in
    let features = get_u32 features_offset in
    Logs.debug (fun m -> m "Server features: %d" features) ;
    let has_error_feature = features land 2 <> 0 in
    if has_error_feature then (
      record tracer "ring error check" ;
      let err = get_u32 error_offset in
      if err <> 0 then (
        recordf tracer (fun () -> Printf.sprintf "ring error: %d" err) ;
        exit 100 (* raise (Xs_ring_error err) *)
      )
    ) else
      record tracer "server doesn't have error reporting feature"

  let create () =
    Logs.debug (fun m -> m "opening shared memory page") ;
    let fd =
      if flag_client then
        shm_existing name
      else
        shm_create name
    in
    let buffer = map_buffer fd in
    if not flag_client then (
      Logs.debug (fun m -> m "Initializing ring") ;
      Xenstore_ring.Ring.init buffer
    ) ;
    let notif = Notify.create () in
    if not flag_client then
      ( (* TODO: multiple domid support, each with its own ring *) ) ;
    let t = {buffer; fd; notif; tmp= Bytes.create 1} in
    debug_ring t "create" ; t

  let destroy ch =
    Logs.debug (fun m -> m "closing shared memory file descriptor") ;
    Unix.close ch.fd ;
    if not flag_client then Shm.shm_unlink name

  let rec read_aux ch buf ofs len event =
    debug_ring ch "before read" ;
    let n = R.read ch.buffer buf ofs len in
    debug_ring ch "after read" ;
    Logs.debug (fun m -> m "read: got = %d" n) ;
    if n = 0 then (
      check_ring_error_exn ch ;
      let event = Notify.wait_for_other_end ch.notif event "read" in
      record tracer "read: repeating" ;
      read_aux ch buf ofs len event
    ) else (
      Notify.notify_other_end ch.notif "read" ;
      Logs.debug (fun m -> m "read: done %d" n) ;
      n
    )

  let read ch buf ofs len =
    Logs.debug (fun m -> m "read: start; len = %d" len) ;
    (* read event counter before reading ring and wait for event counter <>
       this value to avoid race conditions in another thread receiving an event
       that we waited for.
       when an event is received all threads that were waiting on one wake up
       and check the ring and then back to waiting
    *)
    Notify.prepare ch.notif |> read_aux ch buf ofs len

  let rec write_aux ch buf ofs len event =
    debug_ring ch "write" ;
    let n = R.write ch.buffer buf ofs len in
    debug_ring ch "write" ;
    Logs.debug (fun m -> m "write: wrote %d/%d" n len) ;
    if n > 0 then
      Notify.notify_other_end ch.notif "write" ;
    if n < len then (
      debug_ring ch "write" ;
      let event = Notify.wait_for_other_end ch.notif event "write" in
      record tracer "write: recursing" ;
      write_aux ch buf (ofs + n) (len - n) event
    ) ;
    record tracer "write: done"

  let write ch buf ofs len =
    Logs.debug (fun m -> m "write: got=%d" len) ;
    Notify.prepare ch.notif |> write_aux ch buf ofs len
end

module Client =
  Xs_client_unix.Client (MakeIO (Xenstore_ring.Ring.Front) (Notify))
module ServerIO = MakeIO (Xenstore_ring.Ring.Back) (Notify)
module PS = Xs_protocol.PacketStream (ServerIO)

let pp_packet = Fmt.using Xs_protocol.Request.prettyprint Fmt.string

let rec loop_forever ps =
  match PS.recv ps with
  | Exception exn ->
      Logs.warn (fun m -> m "Cannot parse request: %a" Fmt.exn exn)
  | Ok req ->
      Logs.debug (fun m -> m "got command: %a" pp_packet req) ;
      let tid = Xs_protocol.get_tid req in
      let rid = Xs_protocol.get_rid req in
      Xs_protocol.Response.(print (Error "ENOTSUP") tid rid) |> PS.send ps ;
      loop_forever ps

let server () =
  Logs.info (fun m -> m "Waiting for commands on %s" name) ;
  let io = ServerIO.create () in
  let ps = PS.make io in
  at_exit (fun () -> ServerIO.destroy io) ;
  loop_forever ps

let memuse c =
  Client.transaction_one_try c @@ fun handle ->
  let module XSAllocatorKey = struct
    type t = {handle: Client.handle; mutable counter: int}

    (* reduce client-side memory usage: do not store keys, since we can always rebuild them *)
    type item = int

    let empty = 0

    let encode_item t n =
      assert (n > 0 && n <= 0xffff) ;
      t.counter <- t.counter + 1 ;
      (t.counter lsl 16) lor n

    let word = Sys.word_size / 8

    let minsize = 16

    let prefix = "data/"

    let maxsize = 1024 - String.length prefix

    let key_of_item idx =
      let size = idx land 0xffff in
      let count = idx lsr 16 in
      let cntstr = string_of_int count in
      let strsize = (((size / word) - 1) * word) - String.length cntstr in
      assert (strsize > 0) ;
      (* TODO: probe writable paths *)
      (* builds a unique string of length 'size' after data/
         by embedding a counter.
         Minsize ensures we'll have plenty of room for the digits
      *)
      prefix ^ cntstr ^ string_make strsize 'x'

    let alloc t n =
      if n < minsize || n > maxsize then
        None
      else
        let item = encode_item t n in
        let k = key_of_item item in
        (* TODO: use max alloc of value... *)
        try
          (* TODO: how to detect endconn? *)
          (* Client.mkdir t.handle k; *)
          Client.immediate c (fun h -> Client.mkdir h k) ;
          Some item
        with
        | Xs_protocol.Error "EQUOTA" ->
            None
        | e ->
            raise e

    let dealloc _t item =
      let k = key_of_item item in
      Client.immediate c (fun h -> Client.rm h k)
    (* Client.rm t.handle k *)

    let init () = {handle; counter= 1}

    let finish _ = ()
  end in
  (* TODO: using more than 1 tx requires modifying alloc interface to be a with_ style *)
  let module KeyA = Memuse.ProbeIndexedAllocator (XSAllocatorKey) in
  let module WC = Memuse.WorstCase (KeyA) in
  let module MAX = Memuse.MaxAlloc (KeyA) in
  Memuse.run (module WC)
(* TODO: choose with enum Memuse.run (module MAX) *)

let client () =
  Logs.debug (fun m -> m "initializing client") ;
  let c = Client.make () in
  Client.set_logger (fun s -> Logs.debug @@ fun m -> m "xenstore-client: %s" s) ;
  if flag_memuse then
    memuse c
  else
    (* Client.immediate c (fun h ->
         Logs.debug (fun m -> m "domainpath: %s" (Client.getdomainpath h 1)
         )
       );*)
    (* due to Bos we have Astring here *)
    let key_prefix = String.v ~len:1024 (fun _ -> 'y') in
    let value_prefix = String.v ~len:2048 (fun _ -> 'v') in
    let i = ref 0 in
    Client.transaction c (fun h ->
        (* we could probe size here? *)
        while true do
          incr i ;
          (* TODO: discover writable paths basedon list *)
          let key = Printf.sprintf "data/%s%d" key_prefix !i in
          let key_enoent = Printf.sprintf "data/no%s%d" key_prefix !i in
          let value = Printf.sprintf "%s%d" value_prefix !i in
          (* TODO: could also trigger unique watch events of various sizes Robson style
             to consume even more memory
             we'll probably be forced to implement as a set..
          *)
          Client.write h key value ;
          (* this will have to get stored in the replay queue so that after a
             conflict reply can be checked to match *)
          ( try
              let (_ : string) = Client.read h key_enoent in
              ()
            with Xs_protocol.Enoent _ -> ()
          ) ;
          Client.rm h key
        done
    )

(* xs_client_unix uses threads, have to set mutex on logger,
   use Logs_threaded.enable ()
   but only when using builtin reporter, Tracedebug's doesn't need it
*)

let () =
  Sys.catch_break true ;
  Fmt_tty.setup_std_outputs ~style_renderer:`None () ;
  OS.Arg.parse_opts () ;
  (* if debug then Logs.set_level (Some Logs.Debug)
  else Logs.set_level (Some Logs.Info); *)
  Logs.info (fun m -> m "PID %d" (Unix.getpid ())) ;
  if flag_client then client () else server ()
