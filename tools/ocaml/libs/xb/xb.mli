(*@ open Stdlib *)
module Op :
  sig
    type operation =
      Op.operation =
        Debug
      | Directory
      | Read
      | Getperms
      | Watch
      | Unwatch
      | Transaction_start
      | Transaction_end
      | Introduce
      | Release
      | Getdomainpath
      | Write
      | Mkdir
      | Rm
      | Setperms
      | Watchevent
      | Error
      | Isintroduced
      | Resume
      | Set_target
      | Reset_watches
      | Invalid
    val size : int
    val of_cval : int -> operation
    val to_cval : operation -> int
    val to_string : operation -> string
  end
module Packet :
  sig
    type t =
      Packet.t = {
      tid : int;
      rid : int;
      ty : Op.operation;
      data : string;
    }
    exception Error of string
    exception DataError of string
    val create : int -> int -> Op.operation -> string -> t
    val of_partialpkt : Partial.pkt -> t
    val to_string : t -> string
    val unpack : t -> int * int * Op.operation * string
    (*@ tid, rid, ty, data = unpack t *)

    val get_tid : t -> int
    val get_ty : t -> Op.operation
    val get_data : t -> string
    val get_rid : t -> int
  end
(* TODO: move annotations from individual other mlis here, and test only this? *)
exception End_of_file
exception Eagain
exception Noent
exception Invalid
exception Reconnect
type backend_mmap = {
  mmap : Xenmmap.mmap_interface;
  eventchn_notify : unit -> unit;
  mutable work_again : bool;
}
type backend_fd = { fd : Unix.file_descr; }
type backend = Fd of backend_fd | Xenmmap of backend_mmap

(*@ predicate is_mmap_backend(b: backend) = match b with
      | Fd _ -> false
      | Xenmmap _ -> true *)

type partial_buf = HaveHdr of Partial.pkt | NoHdr of int * bytes

(*@ predicate is_valid_partial_buf(b: partial_buf) = match b with
    | HaveHdr _ -> true
    | NoHdr (len, b) ->
          Bytes.length b = Partial.headersize
          && 0 <= len <= Partial.headersize
 *)

(*@ predicate is_empty_partial_buf(b: partial_buf) = match b with
    | HaveHdr _ -> false
    | NoHdr (len, _) -> len = Partial.headersize *)

(*@ function nohdr_size(b: partial_buf) : integer = match b with
  | HaveHdr _ -> 0
  | NoHdr (len, _) -> len *)

type t = {
  backend : backend;
  pkt_in : Packet.t Queue.t;
  pkt_out : Packet.t Queue.t;
  mutable partial_in : partial_buf;
  mutable partial_out : string;
}
(*@ invariant is_valid_partial_buf(partial_in) *)

(*@ predicate is_empty(t: t) =
     Queue.is_empty t.pkt_in && Queue.is_empty t.pkt_out
     && is_empty_partial_buf t.partial_in
     && String.length t.partial_out = 0 *)

val init_partial_in : unit -> partial_buf
(*@ b = init_partial_in ()
    ensures is_valid_partial_buf b
    ensures is_empty_partial_buf b
    *)

val reconnect : t -> unit
(*@ reconnect t
    requires is_mmap_backend t.backend
    modifies t
    ensures is_empty t
 *)

val queue : t -> Packet.t -> unit
(*@ queue t p
    modifies t
 *)

val read_fd : backend_fd -> 'a -> bytes -> int -> int
(*@ n = read_fd fd c b len
    ensures n <> 0
    raises End_of_file -> true *)

val read_mmap : backend_mmap -> 'a -> bytes -> int -> int
(*@ n = read_mmap back c b len
    modifies back.work_again
    ensures back.work_again <-> (n > 0)
  *)

val read : t -> bytes -> int -> int
(*@ n = read t b len *)
(* TODO *)

val write_fd : backend_fd -> 'a -> string -> int -> int
(*@ n = write_fd back c b len *)

val write_mmap : backend_mmap -> 'a -> string -> int -> int
(*@ n = write_mmap back c b len *)

val write : t -> string -> int -> int
(*@ n = write t s len *)
(* TODO *)

val output : t -> bool
(*@ r = output t
    modifies t
    ensures r <-> (String.length t.partial_out = 0)
  *)

val input : t -> bool
(*@ r = input t
    modifies t
*)

val newcon : backend -> t
(*@ t = newcon back
    ensures t.backend = back
    ensures is_empty t *)

val open_fd : Unix.file_descr -> t
(*@ t = open_fd fd
    ensures t.backend = Fd { fd }
    ensures is_empty t *)

val open_mmap : Xenmmap.mmap_interface -> (unit -> unit) -> t
(*@ t = open_mmap mmap eventchn_notify
    ensures t.backend = Xenmmap { mmap; eventchn_notify; work_again = false }
    ensures is_empty t *)


val close : t -> unit
(*@ close t
    consumes t *)

val is_fd : t -> bool
(*@ r = is_fd t
    ensures r = not is_mmap_backend t.backend *)

val is_mmap : t -> bool
(*@ r = is_mmap t
    ensures r = is_mmap_backend t.backend *)

val output_len : t -> int
(*@ r = output_len t
    pure
    ensures r = Queue.length t.pkt_out *)

val has_new_output : t -> bool
(*@ r = has_new_output t
    pure
    ensures r <-> output_len t > 0 *)

val has_old_output : t -> bool
(*@ r = has_old_output t
    pure
    ensures r <-> String.length t.partial_out > 0 *)

val has_output : t -> bool
(*@ r = has_output t
    pure
    ensures r = not (has_new_output t || has_old_output t) *)

val peek_output : t -> Packet.t
(*@ p = peek_output t
    requires has_output t
 *)

val input_len : t -> int
(*@ r = input_len t
    pure
    ensures r = Queue.length t.pkt_in *)

val has_in_packet : t -> bool
(*@ r = has_in_packet t
    pure
    ensures r <-> input_len t > 0 *)

val get_in_packet : t -> Packet.t
(*@ p = get_in_packet t
    requires has_in_packet t
    modifies t
*)
(* TODO *)

val has_more_input : t -> bool
(*@ r = has_more_input t *)
(* TODO *)

val is_selectable : t -> bool
(*@ r = is_selectable t
    ensures r = not is_mmap_backend t.backend *)

val get_fd : t -> Unix.file_descr
(*@ r = get_fd t
    requires not is_mmap_backend t.backend *)
