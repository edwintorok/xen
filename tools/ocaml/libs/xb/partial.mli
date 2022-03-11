(*@ function xenstore_payload_max : integer = 4096 *)
open Memory_size_ds
type pkt = {
  tid : int;
  rid : int;
  ty : Op.operation;
  len : int;
  buf : Buffer.t;
}
(*@ invariant 0 <= len < xenstore_payload_max *)
(*@ invariant Buffer.length buf <= len *)

(*@ function headersize: integer = 16 *)
external header_size : unit -> int = "stub_header_size"
(*@ n = header_size ()
    ensures n = headersize *)

val xenstore_payload_max : int
(*@ ensures xenstore_payload_max = xenstore_payload_max *)

val xenstore_rel_path_max : int
(*@ ensures xenstore_rel_path_max = 2048 *)

val of_string : string -> pkt
(*@ p = of_string s *)

val append : pkt -> string -> int -> unit
(*@ append p s len
    requires
      (Buffer.length p.buf + len < p.len)
      && (0 <= len <= String.length s)
  *)

val to_complete : pkt -> int
(*@ n = to_complete pkt
    ensures n = pkt.len - Buffer.length pkt.buf
    ensures n >= 0
    *)

val size_of: pkt -> [> `constant | `updatable] size
