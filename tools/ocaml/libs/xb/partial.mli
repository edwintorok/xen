type pkt = {
  tid : int;
  rid : int;
  ty : Op.operation;
  len : int;
  buf : Buffer.t;
}
external header_size : unit -> int = "stub_header_size"
val xenstore_payload_max : int
val xenstore_rel_path_max : int
val of_string : string -> pkt
val append : pkt -> string -> int -> unit
val to_complete : pkt -> int
