type t = { tid : int; rid : int; ty : Op.operation; data : string; }
(*@ invariant String.length data <= Partial.xenstore_payload_max *)
(*@ invariant tid >= 0 && rid >= 0 *)

exception Error of string
exception DataError of string

val create : int -> int -> Op.operation -> string -> t
(*@ t = create tid rid ty data
    requires tid >= 0 && rid >= 0 && String.length data <= Partial.xenstore_payload_max
    ensures t.tid = tid && t.rid = rid && t.ty = ty && t.data = data *)

val of_partialpkt : Partial.pkt -> t
(*@ t = of_partialpkt p
    ensures t.data = p.Partial.buf.Buffer.contents
    ensures t.tid = p.Partial.tid && t.rid = p.Partial.rid && t.ty = p.Partial.ty
*)

val to_string : t -> string
(*@ s = to_string t
    ensures String.length s = String.length t.data + Partial.headersize
*)

val unpack : t -> int * int * Op.operation * string
(*@ tid, rid, ty, data = unpack t
    ensures tid = t.tid && rid = t.rid && ty = t.ty && data = t.data *)

val get_tid : t -> int
(*@ tid = get_tid t
    ensures tid = t.tid && tid >= 0
*)

val get_ty : t -> Op.operation
(*@ ty = get_ty t
    ensures ty = t.ty *)

val get_data : t -> string
(*@ data = get_data t
    ensures data = t.data && String.length data <= Partial.xenstore_payload_max
    *)

val get_rid : t -> int
(*@ rid = get_rid t
    ensures rid = t.rid && rid >= 0
*)

val size_bytes: t -> int
(*@ n = size_bytes t
    pure
    ensures n = String.length t.data + Partial.headersize *)
