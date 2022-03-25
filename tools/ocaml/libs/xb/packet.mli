type t = { tid : int; rid : int; ty : Op.operation; data : string; }
exception Error of string
exception DataError of string

val create : int -> int -> Op.operation -> string -> t
(*@ t = create tid rid ty data
    ensures t.tid = tid && t.rid = rid && t.ty = ty && t.data = data *)

val of_partialpkt : Partial.pkt -> t
(*@ t = of_partialpkt p
    ensures t.data = p.Partial.buf.Buffer.contents *)
    (*ensures t.tid = p.tid && t.rid = p.rid && t.ty = p.ty && t.data = p.buf.contents *)

val to_string : t -> string
(*@ s = to_string t
    ensures String.length s = String.length t.data + Partial.headersize
*)

val unpack : t -> int * int * Op.operation * string
(*@ tid, rid, ty, data = unpack t
    ensures tid = t.tid && rid = t.rid && ty = t.ty && data = t.data *)

val get_tid : t -> int
(*@ tid = get_tid t
    ensures tid = t.tid *)

val get_ty : t -> Op.operation
(*@ ty = get_ty t
    ensures ty = t.ty *)

val get_data : t -> string
(*@ data = get_data t
    ensures data = t.data *)

val get_rid : t -> int
(*@ rid = get_rid t
    ensures rid = t.rid *)
