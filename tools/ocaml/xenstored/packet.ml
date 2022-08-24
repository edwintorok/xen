type request = {
	tid: int;
	rid: int;
	ty: Xenbus.Xb.Op.operation;
	data: string;
}

(* we do not use a Lazy.t here,
  because logging may force it and then we'd increase memory usage
  by storing the full value instead of just the lazy references
 *)
type reply =
| Empty
| String of string
| Path of Store.Path.t
| Perms of Perms.Node.t
| NullJoined of reply list

let rec length = function
	| Empty -> 0
	| String s -> String.length s
	| Path p -> List.fold_left (fun acc s -> acc + 1 + String.length s) 0 p
	| Perms p ->
		(* inefficient, but we have variable length numbers (domids) *)
		Perms.Node.to_string p |> String.length
	| NullJoined l ->
		let sum acc e = acc + (if acc > 0 then 1 else 0) + length e
		in
		List.fold_left sum 0 l

let rec to_string = function
	| Empty -> ""
	| String s -> s
	| Path p -> Store.Path.to_string p
	| Perms p -> Perms.Node.to_string p
	| NullJoined l -> String.concat "\x00" (List.map to_string l)

let null_terminated s =
	NullJoined [String s; Empty]

type response =
	| Ack of (unit -> unit)  (* function is the action to execute after sending the ack *)
	| Reply of reply
	| Error of string

let response_equal a b =
	match (a, b) with
	| (Ack _, Ack _) -> true (* just consider the response, not the post-response action *)
	| (x, y) -> x = y
