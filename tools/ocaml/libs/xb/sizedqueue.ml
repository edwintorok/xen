type 'a t =
  { q: 'a Queue.t
  ; mutable size: Sizetracker.t
  ; size_of: 'a -> int
  }


let create_exn size_of limit = { size = Sizetracker.create_exn limit; size_of; q = Queue.create () }

let clear t =
  Queue.clear t.q;
  t.size <- Sizetracker.clear t.size

let is_empty t = Queue.is_empty t.q

let length t = Queue.length t.q

let peek t = Queue.peek t.q

let pop t =
  let r = Queue.pop t.q in
  t.size <- Sizetracker.remove (t.size_of r) t.size;
  r

let valid = Some ()

let push x t =
  match Sizetracker.add (t.size_of x) t.size with
  | None -> None
  | Some size ->
      Queue.push x t.q;
      t.size <- size;
      valid
