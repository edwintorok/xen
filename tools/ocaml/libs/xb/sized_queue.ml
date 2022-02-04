type 'a t =
  { q: 'a Queue.t
  ; mutable size: Sizetracker.Relaxed.t
  ; size_of: 'a -> int
  }

let create size_of = { size = Sizetracker.Relaxed.empty; size_of; q = Queue.create () }

let clear t =
  Queue.clear t.q;
  t.size <- Sizetracker.Relaxed.empty

let is_empty t = Queue.is_empty t.q

let length t = Queue.length t.q

let peek t = Queue.peek t.q

let pop t =
  let r = Queue.pop t.q in
  t.size <- Sizetracker.Relaxed.remove (t.size_of r) t.size;
  r

let push x t =
  t.size <- Sizetracker.Relaxed.add (t.size_of x) t.size;
  Queue.push x t.q

let size t = t.size
