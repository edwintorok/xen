type 'a t = {
    q: 'a Queue.t
  ; size_of: 'a -> Size_tracker.t
  ; mutable size: Size_tracker.t
}

type 'a size_of = 'a -> Size_tracker.t

let create size_of = {q= Queue.create (); size_of; size= Size_tracker.empty}

let size t = t.size

let push e t =
  let delta = t.size_of e in
  Queue.add e t.q ;
  t.size <- Size_tracker.add t.size delta

let pop t =
  let r = Queue.pop t.q in
  t.size <- Size_tracker.remove t.size @@ t.size_of r ;
  r

let peek t = Queue.peek t.q

let clear t =
  Queue.clear t.q ;
  t.size <- Size_tracker.empty

let is_empty t = Queue.is_empty t.q

let length t = Queue.length t.q

let iter f t = Queue.iter f t.q

let fold f init t = Queue.fold f init t.q
