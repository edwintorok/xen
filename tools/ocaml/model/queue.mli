type 'a t
(*@ model contents: 'a seq *)

(*@ predicate is_empty(q: 'a t) = Seq.length q.contents = 0 *)

(*@ function length(q: 'a t) : integer = Seq.length q.contents *)

