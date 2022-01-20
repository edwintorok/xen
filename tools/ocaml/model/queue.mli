type 'a t
(*@ model contents: 'a seq *)

(*@ predicate is_empty(q: 'a t) = Seq.length q.contents = 0 *)

(*@ function length(q: 'a t) : integer = Seq.length q.contents *)

exception Empty

val pop: 'a t -> 'a
(*@ v = pop q
    modifies q
    ensures old q.contents = Seq.snoc q.contents v
    raises Empty -> q.contents = old q.contents = Seq.empty
*)

