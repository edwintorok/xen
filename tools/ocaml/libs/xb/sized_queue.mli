type 'a t

type 'a size_of = 'a -> Size_tracker.t

val create : 'a size_of -> 'a t

val size : _ t -> Size_tracker.t

val push : 'a -> 'a t -> unit

val peek : 'a t -> 'a

val pop : 'a t -> 'a

val clear : _ t -> unit

val is_empty : _ t -> bool

val length : _ t -> int

val iter : ('a -> unit) -> 'a t -> unit

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a