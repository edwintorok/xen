(*@ open Seq *)

(* TODO: use just ortac, cameleer can't prove this yet *)

module type Queue = sig
  type 'a t
  (*@ mutable model view: 'a seq *)

  val create : unit -> 'a t
  (*@ q = create ()
      pure
      ensures q.view = Seq.empty *)

  val add: 'a -> 'a t -> unit
  (*@ add v q
      modifies q.view
      ensures q.view = Seq.snoc (old q.view) v *)

  val[@logic] fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (*@ r = fold f init q
      pure
   *)

  (*@ function get_view(x: 'a t) : 'a seq = x.view *)
end

(*** from sum.ml ****)
(*@ function logic_sum (f: int -> int) (l: int) (u: int) : integer *)

(*@ axiom logic_sum_0: forall f l u. u <= l -> logic_sum f l u = 0 *)
(*@ axiom logic_sum_n: forall f l u. u > l ->
      logic_sum f l u = logic_sum f l (u-1) + f (u-1) *)

(*@ lemma logic_sum_left:
      forall f: (int -> int), a b: int.
      a < b -> logic_sum f a b = f a + logic_sum f (a + 1) b *)

(*@ lemma logic_sum_ext:
      forall f g: (int -> int), a b: int.
      (forall i. a <= i < b -> f i = g i) ->
      logic_sum f a b = logic_sum g a b *)

(*@ lemma logic_sum_le:
      forall f g: (int -> int), a b: int.
      (forall i. a <= i < b -> f i <= g i) ->
      logic_sum f a b <= logic_sum g a b *)

(*@ lemma logic_sum_zero:
      forall f: (int -> int), a b: int.
      (forall i. a <= i < b -> f i = 0) ->
      logic_sum f a b = 0 *)

(*@ lemma logic_sum_nonneg:
      forall f: (int -> int), a b: int.
      (forall i. a <= i < b -> 0 <= f i) ->
      0 <= logic_sum f a b *)

(*@ lemma logic_sum_decomp:
      forall f: (int -> int), a b c: int. a <= b <= c ->
      logic_sum f a c = logic_sum f a b + logic_sum f b c *)
(****)

(*@ function seq_size(s: 'a seq)(f:'a -> int): integer =
  logic_sum (fun i -> f s[i]) 0 (length s) *)

type 'a t =
  { mutable size_bytes: int
  ; q: 'a Queue.t
  ; f: 'a -> int
  }
(*@ invariant seq_size (Queue.get_view q) f = size_bytes *)


let create f = { size_bytes = 0; q = Queue.create (); f }
(*@ t = create ()
    pure *)

let add x t =
  Queue.add x t.q;
  t.size_bytes <- t.size_bytes + t.f x
(*@ add x t
    modifies t.q
    modifies t.size_bytes
    *)
