open Sizeops
module Tracker = struct

  type t = Size.t (* the sum *)

  let empty = Size.of_int 0

  let add acc x = Size.(acc + x)

  let remove acc x = Size.(acc - x)

  let add_opt acc x =
    let t = Size.(acc + x) in
    match Size.to_int_opt t with
    | None -> None
    | Some _ -> Some t
end

module Queue = struct
  type 'a t =
    { q: 'a Queue.t
    ; mutable size: Tracker.t
    ; get_size: 'a -> Size.t
    }

  let create_sized get_size = { q = Queue.create (); size = Tracker.empty; get_size }

  let add e t =
    let size = t.get_size e in
    (* get_size is first in case it raises exceptions *)

    Queue.add e t.q;
    t.size <- Tracker.add t.size size

  let push = add

  let take t =
    let size = t.get_size (Queue.peek t.q) in
    (* get_size is first in case it raises exceptions *)

    let e = Queue.take t.q in
    t.size <- Tracker.remove t.size size;
    e

  let pop = take

  let peek t = Queue.peek t.q

  let top = peek

  let clear t =
    Queue.clear t.q;
    t.size <- Tracker.empty

  let is_empty t = Queue.is_empty t.q

  let copy t = { t with q = Queue.copy t.q }

  let length t = Queue.length t.q

  let iter f t = Queue.iter f t.q

  let fold f init t = Queue.fold f init t.q

  let transfer src dst =
    Queue.transfer src.q dst.q;
    dst.size <- Tracker.add dst.size src.size;
    src.size <- Tracker.empty

  (* needs to be O(1) to avoid O(N^2) complexity in packet processing loop *)
  let size_of t =
    let overhead = Queue.length t.q * 3 + 2 in
    Size.(t.size + of_int overhead)
end

module Hashtbl = struct
  type ('a, 'b) t =
    { h: ('a, 'b) Hashtbl.t
    ; mutable size: Tracker.t
    ; get_key_size: 'a -> Size.t
    ; get_value_size: 'b -> Size.t
    }

  (* note about randomized hashtbl:
    would need something like Siphash for this to improve security,
    but lets get ready for when that PR is merged: https://github.com/ocaml/ocaml/pull/9764
  *)

  let create_sized get_key_size get_value_size n =
    { h = Hashtbl.create ~random:true n
    ; size = Tracker.empty
    ; get_key_size
    ; get_value_size
    }

  let reset t =
    Hashtbl.reset t.h;
    t.size <- Tracker.empty

  (* [clear] is not implemented: it doesn't shrink the table size,
     thus prone to space leaks *)

  let copy t = { t with h = Hashtbl.copy t.h }

  (* [add] is not implemented on purpose: it doesn't remove previous bindings,
     but simply hides them, thus prone to space leaks *)
  let remove t k =
    begin try
      let prev = Hashtbl.find t.h k in
      let size = Size.(t.get_key_size k + t.get_value_size prev) in
      t.size <- Tracker.remove t.size size
    with Not_found -> ()
    end;
    Hashtbl.remove t.h k

  let replace t k data =
    remove t k;
    let size = Size.(t.get_key_size k + t.get_value_size data) in
    Hashtbl.replace t.h k data;
    t.size <- Tracker.add t.size size

  let find t k = Hashtbl.find t.h k

  let find_all t k = Hashtbl.find_all t.h k

  let mem t k = Hashtbl.mem t.h k

  let iter f t = Hashtbl.iter f t.h

  let fold f t init = Hashtbl.fold f t init

  let length t = Hashtbl.length t

  (* needs to be O(1) to avoid O(N^2) complexity in packet processing loop *)
  let size_of t =
    let stats = Hashtbl.stats t.h in
    (* a constant time approximation *)
    let overhead = stats.Hashtbl.max_bucket_length * stats.Hashtbl.num_buckets * 2 in
    Size.(t.size + of_int overhead)
end

module List = struct
  type 'a t =
    { l: 'a list
    ; mutable size: Tracker.t
    ; mutable length: int
    ; get_size: 'a -> Size.t
    }

  let empty get_size = { l = []; size = Tracker.empty; length = 0; get_size }
  let cons a t = { t with l = a :: t.l; size = Tracker.add t.size (t.get_size a); length = t.length + 1}
  let length t = t.length
  let hd t = List.hd t.l
  let tl t = List.tl t.l
  let rev t = { t with l = List.rev t.l }
  let rev_append l1 l2 =
    assert (l1.get_size == l2.get_size);
    { l = List.rev_append l1.l l2.l
    ; size = Tracker.add l1.size l2.size
    ; length = l1.length + l2.length
    ; get_size = l1.get_size }
  let iter f t = List.iter f t.l
  let rev_map get_size f t =
    let l = List.rev_map f t.l in
    { l
    ; size = List.fold_left Tracker.add Tracker.empty l
    ; length = t.length
    ; get_size }

  let fold_left f init t = List.fold_left f init t.l
  (* TODO: more list functions as needed *)
end

let size_of_string s = Size.of_bytes (String.length s)
let size_of_bytes s = Size.of_bytes (Bytes.length s)

let value = Size.of_words 0
let size_of_option size_of_element = function
  | None -> value
  | Some x -> Size.(value + size_of_element x)

(* TODO: ortac specifications that size_of is a good approximation for Obj.reachable_size, i.e. that
   it matches exactly for queues, and it is an over-approximation for Hashtbls.
   Obj.reachable_size would be O(n) though, whereas [size_t] is meant to be O(1).
   *)
