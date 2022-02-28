open Sizeops
let value = Size.of_words 0
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

module MutableTracker = struct
  type t = {
    mutable t: Tracker.t;
    mutable parent: t option
  }

  let empty () = { t = Tracker.empty; parent = None }
  let of_tracker t = { t ; parent = None }
  let set_parent t ~parent = t.parent <- Some parent
  let remove_parent t = t.parent <- None
  let size t = t.t
  let rec add t n =
    t.t <- Tracker.add t.t n;
    Option.iter (fun parent -> add parent n) t.parent

  let add_mutable t n e =
    add t n;
    set_parent e ~parent:t

  let rec remove t n =
    t.t <- Tracker.remove t.t n;
    Option.iter (fun parent -> remove parent n) t.parent

  let remove_mutable t n e =
    remove_parent e;
    t.t <- Tracker.remove t.t n

  let clear t = t.t <- Tracker.empty

  (* TODO: set/remove parent is the one that should add/remove *)
end

module Record = struct
  let field_size size_of f = Size.(size_of f + value)
  module Immutable = struct
    type t = Tracker.t

    let size_of compute record = compute Tracker.empty

    let immutable_field f size_of acc = Tracker.add acc (field_size size_of f)
  end
  module Mutable = struct
    type t = MutableTracker.t

    let register _record compute =
      let t = MutableTracker.empty () in
      MutableTracker.add t value; (* the tracker field itself *)
      compute t;
      t

    let size_of tracker_of record = MutableTracker.size (tracker_of record)

    let immutable_field f size_of acc = MutableTracker.add acc (field_size size_of f)
    let mutable_field f size_of tracker_of acc = MutableTracker.add_mutable acc (field_size size_of f) (tracker_of f); acc
  end
end

module Queue = struct
  type 'a t =
    { q: 'a Queue.t
    ; size: MutableTracker.t
    ; get_size: 'a -> Size.t
    }

  let create_sized get_size = { q = Queue.create (); size = MutableTracker.empty (); get_size }

  let add e t =
    let size = t.get_size e in
    (* get_size is first in case it raises exceptions *)

    Queue.add e t.q;
    MutableTracker.add t.size size

  let push = add

  let take t =
    let size = t.get_size (Queue.peek t.q) in
    (* get_size is first in case it raises exceptions *)

    let e = Queue.take t.q in
    MutableTracker.remove t.size size;
    e

  let pop = take

  let peek t = Queue.peek t.q

  let top = peek

  let clear t =
    Queue.clear t.q;
    MutableTracker.clear t.size

  let is_empty t = Queue.is_empty t.q

  let copy t = { t with q = Queue.copy t.q }

  let length t = Queue.length t.q

  let iter f t = Queue.iter f t.q

  let fold f init t = Queue.fold f init t.q

  let transfer src dst =
    Queue.transfer src.q dst.q;
    MutableTracker.add dst.size (MutableTracker.size src.size);
    MutableTracker.clear src.size

  (* needs to be O(1) to avoid O(N^2) complexity in packet processing loop *)
  let size_of t =
    let overhead = Queue.length t.q * 3 + 2 in
    Size.(MutableTracker.size t.size + of_int overhead)
end

module Hashtbl = struct
  type ('a, 'b) t =
    { h: ('a, 'b) Hashtbl.t
    ; size: MutableTracker.t
    ; get_key_size: 'a -> Size.t
    ; get_value_size: 'b -> MutableTracker.t
    }

  (* note about randomized hashtbl:
    would need something like Siphash for this to improve security,
    but lets get ready for when that PR is merged: https://github.com/ocaml/ocaml/pull/9764
  *)

  let create_sized get_key_size get_value_size n =
    { h = Hashtbl.create ~random:true n
    ; size = MutableTracker.empty ()
    ; get_key_size
    ; get_value_size
    }

  let reset t =
    Hashtbl.reset t.h;
    MutableTracker.clear t.size

  (* [clear] is not implemented: it doesn't shrink the table size,
     thus prone to space leaks *)

  let copy t = { t with h = Hashtbl.copy t.h }

  (* [add] is not implemented on purpose: it doesn't remove previous bindings,
     but simply hides them, thus prone to space leaks *)
  let remove t k =
    begin try
      let prev = Hashtbl.find t.h k in
      let value_tracker = t.get_value_size prev in
      let size = Size.(t.get_key_size k + MutableTracker.size value_tracker) in
      Printf.eprintf "hashtbl, removing %d\n" (Option.value ~default:(-1) (Size.to_int_opt size));
      MutableTracker.remove_mutable t.size size value_tracker
    with Not_found -> ()
    end;
    Hashtbl.remove t.h k

  let replace t k data =
    remove t k;
    let value_tracker = t.get_value_size data in
    let size = Size.(t.get_key_size k + MutableTracker.size value_tracker) in
    Hashtbl.replace t.h k data;
      Printf.eprintf "hashtbl, adding %d\n" (Option.value ~default:(-1) (Size.to_int_opt size));
    MutableTracker.add_mutable t.size size value_tracker

  let find t k = Hashtbl.find t.h k

  let find_all t k = Hashtbl.find_all t.h k

  let mem t k = Hashtbl.mem t.h k

  let iter f t = Hashtbl.iter f t.h

  let fold f t init = Hashtbl.fold f t.h init

  let length t = Hashtbl.length t.h

  (* needs to be O(1) to avoid O(N^2) complexity in packet processing loop *)
  let size_of t =
    let stats = Hashtbl.stats t.h in
    (* a constant time approximation *)
    let overhead = stats.Hashtbl.max_bucket_length * stats.Hashtbl.num_buckets * 2 in
    Size.(MutableTracker.size t.size + of_int overhead)
end

module List = struct
  type 'a t =
    { l: 'a list
    ; size: Tracker.t
    ; length: int
    ; get_size: 'a -> Size.t
    }

  let of_list get_size l =
    let size = List.fold_left (fun acc e -> Tracker.add acc (get_size e)) Tracker.empty l in
    { l; get_size; length = List.length l; size }

  let filter f t = of_list t.get_size (List.filter f t.l)

  let empty get_size = { l = []; size = Tracker.empty; length = 0; get_size }
  let cons a t = { t with l = a :: t.l; size = Tracker.add t.size (t.get_size a); length = t.length + 1}
  let length t = t.length
  let hd t = List.hd t.l
  let tl t =
    let l = List.tl t.l in
    { l
    ; size = Tracker.remove t.size (t.get_size (List.hd t.l))
    ; length = t.length - 1
    ; get_size = t.get_size
    }


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
    ; size = List.fold_left (fun acc e -> Tracker.add acc (get_size e)) Tracker.empty l
    ; length = t.length
    ; get_size }

  let fold_left f init t = List.fold_left f init t.l
  (* TODO: more list functions as needed *)

  let size_of t = t.size

  let for_all f t = List.for_all f t.l
  let exists f t = List.exists f t.l
  let find e t = List.find e t.l

  let rev t =
    { t with l = List.rev t.l }

  let is_empty t = match t.l with [] -> true | _ -> false

end

let size_of_string s = Size.of_bytes (String.length s)
let size_of_bytes s = Size.of_bytes (Bytes.length s)


let size_of_int _ = value
let size_of_option size_of_element = function
  | None -> value
  | Some x -> Size.(value + size_of_element x)

(* TODO: ortac specifications that size_of is a good approximation for Obj.reachable_size, i.e. that
   it matches exactly for queues, and it is an over-approximation for Hashtbls.
   Obj.reachable_size would be O(n) though, whereas [size_t] is meant to be O(1).
   *)
