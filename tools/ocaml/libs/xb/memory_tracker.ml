open Sizeops
let value = Size.of_words 0
module Tracker = struct

  type t = Size.t (* the sum *)

  let empty = Size.of_int 0

  let add acc x = Size.(acc + x)

  let remove acc x = Size.(acc - x)
end

module Record = struct
  module Immutable = struct
    type t = Size.t
    let empty = value
    let add_field acc size_of x =
      let n = size_of x in
      Size.(acc + value + n)
  end

  module Mutable = struct
    type t =
      { mutable size: Immutable.t
      ; mutable parent: t option
      }

    let create fields = { size = Size.of_words fields; parent = None }

    let size_of t = t.size

    let set_parent t ~parent =
      match t.parent with
      | None -> t.parent <- Some parent
      | Some _ -> invalid_arg "mutable records can only have one parent"

    let rec add n t =
      t.size <- Size.(t.size + n);
      Option.iter (add n) t.parent

    let rec sub n t =
      t.size <- Size.(t.size - n);
      Option.iter (sub n) t.parent

    let clear t =
      sub t.size t

    let add_field t size_of ~field =
      add Size.(value + size_of field) t

    let add_mutable_field t ~field =
      set_parent field ~parent:t;
      add_field t size_of ~field

    let add_mutable_entry t key_size_of key ~entry =
      add_field t key_size_of ~field:key;
      add_mutable_field t ~field:entry

    let remove_mutable_entry t key_size_of key_size ~entry =
      sub Size.(value + key_size_of key_size) t;
      entry.parent <- None;
      sub entry.size t
  end

  module Ref = struct
    type 'a t =
      { tracker: Mutable.t
      ; size_of: 'a -> Size.t
      ; mutable v: 'a
      (* we normally don't allow mutable fields in tracked records,
         except internally here *)
      }

    let ref size_of v =
      let r = { tracker = Mutable.create 3; size_of; v } in
      Mutable.add_field r.tracker size_of ~field:v;
      r

  let (:=) t v =
    let tracker = t.tracker in
      Mutable.clear tracker;
      Mutable.add (t.size_of v) tracker;
      t.v <- v

  let (!) t = t.v

  end
end

module Queue = struct
  type 'a t =
    { q: 'a Queue.t
    ; tracker: Record.Mutable.t
    ; size_of: 'a -> Size.t
    }

  let create_sized size_of =
    let tracker = Record.Mutable.create 3 in
    { q = Queue.create (); tracker; size_of }

  let item_overhead = Size.of_int 3

  let add e t =
    let size = Size.(t.size_of e + item_overhead) in
    (* get_size is first in case it raises exceptions *)

    Queue.add e t.q;
    Record.Mutable.add size t.tracker

  let push = add

  let take t =
    let size = t.size_of (Queue.peek t.q) in
    (* get_size is first in case it raises exceptions *)

    let e = Queue.take t.q in
    Record.Mutable.sub size t.tracker;
    e

  let pop = take

  let peek t = Queue.peek t.q

  let top = peek

  let clear t =
    Queue.clear t.q;
    Record.Mutable.clear t.tracker

  let is_empty t = Queue.is_empty t.q

  let copy t = { t with q = Queue.copy t.q }

  let length t = Queue.length t.q

  let iter f t = Queue.iter f t.q

  let fold f init t = Queue.fold f init t.q

  let transfer src dst =
    Queue.transfer src.q dst.q;
    Record.Mutable.add src.tracker.size dst.tracker;
    Record.Mutable.clear src.tracker
end

module Hashtbl = struct
  type ('a, 'b) t =
    { h: ('a, 'b) Hashtbl.t
    ; tracker: Record.Mutable.t
    ; get_key_size: 'a -> Size.t
    ; get_value_size: 'b -> Record.Mutable.t
    }

  (* note about randomized hashtbl:
    would need something like Siphash for this to improve security,
    but lets get ready for when that PR is merged: https://github.com/ocaml/ocaml/pull/9764
  *)

  let create_sized get_key_size get_value_size n =
    { h = Hashtbl.create ~random:true n
    ; tracker = Record.Mutable.create 4
    ; get_key_size
    ; get_value_size
    }

  let reset t =
    Hashtbl.reset t.h;
    Record.Mutable.clear t.tracker

  (* [clear] is not implemented: it doesn't shrink the table size,
     thus prone to space leaks *)

  let copy t = { t with h = Hashtbl.copy t.h }

  (* [add] is not implemented on purpose: it doesn't remove previous bindings,
     but simply hides them, thus prone to space leaks *)
  let remove t k =
    begin try
      let prev = Hashtbl.find t.h k in
      let value_tracker = t.get_value_size prev in
      Record.Mutable.remove_mutable_entry t.tracker t.get_key_size k ~entry:value_tracker
    with Not_found -> ()
    end;
    Hashtbl.remove t.h k

  let replace t k data =
    remove t k;
    let value_tracker = t.get_value_size data in
    Hashtbl.replace t.h k data;
    Record.Mutable.add_mutable_entry t.tracker t.get_key_size k ~entry:value_tracker

  let find t k = Hashtbl.find t.h k

  let find_all t k = Hashtbl.find_all t.h k

  let mem t k = Hashtbl.mem t.h k

  let iter f t = Hashtbl.iter f t.h

  let fold f t init = Hashtbl.fold f t.h init

  let length t = Hashtbl.length t.h

  (* needs to be O(1) to avoid O(N^2) complexity in packet processing loop *)
  (*let size_of t =
    let stats = Hashtbl.stats t.h in
    (* a constant time approximation *)
    let overhead = stats.Hashtbl.max_bucket_length * stats.Hashtbl.num_buckets * 2 in
    TODO: we're not tracking this overhead for now...
    Size.(MutableTracker.size t.size + of_int overhead)*)
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
