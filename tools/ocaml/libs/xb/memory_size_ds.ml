open Memory_size
open Sizeops

type 'a size = 'a t

module Container = struct
  type 'a t =
    { tracker: [`updatable] size
    ; size_of: 'a -> [`constant | `immutable] size
    }

  let container_size =
    record_start ()
    |> record_add_immutable @@ tracker @@ unit ()
    |> record_add_immutable @@ func ignore
    |> record_end

  let create ~initial ~item_overhead size_of =
    let initial = add initial container_size in
    { tracker = container_create ~initial ~item_overhead; size_of }

  let add t el = container_add_element (t.size_of el) t.tracker
  let remove t el = container_remove_element (t.size_of el) t.tracker
  let size_of t = t.size_of
  let clear t = container_clear t.tracker
  let transfer ~src ~dst = container_transfer ~src:src.tracker ~dst:dst.tracker
end

module Queue = struct
  type 'a t =
    { q: 'a Queue.t
    ; container: 'a Container.t
    }

  let initial =
    let queue_size =
      record_start ()
    |> record_add_immutable @@ int 0
    |> record_add_immutable @@ unit ()
    |> record_add_immutable @@ unit ()
    |> record_end
    in
    record_start ()
    |> record_add_immutable @@ queue_size
    |> record_add_immutable @@ tracker @@ unit ()
    |> record_end

  let item_overhead =
    record_start ()
    |> record_add_immutable @@ unit ()
    |> record_add_immutable @@ unit ()
    |> record_end
    |> size_of

  let create_sized size_of =
    let container = Container.create ~initial ~item_overhead size_of in
    { q = Queue.create (); container }

  let add e t =
    (* container operation first, in case size_of raises exceptions *)
    Container.add t.container e;
    Queue.add e t.q

  let push = add

  let take t =
    (* container operation first, in case size_of raises exceptions *)
    Container.remove t.container @@ Queue.peek t.q;
    Queue.take t.q

  let pop = take

  let peek t = Queue.peek t.q

  let top = peek

  let clear t =
    Queue.clear t.q;
    Container.clear t.container

  let is_empty t = Queue.is_empty t.q

  let copy t = { t with q = Queue.copy t.q }

  let length t = Queue.length t.q

  let iter f t = Queue.iter f t.q

  let fold f init t = Queue.fold f init t.q

  let transfer src dst =
    Queue.transfer src.q dst.q;
    Container.transfer ~src:src.container ~dst:dst.container
end

module Hashtbl = struct
  type ('a, 'b) t =
    { h: ('a, 'b) Hashtbl.t
    ; container: 'a Container.t
    ; get_value_size: 'b -> [`constant|`immutable|`updatable] size
    }

  let hashtbl_size h =
    (* there is some minimal overhead based on a hashtbl's minimum size
       that even an empty hashtbl would use *)
    let n = Hashtbl.(stats h).num_buckets in
    record_start h
    |> record_add_immutable @@ int 0
    |> record_add_immutable @@ array unit @@ Array.make n ()
    |> record_add_immutable @@ int 0
    |> record_add_immutable @@ int 0
    |> record_end

  let initial h =
    record_start ()
    |> record_add_immutable @@ hashtbl_size h
    |> record_add_immutable @@ unit ()
    |> record_add_immutable @@ unit ()
    |> record_end

  (* note about randomized hashtbl:
    would need something like Siphash for this to improve security,
    but lets get ready for when that PR is merged: https://github.com/ocaml/ocaml/pull/9764
  *)

  let item_overhead = Sizeops.Size.of_int 0 (* not a constant overhead, we can compute an approximation from stats if we have to *)

  let create_sized get_key_size get_value_size n =
    let h = Hashtbl.create ~random:true n in
    { h
    ; container = Container.create ~initial:(initial h) ~item_overhead get_key_size
    ; get_value_size
    }

  let reset t =
    Hashtbl.reset t.h;
    Container.clear t.container

  (* [clear] is not implemented: it doesn't shrink the table size,
     thus prone to space leaks *)

  let copy t = { t with h = Hashtbl.copy t.h }

  (* [add] is not implemented on purpose: it doesn't remove previous bindings,
     but simply hides them, thus prone to space leaks *)
  let remove t k =
    begin try
      let prev = Hashtbl.find t.h k in
      let value_tracker = t.get_value_size prev in
      Container.remove t.container k;
      (* TODO.. *)
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

