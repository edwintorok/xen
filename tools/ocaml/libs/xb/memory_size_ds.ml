open Memory_size
open Sizeops

type 'a size = 'a t
type require_nestable = Memory_size.require_nestable
type array_compatible = Memory_size.array_compatible
module Container = struct
  type container_size = [`updatable] size
  type 'a t =
    { tracker: container_size
    ; size_of: 'a -> require_nestable size
    }

  let container_size =
    record_start ()
    |> record_add_immutable @@ tracker @@ unit ()
    |> record_add_immutable @@ func ignore
    |> record_end

  let open_nestable x = (x: 'a -> [< require_nestable] size :> 'a -> [> require_nestable] size)

  let initial n = add n container_size

  let create ~initial ~item_overhead size_of =
    { tracker = container_create ~initial ~item_overhead; size_of = open_nestable size_of }

  let add t el =
    container_add_element (t.size_of el) t.tracker

  let remove t el = container_remove_element (t.size_of el) t.tracker

  let add_kv t key valuesize =
    add t key;
    container_add_element valuesize t.tracker

  let remove_kv t key valuesize =
    remove t key;
    container_remove_element valuesize t.tracker

  let size_of t = (t.tracker : [<`updatable] size :> [> `updatable] size)
  let clear t = container_clear t.tracker
  let transfer ~src ~dst =
    container_transfer ~src:src.tracker ~dst:dst.tracker
end

(* use name matching the one in stdlib to ensure this one is used
   and to reduce the amount of changes: only need to change Queue.create to Queue.create_sized *)
module Buffer = struct
  type t = { buf: Buffer.t; mutable bufsize: int; mutable is_initial: bool; container: int Container.t }

  let buffer_size n =
    record_start ()
    |> record_add_immutable @@ bytes_n 0
    |> record_add_immutable @@ int 0
    |> record_add_immutable @@ int 0
    |> record_add_immutable @@ bytes_n n
    |> record_end

  let initial n =
    record_start ()
    |> record_add_immutable @@ buffer_size n
    |> record_add_mutable_const @@ int 0
    |> record_add_immutable @@ unit () (* container will track its own overhead *)
    |> record_end
    |> Container.initial

  let item_overhead = Memory_size.int 0

  let create n =
    { buf = Buffer.create n; bufsize = n; is_initial = true; container = Container.create ~initial:(initial n) ~item_overhead bytes_n }

  let length t = Buffer.length t.buf

  let update_size t =
    (* the true size of Buffer's internal buffer is not exposed through its API,
       so we have to mimic the size growth that happens internally.
       the unit tests will ensure to detect if the language runtime's internal implementation
       changes and this is no longer accurate.
       *)
    if Buffer.length t.buf > t.bufsize then begin
      if t.is_initial then
        t.is_initial <- false
      else
        (* we allocate a new buffer, but if we've never grown the buffer before
           then the initial one remains allocated too.
           Only remove the bufsize if not the initial allocation *)
        Container.remove t.container t.bufsize;

      (* we will allocate a new buffer, the old one will be GCed *)
      while t.bufsize < Buffer.length t.buf do
        t.bufsize <- t.bufsize * 2;
      done;
      Container.add t.container t.bufsize
    end

  let add_char t c =
    Buffer.add_char t.buf c;
    update_size t

  let add_substring t s pos len =
    Buffer.add_substring t.buf s pos len;
    update_size t

  let reset t =
    Buffer.reset t.buf;
    (* back to initial *)
    Container.clear t.container;
    t.is_initial <- true

  let contents t = Buffer.contents t.buf
  let size_of t = Container.size_of t.container
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
    |> Container.initial

  let item_overhead =
    record_start ()
    |> record_add_immutable @@ unit ()
    |> record_add_immutable @@ unit ()
    |> record_end

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

  (* copy is not implemented: it'd require 2 parents for expressions if we allow
     copying a queue with updatable elements *)

  let length t = Queue.length t.q

  let iter f t = Queue.iter f t.q

  let fold f init t = Queue.fold f init t.q

  let transfer src dst =
    Queue.transfer src.q dst.q;
    Container.transfer ~src:src.container ~dst:dst.container

  let size_of t = Container.size_of t.container
end

(* use name matching the one in stdlib to ensure this one is used
   and to reduce the amount of changes: only need to change Hashtbl.create to Hashtbl.create_sized *)
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
    |> Container.initial

  (* note about randomized hashtbl:
    would need something like Siphash for this to improve security,
    but lets get ready for when that PR is merged: https://github.com/ocaml/ocaml/pull/9764
  *)

  let item_overhead = Memory_size.int 0 (* not a constant overhead, we can compute an approximation from stats if we have to *)

  let create_sized get_key_size get_value_size n =
    let h = Hashtbl.create ~random:true n in
    { h
    ; container = Container.create ~initial:(initial h) ~item_overhead get_key_size
    ; get_value_size = Container.open_nestable get_value_size
    }

  let reset t =
    Hashtbl.reset t.h;
    Container.clear t.container

  (* [clear] is not implemented: it doesn't shrink the table size,
     thus prone to space leaks *)

  (* copy is not implemented: it'd require 2 parents for expressions if we allow
     copying a queue with updatable elements *)

  (* [add] is not implemented on purpose: it doesn't remove previous bindings,
     but simply hides them, thus prone to space leaks *)
  let remove t k =
    begin try
      let prev = Hashtbl.find t.h k in
      Container.remove_kv t.container k @@ t.get_value_size prev
    with Not_found -> ()
    end;
    Hashtbl.remove t.h k

  let replace t k data =
    remove t k;
    Container.add_kv t.container k @@ t.get_value_size data;
    Hashtbl.replace t.h k data

  let find t k = Hashtbl.find t.h k

  let find_all t k = Hashtbl.find_all t.h k

  let mem t k = Hashtbl.mem t.h k

  let iter f t = Hashtbl.iter f t.h

  let fold f t init = Hashtbl.fold f t.h init

  let length t = Hashtbl.length t.h

  (* needs to be O(1) to avoid O(N^2) complexity in packet processing loop *)
  let size_of t = Container.size_of t.container
  (*let size_of t =
    let stats = Hashtbl.stats t.h in
    (* a constant time approximation *)
    let overhead = stats.Hashtbl.max_bucket_length * stats.Hashtbl.num_buckets * 2 in
    TODO: we're not tracking this overhead for now...
    Size.(MutableTracker.size t.size + of_int overhead)*)
end

(* Use a different module name so it doesn't clash with stdlib's List module.
   Lists are used everywhere, and not all of them need to track their sizes,
   updating all code to use the sized list tracking would cause much churn.

   Only implements functions from the List module that are tail-recursive,
   to avoid stack overflow.
   *)
module SizedList = struct
  type 'a t =
    { l: 'a list
    ; size: [`constant | `immutable | `updatable] size
    ; length: int
    ; size_of: 'a -> [`constant | `updatable | `immutable] size
    }

  let initial =
    record_start ()
    |> record_add_immutable @@ unit ()
    |> record_add_immutable @@ unit ()
    |> record_add_immutable @@ int 0
    |> record_add_immutable @@ unit ()
    |> record_end
    |> Container.initial

  let item_overhead =
    record_start ()
    |> record_add_immutable @@ unit ()
    |> record_add_immutable @@ unit ()
    |> record_end

  (* a list of elements with size 0 still takes up space, must consider element overhead *)
  let add acc e =
    add acc e
    |> add item_overhead

  let remove' = remove
  let remove acc e =
    remove acc e
    |> remove item_overhead


  (* filter itself is O(N) but we traverse the list only once,
     and only recompute the size for elements that are removed *)
  let filter pred t =
    let rec loop size length filtered = function
      | [] -> { l = List.rev filtered; length; size; size_of = t.size_of }
      | hd :: tl when pred hd ->
          loop size length (hd :: filtered) tl
      | hd :: tl ->
          let newsize = remove size @@ t.size_of hd in
          loop newsize (length-1) filtered tl
    in
    loop t.size t.length [] t.l

  let empty size_of = { l = []; size = initial; length = 0; size_of = Container.open_nestable size_of }
  let cons a t = { t with l = a :: t.l; size = add t.size (t.size_of a); length = t.length + 1}
  let length t = t.length
  let hd t = List.hd t.l
  let tl t =
    let l = List.tl t.l in
    { l
    ; size = remove t.size @@ t.size_of @@ List.hd t.l
    ; length = t.length - 1
    ; size_of = t.size_of
    }

  let rev_append l1 l2 =
    assert (l1.size_of == l2.size_of);
    let size = remove (add l1.size l2.size) initial in
    (* add l1.size l2.size = (l1 size + initial) + (l2 size + initial) + item_overhead
       remove (..) initial = (..) - initial - item_overhead
       l1 size + initial + l2 size + initial + item_overhead - initial - item_overhead
       = l1 size + l2 size + initial *)
    { l = List.rev_append l1.l l2.l
    ; size
    ; length = l1.length + l2.length
    ; size_of = l1.size_of }

  let iter f t = List.iter f t.l

  (* O(N), each element is potentially mapped and recomputed,
     but we traverse the list only once
   *)
  let rev_map size_of f t =
    let size_of = Container.open_nestable size_of in
    let rec loop l size = function
      | [] -> { l; length = t.length; size; size_of }
      | hd :: tl ->
          let mapped = f hd in
          loop (mapped :: l) (add size @@ size_of mapped)  tl
    in
    loop [] initial t.l

  let fold_left f init t = List.fold_left f init t.l

  let size_of t = (t.size : [< require_nestable] size :> [> require_nestable] size)

  let for_all f t = List.for_all f t.l
  let exists f t = List.exists f t.l
  let find e t = List.find e t.l

  let rev t =
    { t with l = List.rev t.l }

  let is_empty t = match t.l with [] -> true | _ -> false
end

