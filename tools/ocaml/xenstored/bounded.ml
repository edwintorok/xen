module type Sys = sig
  (*@ function _word_size_bytes: integer *)
  (*@ axiom bitness: _word_size_bytes = 4 || _word_size_bytes = 8 *)
  (*@ function word_size: integer = _word_size_bytes * 8 *)

  (*@ function max_array_length: integer *)
  (*@ axiom maxarrayl:
    (word_size = 32 -> max_array_length = 4194303)
    && (word_size = 64 -> 4194303 < max_array_length <= 18014398509481983)
    *)

  (*@ function max_string_length: integer = _word_size_bytes * max_array_length - 1 *)
end

module type Sized = sig
  type t (** a type that we want to associate a size with *)

  val size_bytes: t -> int
  (** [size_bytes t] is the size in bytes of [t], to be used for quota purposes.
      It should be related to the size that a xenstore client specified, and exclude any
      OCaml/implementation specific overhead.
      It should include the size of any values it contains (even if that value is shared).
      The OCaml overhead must have a constant upper bound defined by {!val:overhead_words}.
      It should be computed in O(1).
      *)
  (*@ n = size_bytes t
      pure
      ensures 0 <= n <= Sys.max_string_length *)
  (* [t] was at some point stored in a Packet, thus in a String, hence its size cannot be larger
      than maximum string size. *)

  val max_overhead_words: int
  (** [max_overhead] is the maximum overhead (in words) that the OCaml value itself uses.
      E.g. [2] for a String, and is at least [1] word for the OCaml value header.
      It does not include any extra overhead used by a container to store the item, that overhead
      will be declared on the container itself.
      *)
  (*@ ensures 1 <= result < Sys.max_string_length *)
end

(*@ open Seq *)

module type Container = sig
  type 'a t
  (** The type of an immutable container holding elements of type 'a. *)
  (*@ model contents: 'a seq *)
  (* we model it with a sequence, although a bag would suffice, but cameleer doesn't implement it yet *)

  val fold: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold f init c] traverses [c] and calls [f acc] on each element,
      where [acc] starts out with [init]. *)
  (*@ r = fold f init t
      pure *)
  (* cameleer doesn't have Seq.fold_left to express what 'r' is yet *)

  val add: 'a t -> 'a -> 'a t
  (** [add t e] adds [e] to [t] *)
  (*@ r = add t e
      pure
      ensures r.contents = Seq.cons e t.contents
      *)

  val remove: 'a t -> 'a -> 'a t
  (** [remove t e] removes [e] from [t] *)
  (*@ r = remove t e
      requires Seq.mem e t.contents
      pure
      ensures Seq.cons e r.contents = t.contents *)
end

module SizedContainer(C: Container)(S: Sized) = struct
  let fold = C.fold

  let[@logic] sum acc e = acc + S.size_bytes e

  type element = S.t

  type t =
    { cached_size: int
    ; container: S.t C.t }
  (*@ invariant cached_size = fold sum 0 container *)

  let create container =
    let cached_size = C.fold sum 0 container in
    { cached_size; container }

  let add t e =
    let cached_size = t.cached_size + S.size_bytes e in
    { cached_size; container = C.add t.container e }

  let remove t e =
    let cached_size = t.cached_size - S.size_bytes e in
    { cached_size; container = C.remove t.container e }

  let size_bytes t = t.cached_size
end

(* TODO: use SC instead *)
module Bounded(S: Sized)(C: Container with type 'a t = S.t) = struct
  let size_bytes = S.size_bytes
  type t =
    { byte_limit: int
    ; container: S.t
   }
  (*@ invariant size_bytes container <= byte_limit *)

  let create container ~byte_limit =
    if S.size_bytes container > byte_limit then None
    else Some { byte_limit; container }
  (*@ t = create container ~byte_limit
      requires byte_limit >= 0
      pure
      *)

  let add t e =
    let container = C.add t.container e in
    (* we could move this before the add if we change the types to give us element size *)
    if S.size_bytes t.container > t.byte_limit then None
    else Some { t with container }
  (*@ r = add t e
      pure *)

  let remove t e = { t with container = C.remove t.container e }
  (*@ r = remove t e
      pure *)
end
