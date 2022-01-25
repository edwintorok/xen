module type ImmutableContainer = sig
  type size = int (** bytes, or words, or count, defined by container. *)

  type element (** the type of elements in the container *)

  val element_size: element -> size
  (** [element_size el] is the size of [el] computed in O(1) *)
  (*@ n = element_size el
      pure
      ensures 0 <= n
   *)

  type t (** the type of the container *)

  val count: t -> int
  (*@ n = count t
      pure
      ensures 0 <= n *)

  val add: t -> element -> t
  (*@ r = add t e
      pure
   *)

  val remove: t -> element -> t
  (*@ r = remove t e
      pure
    *)

  val fold: ('a -> element -> 'a) -> 'a -> t -> 'a
  (*@ r = fold f init t
      pure *)

end

module Make(I: ImmutableContainer) = struct
  type size = I.size
  type container = I.t
  type element = I.element

  type raw =
    { container: container
    ; count_limit: int
    ; size_limit: int
    ; cached_size: int
    }

  type t = raw
  let[@logic] count x = I.count x
  (*@ n = count t
      pure
      ensures 0 <= n *)

  let[@logic] raw x = x
  (*@ r = raw t
      pure *)

  let[@logic] container_size c =
    I.fold (fun s e -> s + I.element_size e) 0 c

  let size t = t.cached_size
  (*@ n = size t
      pure *)

  (*@ invariant count t <= (raw t).count_limit *)

  (*@ invariant size t <= (raw t).size_limit *)

  (*@ invariant size t = (raw t).cached_size = container_size (raw t).container *)

  let create container ~count_limit ~size_limit =
    if I.count container > count_limit then
      invalid_arg "Container has exceeded count limit";
    let cached_size = container_size container in
    if cached_size > size_limit then
      invalid_arg "Container has exceeded size limit";
    { container
    ; count_limit
    ; size_limit
    ; cached_size
    }
  (*@ t = create container ~count_limit ~size_limit
      raises Invalid_argument _ -> (count container > count_limit) || (container_size container > size_limit)
      ensures (raw t).container = container
      ensures (raw t).count_limit = count_limit
      ensures (raw t).size_limit = size_limit
   *)

  (*@ r = add t e
      pure *)
  let add t e =
    if I.count t.container >= t.count_limit then None
    else
    let cached_size = t.size + I.element_size e in
    if cached_size > t.size_limit then None
    else Some { t with cached_size; container = I.add t.container e }

  let remove t e =
    let cached_size = t.cached_size - I.element_size e in
    { t with container = I.remove t.container e; cached_size }
  (*@ r = remove t e
      pure
   *)
end
