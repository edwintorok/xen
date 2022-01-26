module type Raw = sig
  type t (** a raw, unvalidated type *)

  val is_valid : t -> bool [@@logic]
  (** [is_valid t] checks whether [t] satisfies some invariants,
      and return true if it does.
      It should not raise exceptions.
   *)
  (*@ b = is_valid t
      pure *)
end

module type S = sig
  type raw

  type t
  (** the type containing only values that satisfy {!val:R.is_valid}.
      This type can only be constructed by {!val:validate} *)
  (*@ model raw: raw *)

  (*@ predicate is_valid (x: raw) *)

  val validate: raw -> t option
  (** [validate raw] returns [Some raw] if [raw] satisfies the
      invariants checked by {!val:R.is_valid}.
      Otherwise it returns None *)
  (*@ t = validate raw
      pure
      ensures not is_valid raw <-> t = None
      ensures match t with None -> not is_valid raw | Some v -> v.raw = raw
    *)

  val to_raw: t -> raw
  (** [to_raw t] retrieves the underlying [raw] value *)
  (*@ r = to_raw t
      pure
      ensures r = t.raw *)
end

module Make(R: Raw) :S with type raw = R.t = struct
  type raw = R.t

  type t = { raw: R.t }
  (* TODO: only because of model *)

  (*@ predicate is_valid (x:raw) = R.is_valid x *)

  let validate raw =
    if R.is_valid raw then Some { raw }
    else None

  let to_raw t = t.raw
end
