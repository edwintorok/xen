module Model = struct
  type t =
    { offset: int (* >= 0 *)
    ; multiplier: float (* >= 1.0 *)
    ; exact : bool
    }

  (* f(n) = offset + multiplier * n *)

  let eval model n =
    model.offset + (model.multiplier *. float n |> Float.ceil |> Float.to_int)

  let exact = { offset = 0; multiplier = 1.0; exact = true }

  let linear ?(offset=0) ?multiplier examples =
    let diff model (computed, expected) =
      expected - eval model computed
    in
    match multiplier with
    | Some m ->
        let model = { offset = 0; multiplier = m; exact = false } in
        let offset = examples |> Seq.map (diff model)
        |> Seq.fold_left Int.max offset
        in
        { model with offset }
    | None ->
        let offset =
          match examples () with
          | Seq.Nil -> 0
          | _ ->
          examples |> Seq.map (diff exact)
          |> Seq.fold_left min max_int
          |> max 0
        in
        let model = { exact with offset } in
        let multiplier =
          examples |> Seq.map (fun (computed, expected) ->
            if computed = 0 then 0.
            else
            float (expected - model.offset) /. float computed
          )
          |> Seq.fold_left Float.max 1.0
        in
        { model with multiplier }

  let check model n real =
    let computed = eval model n in
    if real > computed then
      Fmt.error

end
