let real_size_words x = Obj.reachable_words (Obj.repr x)

module ContainerModel = struct
  type t =
    { base: int
    ; element_overhead: int
    }

  let eval_n t n element_size =
    t.base + n * (t.element_overhead + element_size)

  let eval_seq t element_size_of elements =
    elements |> Seq.map (fun element ->
    element_size_of element + t.element_overhead)
    |> Seq.fold_left (+) t.base

  let of_gen ~to_seq ~gen =
    let base = 0 |> gen |> real_size_words in
    let of_elements n =
      let elements = gen n in
      let total = real_size_words elements in
      let count, sum_elements =
        elements |> to_seq |> Seq.map (fun element -> real_size_words element)
        |> Seq.fold_left (fun (count, sum) x -> count+1, sum + x) (0, base)
      in
      assert (count > 0);
      (total - sum_elements + count - 1) / count
    in
    let element_overhead_1 = of_elements 1
    and element_overhead_100 = (of_elements 100)
    and element_overhead_1000 = (of_elements 1000) in
    let t = { base; element_overhead = Int.max element_overhead_1 element_overhead_100 } in
    let elements_10000 = gen 10000 in
    let calculated = eval_seq t real_size_words (elements_10000 |> to_seq)
    and actual = real_size_words elements_10000 in
    if calculated < actual then
      Fmt.invalid_arg "Failed to validate container memory usage model on 10000 elements. Baseline = %d, per-element overhead: (1 => %d, 100 => %d, 1000 => %d). Calculated usage for 10000 elements: %d, real: %d"
        base element_overhead_1 element_overhead_100 element_overhead_1000 calculated actual;
    t
end

let array_model gen_element =
  let gen n = Array.init n (fun _ -> gen_element ()) in
  ContainerModel.of_gen ~to_seq:Array.to_seq ~gen

let hashtbl_model initial gen_key gen_value =
  let gen n =
    (* we'd fail validation if we didn't use 0 here *)
    let t = Hashtbl.create 0 in
    let (_:unit array) = Array.init n (fun _ ->
      Hashtbl.add t (gen_key ()) (gen_value ())
    ) in
    t
  in
  let base = Hashtbl.create initial |> real_size_words in
  let t = ContainerModel.of_gen ~to_seq:Hashtbl.to_seq ~gen in
  { t with base = Int.max t.base base }

module StringMap = Map.Make(String)

let stringmap_model gen_key gen_value =
  let gen n =
    Array.init n (fun _ ->
      gen_key (), gen_value ()
    ) |> Array.to_seq |> StringMap.of_seq
  in
  ContainerModel.of_gen ~to_seq:StringMap.to_seq ~gen
