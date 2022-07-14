(* based on bechamel example code *)
open Bechamel
open Toolkit

(* TODO: share with bechamel_simple_html *)
let instances = Instance.[monotonic_clock; minor_allocated; major_allocated]

let benchmark tests =
  let cfg = Benchmark.cfg ~kde:(Some 1000) () in
  Benchmark.all cfg instances tests

let analyze raw_results =
  (* a non-zero bootstrap causes an assertion failure with the Obj.reachable_words
     benchmark in bench_sizeops if bootstrapping is used, probably because
     it won't have enough samples as it takes a very long time to run *)
  let ols =
    Analyze.ols ~r_square:true ~bootstrap:0 ~predictors:[|Measure.run|]
  in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  (Analyze.merge ols instances results, raw_results)

let () =
  List.iter (fun i -> Bechamel_notty.Unit.add i (Measure.unit i)) instances

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let cli tests =
  let window =
    match winsize Unix.stdout with
    | Some (w, h) ->
        {Bechamel_notty.w; h}
    | None ->
        {Bechamel_notty.w= 80; h= 1}
  in
  let results, _ = tests |> benchmark |> analyze in
  print_newline () ;
  (* for --no-buffer *)
  (* Fmt.pf Fmt.stdout "results: %a" Fmt.(hashtbl @@ pair string @@ hashtbl @@ pair string Analyze.OLS.pp) results *)
  img (window, results) |> eol |> output_image
