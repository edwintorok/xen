open Bechamel
open Toolkit

let benchmark tests =
  let instances = Instance.[ monotonic_clock ] in
  let cfg = Benchmark.cfg ~kde:(Some 1000) ~quota:(Time.second 5.) () in
  let raw_results = Benchmark.all cfg instances tests in
  let ols = Analyze.ols ~r_square:true ~bootstrap:10 ~predictors: [| Measure.run |] in
  let instances = [Instance.monotonic_clock] in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  results, raw_results

let nothing _ = Ok ()
let html tests =
  let results = benchmark tests in
  let results =
    let open Bechamel_js in
    emit ~dst:(Channel stdout) nothing ~compare ~x_label:Measure.run
      ~y_label:(Measure.label Instance.monotonic_clock)
      results
  in
  match results with Ok () -> () | Error (`Msg err) -> invalid_arg err
