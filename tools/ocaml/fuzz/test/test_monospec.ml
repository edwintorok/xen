open Monolith

let iref () = ()

let icand () = ()

let () =
  declare "create" (unit ^> unit) iref icand;

  Monolith.main 20
