let con = Xsgen_test.connection

module C = Xsgen_rtac

let wrap candidate =
  match candidate with
  | Error (Ortac_runtime.Error e) ->
      let errstr = Fmt.to_to_string Ortac_runtime.pp_error_report e in
      Monolith.(Invalid (fun _ -> Print.(comment @@ string errstr)))
  | (Error _ | Ok _) as r ->
      (* ignore other exceptions and values:
          Ortac will have inserted pre/post-condition checks already and
          if there was anything wrong it would've raised an error *)
      Monolith.Valid r

let wrap1 x = wrap
let wrap2 x y = wrap
let wrap3 x y z = wrap
let wrap4 x y z t = wrap

(* from auto-generated rtac monolith code *)
module R = C
open Monolith
module M = Ortac_runtime_monolith
module G =
  struct let perm () = { R.r = (Gen.bool ()); R.w = (Gen.bool ()) } end
module P =
  struct
    let perm { R.r; R.w } =
      M.print_record "" [("r", (Print.bool r)); ("w", (Print.bool w))]
  end
let () =
  let transaction = declare_abstract_type ~var:"transaction" () in
  let connection = constructible (fun () -> C.connection, constant "connection") in
  declare "none" transaction C.none C.none;
  declare "transaction_start" (connection ^!?> transaction) wrap1 C.transaction_start;
  declare "transaction_end" (connection ^> transaction ^> bool ^!?> bool) wrap3 C.transaction_end;

  let path = declare_abstract_type ~var:"path" () in (* TODO: generators here *)
  declare "path" (unit ^> path) C.path C.path;
  declare "directory" (connection ^> transaction ^> path ^!?> list path) wrap3 C.directory;

  let value = declare_abstract_type ~var:"value" () in (* TODO: generators here *)
  declare "value" (unit ^> value) C.value C.value;
  declare "read" (connection ^> transaction ^> path ^!?> value) wrap3 C.read;
  declare "write" (connection ^> transaction ^> path ^> value ^!?> unit) wrap4 C.write;
  declare "mkdir" (connection ^> transaction ^> path ^!?> unit) wrap3 C.mkdir;
  declare "rm" (connection ^> transaction ^> path ^!?> unit) wrap3 C.rm;

  let perm =
    let neg = easily_constructible G.perm P.perm in
    let pos = deconstructible P.perm in ifpol neg pos
  in
  let domid = declare_abstract_type ~var:"domid" () in

  let perms = declare_abstract_type ~var:"perms" () in
  declare "perms" (domid ^> perm ^> perms) C.perms C.perms;
  declare "getperms" (connection ^> transaction ^> path ^!?> perms) wrap3 C.getperms;
  declare "setperms" (connection ^> transaction ^> path ^> perms ^!?> unit) wrap4 C.setperms;

  declare "introduce" (connection ^!?> domid) wrap1 C.introduce;
  declare "release" (connection ^> domid ^!?> unit) wrap2 C.release;
  declare "resume" (connection ^> domid ^!?> unit) wrap2 C.resume;

  declare "getdomainpath" (connection ^> domid ^!?> path) wrap2 C.getdomainpath;

  let token = declare_abstract_type ~var:"token" () in (* TODO: generators here *)
  declare "token" (unit ^> token) C.token C.token;
  declare "watch" (connection ^> path ^> token ^!?> unit) wrap3 C.watch
  (*declare "unwatch" (connection ^> path ^> token ^!?> unit) wrap3 C.unwatch*)


let () = let fuel = 100 in main fuel
