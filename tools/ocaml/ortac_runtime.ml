include Ortac_runtime_monolith
module Errors = struct
  include Errors
  let report t =
    try report t
    with Monolith.PleaseBackOff -> raise Monolith.Unimplemented
    (* reference implementation can raise PleaseBackOff, candidate can only raise Unimplemented.
       Currently we have no reference implementation, just candidate *)

end
