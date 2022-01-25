type port = Xeneventchn.t
include Event
let __logical_max_evtchns__001_ = Ortac_runtime_monolith.Z.of_int 131072
let fd_ok t = ignore @@ Event.fd t
