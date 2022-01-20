type operation =
    Debug
  | Directory
  | Read
  | Getperms
  | Watch
  | Unwatch
  | Transaction_start
  | Transaction_end
  | Introduce
  | Release
  | Getdomainpath
  | Write
  | Mkdir
  | Rm
  | Setperms
  | Watchevent
  | Error
  | Isintroduced
  | Resume
  | Set_target
  | Reset_watches
  | Invalid

val size : int
(*@ function size: integer *)

val of_cval : int -> operation
(*@ op = of_cval i *)

val to_cval : operation -> int
(*@ r = to_cval op
    ensures 0 <= r < size *)

val to_string : operation -> string
(*@ s = to_string op *)
