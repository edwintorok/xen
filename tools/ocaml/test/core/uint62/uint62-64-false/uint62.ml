type t = int64

let of_int64 i = Int64.logand i 0x3FFF_FFFF_FFFF_FFFFL

let to_int64 (i : t) = i
