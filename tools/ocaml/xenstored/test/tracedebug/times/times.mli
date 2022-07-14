(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(** type for time ring *)
type t

val create : int -> t
(** [create n] creates a time ring of size [n] *)

val record : t -> int -> unit
(** [record t index] records the current timestamp in the ring at index [i].
  Caller has to ensure the ring index is within bounds.
  This call is performance critical, it records the timestamp without conversion
  and allocation. *)

val get_as_ns : t -> int -> int64 option
(** [get_as_ns t i] returns the [i]th timestamp converted to nanoseconds.
    Invalid timestamp are returned as [0L]. *)

val precision : int
(** [precision] usual precision in fractional digits. [9] would be [1 ns] *)

val fill : t -> unit
(** [fill t] fills [t] with invalid timestamps *)

val id : string
(** [id] identifies which method we used to fetch the time *)
