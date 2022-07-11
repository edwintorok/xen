(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
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

let xenstored_major = 1
let xenstored_minor = 0

let xs_daemon_socket = Paths.xen_run_stored ^ "/socket"

let default_config_dir = Paths.xen_config_dir

let maxwatch = ref (50)
let maxtransaction = ref (20)
let maxrequests = ref (-1)   (* maximum requests per transaction *)

let gc_max_overhead = ref 100
let gc_switch_threshold = ref (100*1024*1024)
let gc_allocation_policy =
  let current_policy = (Gc.get ()).Gc.allocation_policy in
  (* if we have a version of OCaml new enough to support allocation policy 2 or newer (best-fit)
     then don't change it by default, otherwise switch to 1 (first-fit) once we use more memory.
     First-fit reduces fragmentation, and compactions, but can be somewhat slower.
  *)
  if current_policy < 2 then ref 1 else ref current_policy

(* TODO: calc based on config limits and RAM% and allow to disable? or just set
 *)
let maxdomumemory = ref (512*1024*1024)

let conflict_burst_limit = ref 5.0
let conflict_max_history_seconds = ref 0.05
let conflict_rate_limit_is_aggregate = ref true

let domid_self = 0x7FF0

let path_max = ref Xenbus.Partial.xenstore_rel_path_max

exception Not_a_directory of string
exception Not_a_value of string
exception Already_exist
exception Doesnt_exist
exception Lookup_Doesnt_exist of string
exception Invalid_path
exception Permission_denied
exception Unknown_operation
