(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) 2012-2014 Citrix Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type gntref = int
type domid = int

let console = 0 (* public/grant_table.h:GNTTAB_RESERVED_CONSOLE *)
let xenstore = 1 (* public/grant_table.h:GNTTAB_RESERVED_XENSTORE *)

type grant_handle (* handle to a mapped grant *)

module Gnttab = struct
  type interface

  external interface_open': unit -> interface = "stub_gnttab_interface_open"

  let interface_open () =
    try
      interface_open' ()
    with e ->
      Printf.fprintf stderr "Failed to open grant table device: ENOENT\n";
      Printf.fprintf stderr "Does this system have Xen userspace grant table support?\n";
      Printf.fprintf stderr "On linux try:\n";
      Printf.fprintf stderr "  sudo modprobe xen-gntdev\n%!";
      raise e

  external interface_close: interface -> unit = "stub_gnttab_interface_close"

  type grant = {
    domid: domid;
    ref: gntref;
  }

  external unmap_exn : interface -> Xenmmap.mmap_interface -> unit = "stub_gnttab_unmap"

  external map_fresh_exn: interface -> gntref -> domid -> bool -> Xenmmap.mmap_interface = "stub_gnttab_map_fresh"

  module Local_mapping = struct
    type t = Xenmmap.mmap_interface

    let to_pages interface t =
      Xenmmap.make t ~unmap:(unmap_exn interface)
  end

  let map_exn interface grant writable =
    map_fresh_exn interface grant.ref grant.domid writable

end
