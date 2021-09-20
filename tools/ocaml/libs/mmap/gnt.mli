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

(** Allow a local xen domain to read/write memory exported ("granted")
    from foreign domains. Safe memory sharing is a building block of all
    xen inter-domain communication protocols such as those for virtual
    network and disk devices.

    Foreign domains will explicitly "grant" us access to certain memory
    regions such as disk buffers. These regions are uniquely identified
    by the pair of (foreign domain id, integer reference) which is
    passed to us over some existing channel (typically via xenstore keys
    or via structures in previously-shared memory region).
*)

(** {2 Common interface} *)

type gntref = int
(** Type of a grant table index, called a grant reference in
    Xen's terminology. *)

(** {2 Receiving foreign pages} *)

module Gnttab : sig
  type interface
  (** A connection to the grant device, needed for mapping/unmapping *)

  val interface_open: unit -> interface
  (** Open a connection to the grant device. This must be done before any
      calls to map or unmap. *)

  val interface_close: interface -> unit
  (** Close a connection to the grant device. Any future calls to map or
      unmap will fail. *)

  type grant = {
    domid: int;
    (** foreign domain who is exporting memory *)
    ref: gntref;
    (** id which identifies the specific export in the foreign domain *)
  }

  (** A foreign domain must explicitly "grant" us memory and send us the
      "reference". The pair of (foreign domain id, reference) uniquely
      identifies the block of memory. This pair ("grant") is transmitted
      to us out-of-band, usually either via xenstore during device setup or
      via a shared memory ring structure. *)

  module Local_mapping : sig
    type t
    (** Abstract type representing a locally-mapped shared memory page *)

    val to_pages: interface -> t -> Xenmmap.t
  end

  val map_exn : interface -> grant -> bool -> Local_mapping.t
  (** [map_exn if grant writable] creates a single mapping from
      [grant] that will be writable if [writable] is [true]. *)

  val unmap_exn: interface -> Local_mapping.t -> unit
  (** Unmap a single mapping (which may involve multiple grants). Throws a
      Failure if unsuccessful. *)
end

val console: gntref
(** In xen-4.2 and later, the domain builder will allocate one of the
    reserved grant table entries and use it to pre-authorise the console
    backend domain. *)

val xenstore: gntref
(** In xen-4.2 and later, the domain builder will allocate one of the
    reserved grant table entries and use it to pre-authorise the xenstore
    backend domain. *)
