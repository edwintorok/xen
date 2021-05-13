/*
 * Copyright (C) 2012-2013 Citrix Inc
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
 */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>

/* For PROT_READ | PROT_WRITE */
#include <sys/mman.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#include "xengnttab.h"
#include "mmap_stubs.h"

#ifndef Data_abstract_val
#define Data_abstract_val(v) ((void*) Op_val(v))
#endif

#define _G(__g) (*((xengnttab_handle**)Data_abstract_val(__g)))

CAMLprim value stub_gnttab_interface_open(void)
{
	CAMLparam0();
	CAMLlocal1(result);
	xengnttab_handle *xgh;

	xgh = xengnttab_open(NULL, 0);
	if (xgh == NULL)
		caml_failwith("Failed to open interface");
	result = caml_alloc(1, Abstract_tag);
	_G(result) = xgh;

	CAMLreturn(result);
}

CAMLprim value stub_gnttab_interface_close(value xgh)
{
	CAMLparam1(xgh);

	xengnttab_close(_G(xgh));

	CAMLreturn(Val_unit);
}

#define _M(__m) ((struct mmap_interface*)Data_abstract_val(__m))
#define XEN_PAGE_SHIFT 12

CAMLprim value stub_gnttab_unmap(value xgh, value array)
{
	CAMLparam2(xgh, array);
	int result;

	caml_enter_blocking_section();
	result = xengnttab_unmap(_G(xgh), _M(array)->addr, _M(array)->len >> XEN_PAGE_SHIFT);
	caml_leave_blocking_section();

	if(result!=0) {
		caml_failwith("Failed to unmap grant");
	}

	CAMLreturn(Val_unit);
}

CAMLprim value stub_gnttab_map_fresh(
	value xgh,
	value reference,
	value domid,
	value writable
	)
{
	CAMLparam4(xgh, reference, domid, writable);
	CAMLlocal1(contents);
	void *map;

	caml_enter_blocking_section();
	map = xengnttab_map_grant_ref(_G(xgh), Int_val(domid), Int_val(reference),
		Bool_val(writable)?PROT_READ | PROT_WRITE:PROT_READ);
	caml_leave_blocking_section();

	if(map==NULL) {
		caml_failwith("Failed to map grant ref");
	}
	contents = stub_mmap_alloc(map, 1 << XEN_PAGE_SHIFT);
	CAMLreturn(contents);
}
