/*
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
 */

#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <string.h>
#include <errno.h>
#include "mmap_stubs.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>

#define Wsize_bsize_round(n) (Wsize_bsize( (n) + sizeof(value) - 1 ))

value stub_mmap_alloc(void *addr, size_t len)
{
	CAMLparam0();
	CAMLlocal1(result);
	result = caml_alloc(Wsize_bsize_round(sizeof(struct mmap_interface)), Abstract_tag);
	Intf_val(result)->addr = addr;
	Intf_val(result)->len = len;
	CAMLreturn(result);
}

CAMLprim value stub_mmap_init(value fd, value pflag, value mflag,
                              value len, value offset)
{
	CAMLparam5(fd, pflag, mflag, len, offset);
	CAMLlocal1(result);
	int c_pflag, c_mflag;
	void* addr;
	size_t length;

	switch (Int_val(pflag)) {
	case 0: c_pflag = PROT_READ; break;
	case 1: c_pflag = PROT_WRITE; break;
	case 2: c_pflag = PROT_READ|PROT_WRITE; break;
	default: caml_invalid_argument("protectiontype");
	}

	switch (Int_val(mflag)) {
	case 0: c_mflag = MAP_SHARED; break;
	case 1: c_mflag = MAP_PRIVATE; break;
	default: caml_invalid_argument("maptype");
	}

	if (Int_val(len) < 0)
		caml_invalid_argument("negative size");
	if (Int_val(offset) < 0)
		caml_invalid_argument("negative offset");
	length = Int_val(len);

	caml_enter_blocking_section();
	addr = mmap(NULL, length, c_pflag, c_mflag, Int_val(fd), Int_val(offset));
	caml_leave_blocking_section();
	if (MAP_FAILED == addr)
		uerror("mmap", Nothing);

	result = stub_mmap_alloc(addr, length);
	CAMLreturn(result);
}

CAMLprim value stub_mmap_final(value intf)
{
	CAMLparam1(intf);
	struct mmap_interface interface = *Intf_val(intf);

	/* mark it as freed, in case munmap below fails, so we don't retry it */
	Intf_val(intf)->addr = MAP_FAILED;
	if (interface.addr != MAP_FAILED) {
		caml_enter_blocking_section();
		munmap(interface.addr, interface.len);
		caml_leave_blocking_section();
	}

	CAMLreturn(Val_unit);
}

CAMLprim value stub_mmap_read(value intf, value start, value len)
{
	CAMLparam3(intf, start, len);
	CAMLlocal1(data);
	int c_start;
	int c_len;

	c_start = Int_val(start);
	c_len = Int_val(len);

	if (c_start > Intf_val(intf)->len)
		caml_invalid_argument("start invalid");
	if (c_start + c_len > Intf_val(intf)->len)
		caml_invalid_argument("len invalid");

	data = caml_alloc_string(c_len);
	memcpy((char *) data, Intf_val(intf)->addr + c_start, c_len);

	CAMLreturn(data);
}

CAMLprim value stub_mmap_write(value intf, value data,
                               value start, value len)
{
	CAMLparam4(intf, data, start, len);
	int c_start;
	int c_len;

	c_start = Int_val(start);
	c_len = Int_val(len);

	if (c_start > Intf_val(intf)->len)
		caml_invalid_argument("start invalid");
	if (c_start + c_len > Intf_val(intf)->len)
		caml_invalid_argument("len invalid");

	memcpy(Intf_val(intf)->addr + c_start, (char *) data, c_len);

	CAMLreturn(Val_unit);
}

CAMLprim value stub_mmap_getpagesize(value unit)
{
	CAMLparam1(unit);
	CAMLlocal1(data);

	data = Val_int(getpagesize());
	CAMLreturn(data);
}
