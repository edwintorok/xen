/*
 * Copyright (C) 2013      Citrix Ltd.
 * Author Ian Campbell <ian.campbell@citrix.com>
 * Author Rob Hoes <rob.hoes@citrix.com>
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

struct caml_xtl {
    xentoollog_logger vtable;
    char *vmessage_cb;
    char *progress_cb;
    int refs;
    int disabled;
};

/* [stub_xtl_acquire_ref(xtl)] must be called before passing the logger to a
   Xen library: the logger needs to stay alive for as long as the C library
   might use it, and there are no calls from the C library to tell when it is
   stopped using it */
struct caml_xtl* stub_xtl_acquire_ref(value);

/* [stub_xtl-disable(xtl)] disables invoking any OCaml callbacks from this
   logger.
   This MUST be called in OCaml finalizers of other Xen C libraries, BEFORE
   closing the handle of those libraries (which may or may not invoke the
   logger).
   It is not safe to call other OCaml functions while inside a finalizer.
*/
void stub_xtl_disable(struct caml_xtl*);

/* [stub_xtl_drop_ref(&xtl)] call this after you have closed the underlying Xen
    C library that used the logger.
    The pointer will be set to NULL to detect double free attempts.
*/
void stub_xtl_drop_ref(struct caml_xtl**);

/*
  Recommended usage (note how we store 2 fields in the custom val, instead of
  just the usual pointer):

  typedef struct {
    yourxenlib_ctx* ctx;
    struct caml_xtl* logger;
  } ctx_val_t;

  #define Ctx_val(x) ((ctx_val_t *) Data_custom_val(x))

  static void yourstub_finalize(value)
  {
    stub_xtl_disable(Ctx_val(ctx)->logger);
    yourxenlib_free(Ctx_val(ctx)->ctx);
    stub_xtl_drop_ref(&Ctx_val(ctx)->logger);
  }

  CAMLprim value yourstub_alloc(value logger)
  {
    CAMLparam1(logger);
    CAMLlocal1(handle);
    ctx_val_t ctxval;

    ctxval.logger = stub_xtl_acquire_ref(logger);
    ret = yourxenlib_alloc(&ctxval.logger);
    if (ret) {
        stub_xtl_drop_ref(&ctxval.logger);
        failwith_yourlib(...);
    }

    handle = caml_alloc_custom(&yourlib_custom_ops, sizeof(ctxval), 0, 1);
    *Ctx_val(handle) = ctxval;

    CAMLreturn(handle);
  }
*/
