#include <time.h>
#include <stdint.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/fail.h>

CAMLprim int64_t stub_monotonic_clock_get_ns_unboxed(value unit)
{
    /* this function is marked [@noalloc], do not use the CAML* macros here */
    struct timespec ts;
    if ( clock_gettime(CLOCK_MONOTONIC, &ts) )
        uerror("clock_gettime", Val_unit);
    return (int64_t)ts.tv_nsec + (int64_t)ts.tv_sec * 1000000000LL;
}

/* a boxed version must be defined for bytecode */
CAMLprim value stub_monoclock_get_ns_boxed(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(caml_copy_int64(stub_monotonic_clock_get_ns_unboxed (unit)));
}
