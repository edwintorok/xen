#include <time.h>
#include <stdint.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/fail.h>

CAMLprim value stub_clock_record(value bigarray, value idx)
{
    struct timespec ts;
    int64_t timestamp;
    int index = Int_val(idx);
    /* noalloc stub, do not use CAMLparam* macros */
    if (index < 0 || index >= Caml_ba_array_val(bigarray)->dim[0])
        caml_invalid_argument("index"); /* should never happen */

    if ( clock_gettime(CLOCK_MONOTONIC, &ts) )
        uerror("clock_gettime", Val_unit);
    timestamp = (int64_t)ts.tv_nsec + (int64_t)ts.tv_sec * 1000000000LL;
    ((int64_t*)Caml_ba_data_val(bigarray))[index] = timestamp;
    
    return Val_unit;
}
