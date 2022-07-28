#include <fcntl.h>
#include <sys/mman.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

CAMLprim value stub_shm_open(value name, value create, value perm)
{
    CAMLparam3(name, create, perm);
    /* easier than using convert_flag_list on all the flags and keeping flags
       up-to-date */
    int oflag = O_RDWR | (Bool_val(create) ? (O_CREAT | O_EXCL) : 0);
    /* caml_strdup is deprecated, use it for 4.02.3 compat for now though,
       on newer OCaml versions this is an alias for caml_stat_strdup */
    char *namestr = caml_strdup(String_val(name));
    caml_enter_blocking_section();
    int fd = shm_open(namestr, oflag, Int_val(perm));
    caml_leave_blocking_section();
    caml_stat_free(namestr);
    if (fd == -1) uerror("shm_open", name);
    CAMLreturn (Val_int(fd));
}

CAMLprim value stub_shm_unlink(value name)
{
    CAMLparam1(name);
    int rc;

    char *namestr = caml_strdup(String_val(name));
    caml_enter_blocking_section();
    rc = shm_unlink(namestr);
    caml_leave_blocking_section();
    if (rc == -1) uerror("shm_unlink", name);
    CAMLreturn(Val_unit);
}
