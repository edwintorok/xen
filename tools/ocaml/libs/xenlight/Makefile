OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

OCAML_LIBRARY = xenlight
# ignore unused generated functions and allow mixed declarations and code
xenlight_CFLAGS += -Wno-unused -Wno-declaration-after-statement
xenlight_CFLAGS += -I$(OCAML_xentoollog)

xenlight_OCAMLINCLUDE += -I $(OCAML_xentoollog)

xenlight_C_OBJS = xenlight_stubs

GENERATED_FILES += xenlight.ml xenlight.ml.tmp xenlight.mli xenlight.mli.tmp
GENERATED_FILES += _libxl_types.ml.in _libxl_types.mli.in
GENERATED_FILES += _libxl_types.inc META


xenlight.ml: xenlight.ml.in _libxl_types.ml.in
	sed -e '1i\
(*\
 * AUTO-GENERATED FILE DO NOT EDIT\
 * Generated from xenlight.ml.in and _libxl_types.ml.in\
 *)\
' \
	    -e '/^(\* @@LIBXL_TYPES@@ \*)$$/r_libxl_types.ml.in' \
	  < xenlight.ml.in > xenlight.ml.tmp
	mv xenlight.ml.tmp xenlight.ml

xenlight.mli: xenlight.mli.in _libxl_types.mli.in
	sed -e '1i\
(*\
 * AUTO-GENERATED FILE DO NOT EDIT\
 * Generated from xenlight.mli.in and _libxl_types.mli.in\
 *)\
' \
	    -e '/^(\* @@LIBXL_TYPES@@ \*)$$/r_libxl_types.mli.in' \
	  < xenlight.mli.in > xenlight.mli.tmp
	mv xenlight.mli.tmp xenlight.mli

_libxl_types.ml.in _libxl_types.mli.in _libxl_types.inc &: genwrap.py $(XEN_ROOT)/tools/libs/light/libxl_types.idl \
                $(XEN_ROOT)/tools/libs/light/idl.py
	PYTHONPATH=$(XEN_ROOT)/tools/libs/light $(PYTHON) genwrap.py \
		$(XEN_ROOT)/tools/libs/light/libxl_types.idl \
		_libxl_types.mli.in _libxl_types.ml.in _libxl_types.inc

include $(OCAML_TOPLEVEL)/Makefile.rules
