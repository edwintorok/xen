OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

OCAML_LIBRARY = xentoollog

# allow mixed declarations and code
xentoollog_CFLAGS += -Wno-declaration-after-statement

xentoollog_C_OBJS = xentoollog_stubs

GENERATED_FILES += xentoollog.ml xentoollog.ml.tmp xentoollog.mli xentoollog.mli.tmp
GENERATED_FILES += _xtl_levels.mli.in _xtl_levels.ml.in _xtl_levels.inc META

xentoollog.ml: xentoollog.ml.in _xtl_levels.ml.in
	sed -e '1i\
(*\
 * AUTO-GENERATED FILE DO NOT EDIT\
 * Generated from xentoollog.ml.in and _xtl_levels.ml.in\
 *)\
' \
	    -e '/^(\* @@XTL_LEVELS@@ \*)$$/r_xtl_levels.ml.in' \
	  < xentoollog.ml.in > xentoollog.ml.tmp
	mv xentoollog.ml.tmp xentoollog.ml

xentoollog.mli: xentoollog.mli.in _xtl_levels.mli.in
	sed -e '1i\
(*\
 * AUTO-GENERATED FILE DO NOT EDIT\
 * Generated from xentoollog.mli.in and _xtl_levels.mli.in\
 *)\
' \
	    -e '/^(\* @@XTL_LEVELS@@ \*)$$/r_xtl_levels.mli.in' \
	  < xentoollog.mli.in > xentoollog.mli.tmp
	mv xentoollog.mli.tmp xentoollog.mli


_xtl_levels.ml.in _xtl_levels.mli.in _xtl_levels.inc &: genlevels.py $(XEN_INCLUDE)/xentoollog.h
	$(PYTHON) genlevels.py _xtl_levels.mli.in _xtl_levels.ml.in _xtl_levels.inc

include $(OCAML_TOPLEVEL)/Makefile.rules
