OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

CFLAGS += -I../xenmmap $(CFLAGS_libxenctrl) $(CFLAGS_libxenguest)
CFLAGS += $(APPEND_CFLAGS)
OCAMLINCLUDE += -I ../xenmmap

LIBS_xenctrl = $(LDLIBS_libxenctrl) $(LDLIBS_libxenguest)

OCAML_LIBRARY = xenctrl

xenctrl_stubs.o: xenctrl_abi_check.h

xenctrl_abi_check.h: abi-check xenctrl_stubs.c xenctrl.ml
	$(PERL) -w $^ >$@.tmp && mv -f $@.tmp $@

GENERATED_FILES += xenctrl_abi_check.h

include $(OCAML_TOPLEVEL)/Makefile.rules
