OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

OCAML_LIBRARY = xenctrl

xenctrl_OCAMLINCLUDE += -I $(OCAML_xenmmap)
xenctrl_CFLAGS += -I$(OCAML_xenmmap)

xenctrl_C_OBJS = xenctrl_stubs

xenctrl_stubs.o: xenctrl_abi_check.h

xenctrl_abi_check.h: abi-check xenctrl_stubs.c xenctrl.ml
	$(PERL) -w $^ >$@.tmp && mv -f $@.tmp $@

xenctrl_GENERATED_FILES += xenctrl_abi_check.h

include $(OCAML_TOPLEVEL)/Makefile.rules
