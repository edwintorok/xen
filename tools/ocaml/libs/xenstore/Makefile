OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

OCAMLINCLUDE += -I ../xenbus/
OCAMLFLAGS += -for-pack Xenstore

OCAML_LIBRARY = xenstore

include $(OCAML_TOPLEVEL)/Makefile.rules
