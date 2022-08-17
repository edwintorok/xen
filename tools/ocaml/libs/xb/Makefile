OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

.NOTPARALLEL:
# Ocaml is such a PITA!

OCAML_LIBRARY = xenbus

xenbus_OCAMLINCLUDE += -I $(OCAML_xenmmap)
xenbus_CFLAGS += -I$(OCAML_xenmmap)

xenbus_OCAMLFLAGS += -for-pack Xenbus

# objects that go into the pack
xenbus_OBJS = op partial packet xs_ring xb

xenbus_C_OBJS = xs_ring_stubs xenbus_stubs

include $(OCAML_TOPLEVEL)/Makefile.rules
