OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

CFLAGS += -I../xenmmap
CFLAGS += $(CFLAGS_libxenctrl) # For xen_mb()
CFLAGS += $(CFLAGS_xeninclude)
CFLAGS += $(APPEND_CFLAGS)
OCAMLINCLUDE += -I ../xenmmap
OCAMLFLAGS += -for-pack Xenbus


.NOTPARALLEL:
# Ocaml is such a PITA!

OCAML_LIBRARY = xenbus
PACK_LIBRARY = $(OCAML_LIBRARY)

include $(OCAML_TOPLEVEL)/Makefile.rules
