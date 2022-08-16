OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

CFLAGS += -I../xenmmap
CFLAGS += $(CFLAGS_libxenctrl) # For xen_mb()
CFLAGS += $(CFLAGS_xeninclude)
CFLAGS += $(APPEND_CFLAGS)
OCAMLINCLUDE += -I ../xenmmap
OCAMLOPTFLAGS += -for-pack Xenbus


.NOTPARALLEL:
# Ocaml is such a PITA!

PREINTF = op.cmi partial.cmi packet.cmi
PREOBJS = op partial packet xs_ring
PRELIBS = $(foreach obj, $(PREOBJS),$(obj).cmo) $(foreach obj,$(PREOJBS),$(obj).cmx)
OBJS = op partial packet xs_ring xb
INTF = op.cmi packet.cmi xb.cmi
LIBS = xenbus.cma xenbus.cmxa

ALL_OCAML_OBJS = $(OBJS) $(PREOJBS)

all: $(PREINTF) $(PRELIBS) $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

xenbus_OBJS = xenbus
xenbus_C_OBJS = xs_ring_stubs xenbus_stubs
OCAML_LIBRARY = xenbus
PACK_LIBRARY = $(OCAML_LIBRARY)

include $(OCAML_TOPLEVEL)/Makefile.rules
