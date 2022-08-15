OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

CFLAGS += -I../mmap
CFLAGS += $(CFLAGS_libxenctrl) # For xen_mb()
CFLAGS += $(CFLAGS_xeninclude)
CFLAGS += $(APPEND_CFLAGS)

# can't -open Xenbus when compiling xenbus.mli itself,
# so don't add -open flags to OCAMLINCLUDE
OCAMLINCLUDE += -I ../mmap
OCAMLOPTFLAGS += -no-alias-deps -open Xenbus
OCAMLCFLAGS += -no-alias-deps -open Xenbus
OCAMLDEPFLAGS += -map xenbus.mli -open Xenbus

.NOTPARALLEL:
# Ocaml is such a PITA!

PREINTF = xenbus__Op.cmi xenbus__Partial.cmi xenbus__Packet.cmi
PREOBJS = xenbus__Op xenbus__Partial xenbus__Packet xenbus__Xs_ring
PRELIBS = $(foreach obj, $(PREOBJS),$(obj).cmo) $(foreach obj,$(PREOJBS),$(obj).cmx)
OBJS = xenbus__Op xenbus__Partial xenbus__Packet xenbus__Xs_ring xenbus__Xb
INTF = xenbus__Op.cmi xenbus__Packet.cmi xenbus__Xb.cmi
LIBS = xenbus.cma xenbus.cmxa

ALL_OCAML_OBJS = $(OBJS) $(PREOJBS)

all: $(PREINTF) $(PRELIBS) $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

xenbus_OBJS = $(OBJS)
xenbus_C_OBJS = xs_ring_stubs xenbus_stubs
OCAML_LIBRARY = xenbus

xenbus.cmi: xenbus.mli
	$(E) " CMI      $@"
	$(OCAMLC) $(OCAMLINCLUDE) -no-alias-deps -w -49 -c $< -o $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	$(OCAMLFIND) remove -destdir $(OCAMLDESTDIR) xenbus
	$(OCAMLFIND) install -destdir $(OCAMLDESTDIR) -ldconf ignore xenbus META $(LIBS) xenbus.cmi *.a *.so

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove -destdir $(OCAMLDESTDIR) xenbus

include $(OCAML_TOPLEVEL)/Makefile.rules
