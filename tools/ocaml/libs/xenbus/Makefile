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

xenbus.cmx : $(foreach obj, $(OBJS), $(obj).cmx)
	$(E) " CMX      $@"
	$(OCAMLOPT) -pack -o $@ $^

xenbus.cmo : $(foreach obj, $(OBJS), $(obj).cmo)
	$(E) " CMO      $@"
	$(OCAMLC) -pack -o $@ $^

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	$(OCAMLFIND) remove -destdir $(OCAMLDESTDIR) xenbus
	$(OCAMLFIND) install -destdir $(OCAMLDESTDIR) -ldconf ignore xenbus META $(LIBS) xenbus.cmo xenbus.cmi xenbus.cmx *.a *.so

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove -destdir $(OCAMLDESTDIR) xenbus

include $(OCAML_TOPLEVEL)/Makefile.rules
