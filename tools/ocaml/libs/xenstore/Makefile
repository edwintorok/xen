OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

OCAMLINCLUDE += -I ../xenbus/
OCAMLFLAGS += -for-pack Xenstore

.NOTPARALLEL:
# Ocaml is such a PITA!

OCAML_NOC_LIBRARY = xenstore

xenstore.cmx : $(foreach obj, $(OBJS), $(obj).cmx)
	$(E) " CMX      $@"
	$(Q)$(OCAMLOPT) -pack -o $@ $^

xenstore.cmo : $(foreach obj, $(OBJS), $(obj).cmo)
	$(E) " CMO      $@"
	$(Q)$(OCAMLC) -pack -o $@ $^


.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	$(OCAMLFIND) remove -destdir $(OCAMLDESTDIR) xenstore
	$(OCAMLFIND) install -destdir $(OCAMLDESTDIR) -ldconf ignore xenstore META $(LIBS) xenstore.cmo xenstore.cmi xenstore.cmx *.a

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove -destdir $(OCAMLDESTDIR) xenstore

include $(OCAML_TOPLEVEL)/Makefile.rules
