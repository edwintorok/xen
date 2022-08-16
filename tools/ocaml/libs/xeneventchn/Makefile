OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

CFLAGS += $(CFLAGS_libxenevtchn) $(CFLAGS_xeninclude)

LIBS_xeneventchn = $(LDLIBS_libxenevtchn)

OCAML_LIBRARY = xeneventchn

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	$(OCAMLFIND) remove -destdir $(OCAMLDESTDIR) xeneventchn
	$(OCAMLFIND) install -destdir $(OCAMLDESTDIR) -ldconf ignore xeneventchn META $(INTF) $(LIBS) *.a *.so *.cmx

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove -destdir $(OCAMLDESTDIR) xeneventchn

include $(OCAML_TOPLEVEL)/Makefile.rules

