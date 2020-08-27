TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(TOPLEVEL)/../..
include $(TOPLEVEL)/common.make

INTF = $(foreach obj, $(OBJS),$(obj).cmi)

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

# gnt is an internal library, not installed
gnt_OBJS = gnt
gnt_C_OBJS = gnttab_stubs
gnt_LIBS = gnt.cma gnt.cmxa
LIBS_gnt = $(LDLIBS_libxengnttab)
CFLAGS += $(CFLAGS_libxengnttab)

xenmmap_OBJS = xenmmap
xenmmap_C_OBJS = xenmmap_stubs
xenmmap_LIBS = xenmmap.cma xenmmap.cmxa

OCAML_LIBRARY = xenmmap gnt
OBJS = $(xenmmap_OBJS) $(gnt_OBJS)
LIBS = $(xenmmap_LIBS) $(gnt_LIBS)

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	$(OCAMLFIND) remove -destdir $(OCAMLDESTDIR) xenmmap
	$(OCAMLFIND) install -destdir $(OCAMLDESTDIR) -ldconf ignore xenmmap META *xenmmap*.cmi $(xenmmap_LIBS) *xenmmap*.a *xenmmap*.so *xenmmap*.cmx

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove -destdir $(OCAMLDESTDIR) xenmmap

include $(TOPLEVEL)/Makefile.rules

