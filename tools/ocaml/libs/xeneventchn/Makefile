OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

CFLAGS += $(CFLAGS_libxenevtchn) $(CFLAGS_xeninclude)

LIBS_xeneventchn = $(LDLIBS_libxenevtchn)

OCAML_LIBRARY = xeneventchn

include $(OCAML_TOPLEVEL)/Makefile.rules
