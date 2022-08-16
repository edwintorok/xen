OCAML_TOPLEVEL=$(CURDIR)/../..
XEN_ROOT=$(OCAML_TOPLEVEL)/../..
include $(OCAML_TOPLEVEL)/common.make

CFLAGS += $(CFLAGS_libxenevtchn) $(CFLAGS_xeninclude)

CDEPS_xeneventchn = xenevtchn

OCAML_LIBRARY = xeneventchn

include $(OCAML_TOPLEVEL)/Makefile.rules
