XEN_ROOT = $(CURDIR)/../../..
OCAML_TOPLEVEL = $(CURDIR)/..
include $(OCAML_TOPLEVEL)/common.make

# Include configure output (config.h)
CFLAGS += -include $(XEN_ROOT)/tools/config.h

CFLAGS  += $(APPEND_CFLAGS)
LDFLAGS += $(APPEND_LDFLAGS)

OCAMLINCLUDE += \
	-I $(OCAML_TOPLEVEL)/libs/xenbus \
	-I $(OCAML_TOPLEVEL)/libs/xenmmap \
	-I $(OCAML_TOPLEVEL)/libs/xenctrl \
	-I $(OCAML_TOPLEVEL)/libs/xeneventchn

oxenstored_LIBS = unix.cmxa
# for rpath
oxenstored_C_DEPS = evtchn ctrl guest
oxenstored_OCAML_DEPS = xenmmap xeneventchn xenctrl xenbus

PROGRAMS = oxenstored

OCAML_PROGRAM = oxenstored

install: all
	$(INSTALL_DIR) $(DESTDIR)$(sbindir)
	$(INSTALL_PROG) oxenstored $(DESTDIR)$(sbindir)
	$(INSTALL_DIR) $(DESTDIR)$(XEN_CONFIG_DIR)
	$(INSTALL_DATA) oxenstored.conf $(DESTDIR)$(XEN_CONFIG_DIR)

uninstall:
	rm -f $(DESTDIR)$(XEN_CONFIG_DIR)/oxenstored.conf
	rm -f $(DESTDIR)$(sbindir)/oxenstored

include $(OCAML_TOPLEVEL)/Makefile.rules
