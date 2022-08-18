XEN_ROOT = $(CURDIR)/../../..
OCAML_TOPLEVEL = $(CURDIR)/..
include $(OCAML_TOPLEVEL)/common.make

OCAML_PROGRAM = oxenstored

# Include configure output (config.h)
oxenstored_CFLAGS += -include $(XEN_ROOT)/tools/config.h

oxenstored_OBJS = poll syslog
oxenstored_OBJS += paths \
	define \
	stdext \
	trie \
	config \
	packet \
	logging \
	quota \
	perms \
	symbol \
	utils \
	store \
	disk \
	transaction \
	event \
	domain \
	domains \
	connection \
	connections \
	history \
	parse_arg \
	process \
	xenstored

oxenstored_LIBS=unix xenmmap xeneventchn xenctrl xenbus

oxenstored_EXTRA_DEPS = select_stubs.o syslog_stubs.o

install: all
	$(INSTALL_DIR) $(DESTDIR)$(sbindir)
	$(INSTALL_PROG) oxenstored $(DESTDIR)$(sbindir)
	$(INSTALL_DIR) $(DESTDIR)$(XEN_CONFIG_DIR)
	$(INSTALL_DATA) oxenstored.conf $(DESTDIR)$(XEN_CONFIG_DIR)

uninstall:
	rm -f $(DESTDIR)$(XEN_CONFIG_DIR)/oxenstored.conf
	rm -f $(DESTDIR)$(sbindir)/oxenstored

include $(OCAML_TOPLEVEL)/Makefile.rules
