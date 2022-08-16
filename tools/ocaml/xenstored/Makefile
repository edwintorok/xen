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

LIBS = syslog.cma syslog.cmxa poll.cma poll.cmxa
syslog_OBJS = syslog
syslog_C_OBJS = syslog_stubs
poll_OBJS = poll
poll_C_OBJS = select_stubs
OCAML_LIBRARY = syslog poll

OBJS = paths \
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

INTF = symbol.cmi trie.cmi syslog.cmi poll.cmi

XENSTOREDLIBS = \
	unix.cmxa \
	-ccopt -L -ccopt . syslog.cmxa \
	-ccopt -L -ccopt . poll.cmxa \
	-ccopt -L -ccopt $(OCAML_TOPLEVEL)/libs/xenmmap $(OCAML_TOPLEVEL)/libs/xenmmap/xenmmap.cmxa \
	-ccopt -L -ccopt $(OCAML_TOPLEVEL)/libs/xeneventchn $(OCAML_TOPLEVEL)/libs/xeneventchn/xeneventchn.cmxa \
	-ccopt -L -ccopt $(OCAML_TOPLEVEL)/libs/xenctrl $(OCAML_TOPLEVEL)/libs/xenctrl/xenctrl.cmxa \
	-ccopt -L -ccopt $(OCAML_TOPLEVEL)/libs/xenbus $(OCAML_TOPLEVEL)/libs/xenbus/xenbus.cmxa \
	-ccopt -L -ccopt $(XEN_ROOT)/tools/libs/ctrl

PROGRAMS = oxenstored

oxenstored_LIBS = $(XENSTOREDLIBS)
oxenstored_OBJS = $(OBJS)

OCAML_PROGRAM = oxenstored

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

install: all
	$(INSTALL_DIR) $(DESTDIR)$(sbindir)
	$(INSTALL_PROG) oxenstored $(DESTDIR)$(sbindir)
	$(INSTALL_DIR) $(DESTDIR)$(XEN_CONFIG_DIR)
	$(INSTALL_DATA) oxenstored.conf $(DESTDIR)$(XEN_CONFIG_DIR)

uninstall:
	rm -f $(DESTDIR)$(XEN_CONFIG_DIR)/oxenstored.conf
	rm -f $(DESTDIR)$(sbindir)/oxenstored

include $(OCAML_TOPLEVEL)/Makefile.rules
