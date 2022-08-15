XEN_ROOT = $(CURDIR)/../../..
OCAML_TOPLEVEL = $(CURDIR)/..
include $(OCAML_TOPLEVEL)/common.make

CFLAGS += $(APPEND_CFLAGS)

OCAMLINCLUDE += \
	-I $(OCAML_TOPLEVEL)/libs/xentoollog \
	-I $(OCAML_TOPLEVEL)/libs/xl

OBJS = xtl

PROGRAMS = xtl

xtl_LIBS =  \
	-ccopt -L -ccopt $(OCAML_TOPLEVEL)/libs/xentoollog $(OCAML_TOPLEVEL)/libs/xentoollog/xentoollog.cmxa

xtl_OBJS = xtl

OCAML_PROGRAM = xtl

all: $(PROGRAMS)

bins: $(PROGRAMS)

install:

uninstall:

include $(OCAML_TOPLEVEL)/Makefile.rules
