XEN_ROOT = $(CURDIR)/../../..
OCAML_TOPLEVEL = $(CURDIR)/..
include $(OCAML_TOPLEVEL)/common.make

OCAMLINCLUDE += -I +compiler-libs
lintcstubs_arity_LIBS = ocamlcommon.cmxa

lintcstubs_arity_OBJS = lintcstubs_arity
OCAML_PROGRAM = lintcstubs_arity

PROGRAMS = $(OCAML_PROGRAM)

all: primitives.h

# generates a single include header because all primitives will eventually
# be linked into same binary, this ensures there are on conflicting
# declarations
primitives.h: $(PROGRAMS) Makefile
	./$(PROGRAMS) $(wildcard ../libs/*/*.ml ../*/*.ml) >$@

include $(OCAML_TOPLEVEL)/Makefile.rules
