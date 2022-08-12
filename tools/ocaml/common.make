include $(XEN_ROOT)/tools/Rules.mk

CC ?= gcc
OCAMLLEX ?= ocamllex
OCAMLYACC ?= ocamlyacc
OCAMLFIND ?= ocamlfind

CFLAGS += -fPIC -Werror -I$(shell ocamlc -where)

OCAMLOPTFLAGS = -g -ccopt "$(LDFLAGS)" $(OCAMLINCLUDE) -cc $(CC) -w F -warn-error F
OCAMLCFLAGS += -g $(OCAMLINCLUDE) -w F -warn-error F

o= >$@.new && mv -f $@.new $@
