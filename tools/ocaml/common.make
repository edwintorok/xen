include $(XEN_ROOT)/tools/Rules.mk

CC ?= gcc

OCAMLC_WHERE=$(shell $(OCAMLC) -where)
CFLAGS += -fPIC -Werror -I$(OCAMLC_WHERE)

OCAMLFLAGS = -g $(OCAMLINCLUDE) -cc $(CC) -w F -warn-error F

VERSION := 4.1

OCAMLDESTDIR ?= $(DESTDIR)$(shell $(OCAMLFIND) printconf destdir)

o= >$@.new && mv -f $@.new $@
