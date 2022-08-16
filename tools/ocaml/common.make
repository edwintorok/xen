include $(XEN_ROOT)/tools/Rules.mk

CC ?= gcc

CFLAGS += -fPIC -Werror -I$(shell ocamlc -where)

OCAMLFLAGS = -g -ccopt "$(LDFLAGS)" $(OCAMLINCLUDE) -cc $(CC) -w F -warn-error F

VERSION := 4.1

OCAMLDESTDIR ?= $(DESTDIR)$(shell $(OCAMLFIND) printconf destdir)

o= >$@.new && mv -f $@.new $@
