XEN_ROOT = $(CURDIR)/../..
include common.make
include Makefile.rules

INTERFACES=$(wildcard libs/*/*.mli xenstored/*.mli)
IMPLS=$(wildcard libs/*/*.ml xenstored/*.ml)

ALL_OCAML_OBJ_SOURCES=$(INTERFACES) $(IMPLS)

PROG=xenstored/xenstored

.PHONY: all
all: $(PROG)

OCAMLINCLUDE:=$(addprefix -I ,$(wildcard libs/*/ xenstored))

.ocamldep.make: $(ALL_OCAML_OBJ_SOURCES) Makefile Makefile.rules
	$(call quiet-command, $(OCAMLDEP) $(OCAMLINCLUDE) $(ALL_OCAML_OBJ_SOURCES) >.ocamldep.make.tmp,MLDEP,)
	@echo -n $(PROG): >>.ocamldep.make.tmp
	$(call quiet-command, $(OCAMLDEP) $(OCAMLINCLUDE) -sort $(IMPLS) | sed -e 's/\.ml/\.cmx/g' >>.ocamldep.make.tmp,MLDEPSORT,)
	@mv .ocamldep.make.tmp $@

include .ocamldep.make

libs/xc/xenctrl_abi_check.h: libs/xc/abi-check libs/xc/xenctrl_stubs.c libs/xc/xenctrl.ml
		$(PERL) -w $^ >$@.tmp && mv -f $@.tmp $@
GENERATED_FILES += libs/xc/xenctrl_abi_check.h
libs/xc/xenctrl_stubs.o:: libs/xc/xenctrl_abi_check.h

CFLAGS+=-I ../include/ -I libs/mmap
STUBS=$(wildcard libs/*/*_stubs.c xenstored/*_stubs.c)

# can be used in ocamlmklib
PROG_DEPS=evtchn guest ctrl
#LDLIBS=$(foreach DEP,$(PROG_DEPS),$(realpath ../libs/$(DEP)/libxen$(DEP).so))
LDLIBS=$(addprefix -lxen,$(PROG_DEPS))
PROG_SHDEPS=$(foreach DEP,$(PROG_DEPS),$(SHDEPS_libxen$(DEP)))
PROG_LIBDIRS=$(foreach DEP,$(PROG_DEPS),-L../libs/$(DEP))
PROG_LDFLAGS=$(PROG_SHDEPS) $(PROG_LIBDIRS) $(LDLIBS)
# do not use -Wl,-rpath-link: it'll end up in the .cmxa, instead use -L when linking oxenstored,
# when linking anything else out-of-tree none of these rpath flags are needed
OCAMLOPT_LINK=-I +unix unix.cmxa
$(PROG): $(STUBS:.c=.o)
		$(call quiet-command, $(OCAMLOPT) -o $@ $(OCAMLINCLUDE) $(OCAMLOPT_LINK) $^ $(addprefix -cclib ,$(PROG_LDFLAGS)),MLOPT,$@)

.PHONY: clean
clean:
		rm -f .ocamldep.make libs/*/*.so libs/*/*.cmxa libs/*/*.cma libs/*/*.cmi libs/*/*.cmo libs/*/*.cmx xenstored/*.cma xenstored/*.cmi xenstored/*.cmi xenstored/*.cmo xenstored/*.cmx libs/*/*.a libs/*/*.o xenstored/*.a xenstored/*.o $(PROG)
