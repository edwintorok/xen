XEN_ROOT = $(CURDIR)/../../..
include $(XEN_ROOT)/tools/Rules.mk

IS_CLEAN_ONLY_GOAL=1
include $(XEN_ROOT)/tools/ocaml/Makefile.rules
SUBDIRS=$(OCAML_LIBS)

.PHONY: all
all: subdirs-all

.PHONY: install
install: subdirs-install

.PHONY: uninstall
uninstall: subdirs-uninstall

.PHONY: clean
clean: subdirs-clean

.PHONY: distclean
distclean: subdirs-distclean
