XEN_ROOT = $(CURDIR)/../../..
OCAML_TOPLEVEL = $(CURDIR)/..
include $(OCAML_TOPLEVEL)/common.make

xtl_LIBS = xentoollog

xtl_OBJS = xtl

send_debug_keys_LIBS = xentoollog xenlight

send_debug_keys_OBJS = xtl send_debug_keys

list_domains_LIBS = xentoollog xenlight

list_domains_OBJS = xtl list_domains

raise_exception_LIBS =  xentoollog xenlight

raise_exception_OBJS = raise_exception

dmesg_LIBS =  xentoollog xenlight

dmesg_OBJS = xtl dmesg

OCAML_PROGRAM = xtl send_debug_keys list_domains raise_exception dmesg

install:

uninstall:

include $(OCAML_TOPLEVEL)/Makefile.rules
