#
# Copyright (c) 2026      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
	hook_fault_injector.h \
	hook_fault_injector_component.c \
	hook_fault_injector_fns.c

# This component will only ever be built statically -- never as a DSO.

noinst_LTLIBRARIES = libmca_hook_fault_injector.la

libmca_hook_fault_injector_la_SOURCES = $(sources)
libmca_hook_fault_injector_la_LDFLAGS = -module -avoid-version
