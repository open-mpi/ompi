#
# Copyright (c) 2019      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
	hook_report_bindings_full.h \
	hook_report_bindings_full_component.c \
	hook_report_bindings_full_fns.c

# This component will only ever be built statically -- never as a DSO.

noinst_LTLIBRARIES = libmca_hook_report_bindings_full.la

libmca_hook_report_bindings_full_la_SOURCES = $(sources)
libmca_hook_report_bindings_full_la_LDFLAGS = -module -avoid-version
