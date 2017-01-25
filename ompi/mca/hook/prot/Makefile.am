#
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
	hook_prot.h \
	hook_prot_component.c \
	hook_prot_fns.c

# This component will only ever be built statically -- never as a DSO.

noinst_LTLIBRARIES = libmca_hook_prot.la

libmca_hook_prot_la_SOURCES = $(sources)
libmca_hook_prot_la_LDFLAGS = -module -avoid-version
