#
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
	hook_demo.h \
	hook_demo_component.c \
	hook_demo_fns.c

# This component will only ever be built statically -- never as a DSO.

noinst_LTLIBRARIES = libmca_hook_demo.la

libmca_hook_demo_la_SOURCES = $(sources)
libmca_hook_demo_la_LDFLAGS = -module -avoid-version
