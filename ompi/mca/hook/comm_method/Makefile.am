#
# Copyright (c) 2018      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
	hook_comm_method.h \
	hook_comm_method_component.c \
	hook_comm_method_fns.c

# This component will only ever be built statically -- never as a DSO.

noinst_LTLIBRARIES = libmca_hook_comm_method.la

libmca_hook_comm_method_la_SOURCES = $(sources)
libmca_hook_comm_method_la_LDFLAGS = -module -avoid-version
