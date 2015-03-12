#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        dl_dlopen.h \
        dl_dlopen_component.c \
        dl_dlopen_module.c

# This component will only ever be built statically -- never as a DSO.

noinst_LTLIBRARIES = libmca_dl_dlopen.la

libmca_dl_dlopen_la_SOURCES = $(sources)
libmca_dl_dlopen_la_LDFLAGS = -module -avoid-version
libmca_dl_dlopen_la_LIBADD = $(opal_dl_dlopen_LIBS)
