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
        dl_libltdl.h \
        dl_libltdl_component.c \
        dl_libltdl_module.c

# This component will only ever be built statically -- never as a DSO.

noinst_LTLIBRARIES = libmca_dl_libltdl.la

libmca_dl_libltdl_la_SOURCES = $(sources)
libmca_dl_libltdl_la_CPPFLAGS = $(opal_dl_libltdl_CPPFLAGS)
libmca_dl_libltdl_la_LDFLAGS = \
        $(opal_dl_libltdl_LDFLAGS) \
        -module -avoid-version
libmca_dl_libltdl_la_LIBADD = $(opal_dl_libltdl_LIBS)
