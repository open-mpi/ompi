#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(compress_zlib_CPPFLAGS)

sources = \
        compress_zlib.h \
        compress_zlib_component.c \
        compress_zlib.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_compress_zlib_DSO
component_noinst =
component_install = mca_compress_zlib.la
else
component_noinst = libmca_compress_zlib.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_compress_zlib_la_SOURCES = $(sources)
mca_compress_zlib_la_LDFLAGS = -module -avoid-version $(compress_zlib_LDFLAGS)
mca_compress_zlib_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la $(compress_zlib_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_compress_zlib_la_SOURCES = $(sources)
libmca_compress_zlib_la_LDFLAGS = -module -avoid-version $(compress_zlib_LDFLAGS)
libmca_compress_zlib_la_LIBADD = $(compress_zlib_LIBS)
