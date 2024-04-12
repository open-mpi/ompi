#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(pcompress_zlib_CPPFLAGS)

sources = \
        compress_zlib.h \
        compress_zlib_component.c \
        compress_zlib.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_pmix_pcompress_zlib_DSO
component_noinst =
component_install = pmix_mca_pcompress_zlib.la
else
component_noinst = libpmix_mca_pcompress_zlib.la
component_install =
endif

mcacomponentdir = $(pmixlibdir)
mcacomponent_LTLIBRARIES = $(component_install)
pmix_mca_pcompress_zlib_la_SOURCES = $(sources)
pmix_mca_pcompress_zlib_la_LDFLAGS = -module -avoid-version $(pcompress_zlib_LDFLAGS)
pmix_mca_pcompress_zlib_la_LIBADD = $(pcompress_zlib_LIBS)
if NEED_LIBPMIX
pmix_mca_pcompress_zlib_la_LIBADD += $(top_builddir)/src/libpmix.la
endif

noinst_LTLIBRARIES = $(component_noinst)
libpmix_mca_pcompress_zlib_la_SOURCES = $(sources)
libpmix_mca_pcompress_zlib_la_LDFLAGS = -module -avoid-version $(pcompress_zlib_LDFLAGS)
libpmix_mca_pcompress_zlib_la_LIBADD = $(pcompress_zlib_LIBS)
