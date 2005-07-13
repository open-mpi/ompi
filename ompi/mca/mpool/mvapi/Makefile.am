#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

CFLAGS = $(mpool_mvapi_CFLAGS)
AM_CPPFLAGS = $(mpool_mvapi_CPPFLAGS)

sources = \
	mpool_mvapi.h \
	mpool_mvapi_module.c \
	mpool_mvapi_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_mpool_mvapi_DSO
component_noinst =
component_install = mca_mpool_mvapi.la
else
component_noinst = libmca_mpool_mvapi.la
component_install =
endif

# See src/mca/ptl/mvapi/Makefile.am for an explanation of
# libmca_common_mvapi.la.

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_mpool_mvapi_la_SOURCES = $(sources)
mca_mpool_mvapi_la_LDFLAGS = -module -avoid-version $(mpool_mvapi_LDFLAGS)
mca_mpool_mvapi_la_LIBADD = $(mpool_mvapi_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpool_mvapi_la_SOURCES = $(sources)
libmca_mpool_mvapi_la_LDFLAGS = -module -avoid-version $(mpool_mvapi_LDFLAGS)
libmca_mpool_mvapi_la_LIBADD = $(mpool_mvapi_LIBS)


