#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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



AM_CPPFLAGS = $(mpool_openib_CPPFLAGS)

sources = \
	mpool_openib.h \
	mpool_openib_module.c \
	mpool_openib_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_mpool_openib_DSO
component_noinst =
component_install = mca_mpool_openib.la
else
component_noinst = libmca_mpool_openib.la
component_install =
endif

# See src/mca/ptl/openib/Makefile.am for an explanation of
# libmca_common_openib.la.

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mpool_openib_la_SOURCES = $(sources)
mca_mpool_openib_la_LDFLAGS = -module -avoid-version $(mpool_openib_LDFLAGS)
mca_mpool_openib_la_LIBADD = \
    $(mpool_openib_LIBS) \
    $(top_ompi_builddir)/ompi/libmpi.la \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpool_openib_la_SOURCES = $(sources)
libmca_mpool_openib_la_LDFLAGS = -module -avoid-version $(mpool_openib_LDFLAGS)
libmca_mpool_openib_la_LIBADD = $(mpool_openib_LIBS)

