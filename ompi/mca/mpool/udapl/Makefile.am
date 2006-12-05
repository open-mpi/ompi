#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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


CFLAGS = $(mpool_udapl_CFLAGS)
AM_CPPFLAGS = $(mpool_udapl_CPPFLAGS)

sources = \
	mpool_udapl.h \
	mpool_udapl_module.c \
	mpool_udapl_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_mpool_udapl_DSO
component_noinst =
component_install = mca_mpool_udapl.la
else
component_noinst = libmca_mpool_udapl.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_mpool_udapl_la_SOURCES = $(sources)
mca_mpool_udapl_la_LIBADD = \
    $(mpool_udapl_LIBS) \
    $(top_ompi_builddir)/ompi/libmpi.la \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la
mca_mpool_udapl_la_LDFLAGS = -module -avoid-version $(mpool_udapl_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpool_udapl_la_SOURCES = $(sources)
libmca_mpool_udapl_la_LIBADD = $(mpool_udapl_LIBS)
libmca_mpool_udapl_la_LDFLAGS = -module -avoid-version $(mpool_udapl_LDFLAGS)
