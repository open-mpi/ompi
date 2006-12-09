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

AM_CPPFLAGS = $(maffinity_libnuma_CPPFLAGS)

sources = \
        maffinity_libnuma.h \
        maffinity_libnuma_component.c \
        maffinity_libnuma_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_maffinity_libnuma_DSO
component_noinst =
component_install = mca_maffinity_libnuma.la
else
component_noinst = libmca_maffinity_libnuma.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_maffinity_libnuma_la_SOURCES = $(sources)
mca_maffinity_libnuma_la_LDFLAGS = -module -avoid-version $(maffinity_libnuma_LDFLAGS)
mca_maffinity_libnuma_la_LIBADD = \
    $(maffinity_libnuma_LIBS) \
    $(top_ompi_builddir)/opal/libopen-pal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_maffinity_libnuma_la_SOURCES =$(sources)
libmca_maffinity_libnuma_la_LIBADD = $(maffinity_libnuma_LIBS)
libmca_maffinity_libnuma_la_LDFLAGS = -module -avoid-version $(maffinity_libnuma_LDFLAGS)
