#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004 The Ohio State University.
#                    All rights reserved.
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

AM_CPPFLAGS = $(ptl_gm_CPPFLAGS)

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

#if OMPI_ENABLE_GM_CACHE
#  additional_source = ptl_gm_regcache.c
#else
#  additional_source =
#endif

libmca_ptl_gm_la_sources = $(additional_source) \
        ptl_gm.c \
        ptl_gm.h \
        ptl_gm_component.c \
        ptl_gm_peer.h \
        ptl_gm_priv.h \
        ptl_gm_priv.c \
        ptl_gm_proc.c \
        ptl_gm_proc.h \
        ptl_gm_sendfrag.c \
        ptl_gm_sendfrag.h \
        ptl_gm_memory.c

if OMPI_BUILD_ptl_gm_DSO
component_noinst =
component_install = mca_ptl_gm.la
else
component_noinst = libmca_ptl_gm.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)

mca_ptl_gm_la_SOURCES = $(libmca_ptl_gm_la_sources)
mca_ptl_gm_la_LIBADD = \
    $(ptl_gm_LIBS) \
    $(top_ompi_builddir)/ompi/libmpi.la \
    $(top_ompi_builddir)/orte/liborte.la \
    $(top_ompi_builddir)/opal/libopal.la
mca_ptl_gm_la_LDFLAGS = -module -avoid-version $(ptl_gm_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_ptl_gm_la_SOURCES = $(libmca_ptl_gm_la_sources)
libmca_ptl_gm_la_LIBADD = $(ptl_gm_LIBS)
libmca_ptl_gm_la_LDFLAGS = -module -avoid-version $(ptl_gm_LDFLAGS)

