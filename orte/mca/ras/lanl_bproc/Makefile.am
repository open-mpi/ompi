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

dist_pkgdata_DATA = help-ras-lanl-bproc.txt



# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ras_lanl_bproc_DSO
component_noinst =
component_install = mca_ras_lanl_bproc.la
else
component_noinst = libmca_ras_lanl_bproc.la
component_install =
endif

AM_CPPFLAGS= $(ras_lanl_bproc_CPPFLAGS)

proxy_SOURCES = \
    ras_lanl_bproc.c \
    ras_lanl_bproc.h \
    ras_lanl_bproc_component.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_ras_lanl_bproc_la_SOURCES = $(proxy_SOURCES)
mca_ras_lanl_bproc_la_LIBADD = \
    $(ras_lanl_bproc_LIBS) \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la
mca_ras_lanl_bproc_la_LDFLAGS = -module -avoid-version $(ras_lanl_bproc_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_ras_lanl_bproc_la_SOURCES = $(proxy_SOURCES)
libmca_ras_lanl_bproc_la_LIBADD = $(ras_lanl_bproc_LIBS)
libmca_ras_lanl_bproc_la_LDFLAGS = -module -avoid-version $(ras_lanl_bproc_LDFLAGS)
