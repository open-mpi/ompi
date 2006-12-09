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

dist_pkgdata_DATA = help-pls-bproc.txt

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pls_bproc_DSO
component_noinst =
component_install = mca_pls_bproc.la
else
component_noinst = libmca_pls_bproc.la
component_install =
endif

sources = \
    pls_bproc.h \
    pls_bproc.c \
    pls_bproc_state.c \
    pls_bproc_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pls_bproc_la_SOURCES = $(sources)
mca_pls_bproc_la_LIBADD = \
    $(pls_bproc_LIBS) \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la
mca_pls_bproc_la_LDFLAGS = -module -avoid-version $(pls_bproc_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pls_bproc_la_SOURCES = $(sources)
libmca_pls_bproc_la_LIBADD = $(pls_bproc_LIBS)
libmca_pls_bproc_la_LDFLAGS = -module -avoid-version $(pls_bproc_LDFLAGS)
