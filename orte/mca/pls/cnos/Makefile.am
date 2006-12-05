#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options



# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pls_cnos_DSO
component_noinst =
component_install = mca_pls_cnos.la
else
component_noinst = libmca_pls_cnos.la
component_install =
endif

cnos_SOURCES = \
    pls_cnos.c \
    pls_cnos.h \
    pls_cnos_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pls_cnos_la_SOURCES = $(cnos_SOURCES)
mca_pls_cnos_la_LIBADD = \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la
mca_pls_cnos_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pls_cnos_la_SOURCES = $(cnos_SOURCES)
libmca_pls_cnos_la_LIBADD =
libmca_pls_cnos_la_LDFLAGS = -module -avoid-version
