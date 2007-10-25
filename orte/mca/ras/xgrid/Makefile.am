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

AM_CPPFLAGS = -I$(top_ompi_builddir)/include
AM_OBJCFLAGS = $(ras_xgrid_OBJCFLAGS)

xgrid_sources = \
	src/ras_xgrid.h \
        src/ras_xgrid_component.c \
        src/ras_xgrid_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ras_xgrid_DSO
component_noinst =
component_install = mca_ras_xgrid.la
else
component_noinst = libmca_ras_xgrid.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ras_xgrid_la_SOURCES = $(xgrid_sources)
mca_ras_xgrid_la_LIBADD = \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la
mca_ras_xgrid_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_ras_xgrid_la_SOURCES = $(xgrid_sources)
libmca_ras_xgrid_la_LIBADD = 
libmca_ras_xgrid_la_LDFLAGS = -module -avoid-version
