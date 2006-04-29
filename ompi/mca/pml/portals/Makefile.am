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

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

AM_CPPFLAGS = $(pml_portals_CPPFLAGS)

if OMPI_BUILD_pml_portals_DSO
component_noinst =
component_install = mca_pml_portals.la
else
component_noinst = libmca_pml_portals.la
component_install =
endif

local_sources = \
	pml_portals.c \
	pml_portals.h \
	pml_portals_component.c \
	pml_portals_probe.c \
	pml_portals_recv.c \
	pml_portals_send.c \
	pml_portals_progress.c \
	pml_portals_start.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_portals_la_SOURCES = $(local_sources)
mca_pml_portals_la_LIBADD = \
    $(pml_portals_LIBS) \
    $(top_ompi_builddir)/ompi/libmpi.la \
    $(top_ompi_builddir)/orte/liborte.la \
    $(top_ompi_builddir)/opal/libopal.la
mca_pml_portals_la_LDFLAGS = -module -avoid-version $(pml_portals_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_portals_la_SOURCES = $(local_sources)
libmca_pml_portals_la_LIBADD = $(pml_portals_LIBS)
libmca_pml_portals_la_LDFLAGS = -module -avoid-version $(pml_portals_LDFLAGS)

