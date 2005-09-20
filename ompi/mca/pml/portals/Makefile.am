#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
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
	pml_portals_cancel.c \
	pml_portals_component.c \
	pml_portals_iprobe.c \
	pml_portals_irecv.c \
	pml_portals_isend.c \
	pml_portals_ptl.c \
	pml_portals_ptl.h \
	pml_portals_progress.c \
	pml_portals_start.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_portals_la_SOURCES = $(local_sources)
mca_pml_portals_la_LIBADD = $(pml_portals_LIBS)
mca_pml_portals_la_LDFLAGS = -module -avoid-version $(pml_portals_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_portals_la_SOURCES = $(local_sources)
libmca_pml_portals_la_LIBADD = $(pml_portals_LIBS)
libmca_pml_portals_la_LDFLAGS = -module -avoid-version $(pml_portals_LDFLAGS)

