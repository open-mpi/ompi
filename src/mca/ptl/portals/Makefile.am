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

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ptl_portals_DSO
component_noinst =
component_install = mca_ptl_portals.la
else
component_noinst = libmca_ptl_portals.la
component_install =
endif

portals_SOURCES = \
	ptl_portals.h \
	ptl_portals_nal.h \
	ptl_portals_component.c \
	ptl_portals_nal.c \
	ptl_portals_stubs.c


mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ptl_portals_la_SOURCES = $(portals_SOURCES)
mca_ptl_portals_la_LIBADD = 
mca_ptl_portals_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_ptl_portals_la_SOURCES = $(portals_SOURCES)
libmca_ptl_portals_la_LIBADD =
libmca_ptl_portals_la_LDFLAGS = -module -avoid-version
