#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

EXTRA_DIST = VERSION

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_ompi_builddir)/src/include \
    -I$(top_ompi_srcdir)/src -I$(top_ompi_srcdir)/src/include

xgrid_sources = \
	src/pls_xgrid.h \
        src/pls_xgrid_component.m \
        src/pls_xgrid_module.m \
	src/pls_xgrid_client.h \
	src/pls_xgrid_client.m

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pls_xgrid_DSO
component_noinst =
component_install = mca_pls_xgrid.la
else
component_noinst = libmca_pls_xgrid.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pls_xgrid_la_SOURCES = $(xgrid_sources)
mca_pls_xgrid_la_LIBADD = 
mca_pls_xgrid_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pls_xgrid_la_SOURCES = $(xgrid_sources)
libmca_pls_xgrid_la_LIBADD = 
libmca_pls_xgrid_la_LDFLAGS = -module -avoid-version
