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

if OMPI_BUILD_ptl_self_DSO
component_noinst =
component_install = mca_ptl_self.la
else
component_noinst = libmca_ptl_self.la
component_install =
endif

self_SOURCES = ptl_self.c ptl_self.h ptl_self_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ptl_self_la_SOURCES = $(self_SOURCES)
mca_ptl_self_la_LIBADD = 
mca_ptl_self_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_ptl_self_la_SOURCES = $(self_SOURCES)
libmca_ptl_self_la_LIBADD =
libmca_ptl_self_la_LDFLAGS = -module -avoid-version
