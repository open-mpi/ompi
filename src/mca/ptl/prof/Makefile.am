#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level OpenMPI Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

EXTRA_DIST = VERSION

# According to the MCA spec, we have to make the output library here
# in the top-level directory, and it has to be named
# libompi_ssi_coll_ompi_basic.la

AM_CPPFLAGS = \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_builddir)/src/ompi/event \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

prof_la_SOURCES = ptl_prof.c ptl_prof.h \
        ptl_prof_component.c

if OMPI_BUILD_ptl_prof_DSO
component_noinst =
component_install = mca_ptl_prof.la
else
component_noinst = libmca_ptl_prof.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ptl_prof_la_SOURCES = $(prof_la_SOURCES)
mca_ptl_prof_la_LIBADD = $(LIBOMPI_LA)
mca_ptl_prof_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_ptl_prof_la_SOURCES = $(prof_la_SOURCES)
libmca_ptl_prof_la_LIBADD = 
libmca_ptl_prof_la_LDFLAGS = -module -avoid-version

