#
# Copyright (c) 2011      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST =

sm_sources = \
	osc_sm.h \
	osc_sm_comm.c \
	osc_sm_component.c \
	osc_sm_active_target.c \
	osc_sm_passive_target.c

AM_CPPFLAGS = $(osc_sm_CPPFLAGS)

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_osc_sm_DSO
component_noinst =
component_install = mca_osc_sm.la
else
component_noinst = libmca_osc_sm.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_osc_sm_la_SOURCES = $(sm_sources)
mca_osc_sm_la_LIBADD =  $(osc_sm_LIBS)
mca_osc_sm_la_LDFLAGS = -module -avoid-version $(osc_sm_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_osc_sm_la_SOURCES = $(sm_sources)
libmca_osc_sm_la_LIBADD = $(osc_sm_LIBS)
libmca_osc_sm_la_LDFLAGS = -module -avoid-version $(osc_sm_LDFLAGS)
