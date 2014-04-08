#
# Copyright (c) 2011      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST =

portals4_sources = \
	osc_portals4.h \
	osc_portals4_comm.c \
	osc_portals4_component.c \
	osc_portals4_active_target.c \
	osc_portals4_passive_target.c \
	osc_portals4_request.c \
        osc_portals4_request.h

AM_CPPFLAGS = $(osc_portals4_CPPFLAGS)

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_osc_portals4_DSO
component_noinst =
component_install = mca_osc_portals4.la
else
component_noinst = libmca_osc_portals4.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_osc_portals4_la_SOURCES = $(portals4_sources)
mca_osc_portals4_la_LIBADD =  $(osc_portals4_LIBS)
mca_osc_portals4_la_LDFLAGS = -module -avoid-version $(osc_portals4_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_osc_portals4_la_SOURCES = $(portals4_sources)
libmca_osc_portals4_la_LIBADD = $(osc_portals4_LIBS)
libmca_osc_portals4_la_LDFLAGS = -module -avoid-version $(osc_portals4_LDFLAGS)
