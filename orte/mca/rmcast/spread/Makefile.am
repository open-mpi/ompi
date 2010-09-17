#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(rmcast_spread_CPPFLAGS)

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_rmcast_spread_DSO
component_noinst =
component_install = mca_rmcast_spread.la
else
component_noinst = libmca_rmcast_spread.la
component_install =
endif

rmcast_spread_SOURCES = \
        rmcast_spread.c \
        rmcast_spread.h \
        rmcast_spread_component.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rmcast_spread_la_SOURCES = $(rmcast_spread_SOURCES)
mca_rmcast_spread_la_LDFLAGS = -module -avoid-version $(rmcast_spread_LDFLAGS)
mca_rmcast_spread_la_LIBADD = $(rmcast_spread_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_rmcast_spread_la_SOURCES = $(rmcast_spread_SOURCES)
libmca_rmcast_spread_la_LDFLAGS = -module -avoid-version $(rmcast_spread_LDFLAGS)
libmca_rmcast_spread_la_LIBADD = $(rmcast_spread_LIBS)
