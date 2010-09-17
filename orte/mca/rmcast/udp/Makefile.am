#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_rmcast_udp_DSO
component_noinst =
component_install = mca_rmcast_udp.la
else
component_noinst = libmca_rmcast_udp.la
component_install =
endif

dist_pkgdata_DATA = help-orte-rmcast-udp.txt

rmcast_udp_SOURCES = \
        rmcast_udp.c \
        rmcast_udp.h \
        rmcast_udp_component.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rmcast_udp_la_SOURCES = $(rmcast_udp_SOURCES)
mca_rmcast_udp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rmcast_udp_la_SOURCES = $(rmcast_udp_SOURCES)
libmca_rmcast_udp_la_LIBADD =
libmca_rmcast_udp_la_LDFLAGS = -module -avoid-version
