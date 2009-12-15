#
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_rmcast_tcp_DSO
component_noinst =
component_install = mca_rmcast_tcp.la
else
component_noinst = libmca_rmcast_tcp.la
component_install =
endif

rmcast_tcp_SOURCES = \
        rmcast_tcp.c \
        rmcast_tcp.h \
        rmcast_tcp_component.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rmcast_tcp_la_SOURCES = $(rmcast_tcp_SOURCES)
mca_rmcast_tcp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rmcast_tcp_la_SOURCES = $(rmcast_tcp_SOURCES)
libmca_rmcast_tcp_la_LIBADD =
libmca_rmcast_tcp_la_LDFLAGS = -module -avoid-version
