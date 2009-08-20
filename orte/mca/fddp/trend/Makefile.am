#
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        fddp_trend.c \
        fddp_trend.h \
        fddp_trend_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_fddp_trend_DSO
component_noinst =
component_install = mca_fddp_trend.la
else
component_noinst = libmca_fddp_trend.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_fddp_trend_la_SOURCES = $(sources)
mca_fddp_trend_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_fddp_trend_la_SOURCES =$(sources)
libmca_fddp_trend_la_LDFLAGS = -module -avoid-version
