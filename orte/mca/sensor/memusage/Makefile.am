#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-orte-sensor-memusage.txt

sources = \
        sensor_memusage.c \
        sensor_memusage.h \
        sensor_memusage_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_sensor_memusage_DSO
component_noinst =
component_install = mca_sensor_memusage.la
else
component_noinst = libmca_sensor_memusage.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sensor_memusage_la_SOURCES = $(sources)
mca_sensor_memusage_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sensor_memusage_la_SOURCES =$(sources)
libmca_sensor_memusage_la_LDFLAGS = -module -avoid-version
