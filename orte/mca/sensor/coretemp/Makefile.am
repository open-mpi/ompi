#
# Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-orte-sensor-coretemp.txt

sources = \
        sensor_coretemp.c \
        sensor_coretemp.h \
        sensor_coretemp_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_sensor_coretemp_DSO
component_noinst =
component_install = mca_sensor_coretemp.la
else
component_noinst = libmca_sensor_coretemp.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sensor_coretemp_la_SOURCES = $(sources)
mca_sensor_coretemp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sensor_coretemp_la_SOURCES =$(sources)
libmca_sensor_coretemp_la_LDFLAGS = -module -avoid-version
