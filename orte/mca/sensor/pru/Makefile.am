#
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-orte-sensor-pru.txt

sources = \
        sensor_pru.c \
        sensor_pru.h \
        sensor_pru_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_sensor_pru_DSO
component_noinst =
component_install = mca_sensor_pru.la
else
component_noinst = libmca_sensor_pru.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sensor_pru_la_SOURCES = $(sources)
mca_sensor_pru_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sensor_pru_la_SOURCES =$(sources)
libmca_sensor_pru_la_LDFLAGS = -module -avoid-version
