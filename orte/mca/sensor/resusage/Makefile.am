#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-orte-sensor-resusage.txt

sources = \
        sensor_resusage.c \
        sensor_resusage.h \
        sensor_resusage_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_sensor_resusage_DSO
component_noinst =
component_install = mca_sensor_resusage.la
else
component_noinst = libmca_sensor_resusage.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sensor_resusage_la_SOURCES = $(sources)
mca_sensor_resusage_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sensor_resusage_la_SOURCES =$(sources)
libmca_sensor_resusage_la_LDFLAGS = -module -avoid-version
