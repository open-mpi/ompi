#
# Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-orte-sensor-freq.txt

sources = \
        sensor_freq.c \
        sensor_freq.h \
        sensor_freq_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_sensor_freq_DSO
component_noinst =
component_install = mca_sensor_freq.la
else
component_noinst = libmca_sensor_freq.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sensor_freq_la_SOURCES = $(sources)
mca_sensor_freq_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sensor_freq_la_SOURCES =$(sources)
libmca_sensor_freq_la_LDFLAGS = -module -avoid-version
