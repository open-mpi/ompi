#
# Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-orte-sensor-sigar.txt

sources = \
        sensor_sigar.c \
        sensor_sigar.h \
        sensor_sigar_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_sensor_sigar_DSO
component_noinst =
component_install = mca_sensor_sigar.la
else
component_noinst = libmca_sensor_sigar.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sensor_sigar_la_CPPFLAGS = $(sensor_sigar_CPPFLAGS)
mca_sensor_sigar_la_SOURCES = $(sources)
mca_sensor_sigar_la_LDFLAGS = -module -avoid-version $(sensor_sigar_LDFLAGS)
mca_sensor_sigar_la_LIBADD = $(sensor_sigar_LIBS) -lm

noinst_LTLIBRARIES = $(component_noinst)
libmca_sensor_sigar_la_SOURCES =$(sources)
libmca_sensor_sigar_la_CPPFLAGS = $(sensor_sigar_CPPFLAGS)
libmca_sensor_sigar_la_LDFLAGS = -module -avoid-version $(sensor_sigar_LDFLAGS)
libmca_sensor_sigar_la_LIBADD = $(sensor_sigar_LIBS) -lm
