#
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(sensor_crsvm_CPPFLAGS)

dist_pkgdata_DATA = help-orte-sensor-crsvm.txt

sources = \
        sensor_crsvm.c \
        sensor_crsvm.h \
        sensor_crsvm_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_sensor_crsvm_DSO
component_noinst =
component_install = mca_sensor_crsvm.la
else
component_noinst = libmca_sensor_crsvm.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sensor_crsvm_la_SOURCES = $(sources)
mca_sensor_crsvm_la_LDFLAGS = -module -avoid-version $(sensor_crsvm_LDFLAGS)
mca_sensor_crsvm_la_LIBADD = $(sensor_crsvm_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_sensor_crsvm_la_SOURCES =$(sources)
libmca_sensor_crsvm_la_LDFLAGS = -module -avoid-version $(sensor_crsvm_LDFLAGS)
libmca_sensor_crsvm_la_LIBADD = $(sensor_crsvm_LIBS)
