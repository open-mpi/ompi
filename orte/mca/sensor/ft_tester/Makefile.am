#
# Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        sensor_ft_tester.c \
        sensor_ft_tester.h \
        sensor_ft_tester_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_sensor_ft_tester_DSO
component_noinst =
component_install = mca_sensor_ft_tester.la
else
component_noinst = libmca_sensor_ft_tester.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sensor_ft_tester_la_SOURCES = $(sources)
mca_sensor_ft_tester_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sensor_ft_tester_la_SOURCES =$(sources)
libmca_sensor_ft_tester_la_LDFLAGS = -module -avoid-version
