#
# Copyright (c) 2015      Intel, Inc. All rights reserved
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        rtc_omp.c \
        rtc_omp.h \
        rtc_omp_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_rtc_omp_DSO
component_noinst =
component_install = mca_rtc_omp.la
else
component_noinst = libmca_rtc_omp.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rtc_omp_la_SOURCES = $(sources)
mca_rtc_omp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rtc_omp_la_SOURCES =$(sources)
libmca_rtc_omp_la_LDFLAGS = -module -avoid-version
