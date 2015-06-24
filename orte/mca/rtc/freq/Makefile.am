#
# Copyright (c) 2014      Intel, Inc. All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_ortedata_DATA = help-rtc-freq.txt

sources = \
        rtc_freq.c \
        rtc_freq.h \
        rtc_freq_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_rtc_freq_DSO
component_noinst =
component_install = mca_rtc_freq.la
else
component_noinst = libmca_rtc_freq.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rtc_freq_la_SOURCES = $(sources)
mca_rtc_freq_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rtc_freq_la_SOURCES =$(sources)
libmca_rtc_freq_la_LDFLAGS = -module -avoid-version
