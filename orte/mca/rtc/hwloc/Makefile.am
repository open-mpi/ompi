#
# Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_ortedata_DATA = help-orte-rtc-hwloc.txt

sources = \
        rtc_hwloc.c \
        rtc_hwloc.h \
        rtc_hwloc_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_rtc_hwloc_DSO
component_noinst =
component_install = mca_rtc_hwloc.la
else
component_noinst = libmca_rtc_hwloc.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rtc_hwloc_la_SOURCES = $(sources)
mca_rtc_hwloc_la_LDFLAGS = -module -avoid-version
mca_rtc_hwloc_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_rtc_hwloc_la_SOURCES =$(sources)
libmca_rtc_hwloc_la_LDFLAGS = -module -avoid-version
