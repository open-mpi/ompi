#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_ortedata_DATA = help-orte-rmaps-ppr.txt

sources = \
        rmaps_ppr.c \
        rmaps_ppr.h \
        rmaps_ppr_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_rmaps_ppr_DSO
component_noinst =
component_install = mca_rmaps_ppr.la
else
component_noinst = libmca_rmaps_ppr.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rmaps_ppr_la_SOURCES = $(sources)
mca_rmaps_ppr_la_LDFLAGS = -module -avoid-version
mca_rmaps_ppr_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_rmaps_ppr_la_SOURCES =$(sources)
libmca_rmaps_ppr_la_LDFLAGS = -module -avoid-version
