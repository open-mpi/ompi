#
# Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_prtedata_DATA = help-prte-rmaps-ppr.txt

sources = \
        rmaps_ppr.c \
        rmaps_ppr.h \
        rmaps_ppr_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_prte_rmaps_ppr_DSO
component_noinst =
component_install = prte_mca_rmaps_ppr.la
else
component_noinst = libprtemca_rmaps_ppr.la
component_install =
endif

mcacomponentdir = $(prtelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
prte_mca_rmaps_ppr_la_SOURCES = $(sources)
prte_mca_rmaps_ppr_la_LDFLAGS = -module -avoid-version
prte_mca_rmaps_ppr_la_LIBADD = $(top_builddir)/src/libprrte.la

noinst_LTLIBRARIES = $(component_noinst)
libprtemca_rmaps_ppr_la_SOURCES =$(sources)
libprtemca_rmaps_ppr_la_LDFLAGS = -module -avoid-version
