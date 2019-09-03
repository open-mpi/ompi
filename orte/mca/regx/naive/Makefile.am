#
# Copyright (c) 2016-2018 Intel, Inc.  All rights reserved.
# Copyright (c) 2019      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
          regx_naive_component.c \
          regx_naive.h \
          regx_naive.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_regx_naive_DSO
component_noinst =
component_install = mca_regx_naive.la
else
component_noinst = libmca_regx_naive.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_regx_naive_la_SOURCES = $(sources)
mca_regx_naive_la_LDFLAGS = -module -avoid-version
mca_regx_naive_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_regx_naive_la_SOURCES = $(sources)
libmca_regx_naive_la_LDFLAGS = -module -avoid-version
