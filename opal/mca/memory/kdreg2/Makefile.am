#
# Copyright (c) 2026      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#

EXTRA_DIST = help-opal-memory-kdreg2.txt

AM_CPPFLAGS = $(memory_kdreg2_CPPFLAGS)

sources = \
    memory_kdreg2.h \
    memory_kdreg2_component.c

if MCA_BUILD_opal_memory_kdreg2_DSO
component_noinst =
component_install = mca_memory_kdreg2.la
else
component_noinst = libmca_memory_kdreg2.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_memory_kdreg2_la_SOURCES = $(sources)
mca_memory_kdreg2_la_LDFLAGS = -module -avoid-version
mca_memory_kdreg2_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_memory_kdreg2_la_SOURCES = $(sources)
libmca_memory_kdreg2_la_LDFLAGS = -module -avoid-version
