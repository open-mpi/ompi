#
# Copyright (c) 2026      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#

# Make "make check" work in an uninstalled --enable-mca-dso build tree
include $(top_srcdir)/Makefile.mca-dso-check

TESTS = rcache_grdma_reuse

check_PROGRAMS = $(TESTS)

rcache_grdma_reuse_SOURCES = rcache_grdma_reuse.c
# Tell the test which memory components configure built, so that it
# can check that kdreg2 is used where its device can be opened
memory_components = $(MCA_opal_memory_STATIC_COMPONENTS) $(MCA_opal_memory_DSO_COMPONENTS)
rcache_grdma_reuse_CPPFLAGS = -DTEST_MEMORY_COMPONENTS='" $(memory_components) "'

LDFLAGS = $(OPAL_PKG_CONFIG_LDFLAGS)
LDADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

distclean-local:
	rm -rf *.dSYM .deps .libs *.log *.o *.trs $(check_PROGRAMS) Makefile
