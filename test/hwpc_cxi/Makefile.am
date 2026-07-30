# -*- Mode: Makefile; c-basic-offset:4 ; indent-tabs-mode:nil -*-
#
# SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
# SPDX-License-Identifier:  MIT
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This test requires CXI hardware and multiple processes to run. 
# Don't run it as part of 'make check' by default.
if PROJECT_OMPI
if HWPC_CXI_ENABLE
    noinst_PROGRAMS = hwpc_cxi_sendrecv_test

    hwpc_cxi_sendrecv_test_SOURCES = hwpc_cxi_sendrecv_test.c
    hwpc_cxi_sendrecv_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_sendrecv_test_LDADD = \
        $(top_builddir)/ompi/.libs/lib@OMPI_LIBMPI_NAME@.so \
        $(top_builddir)/opal/.libs/lib@OPAL_LIB_NAME@.so

    # Distribute test scripts and documentation
    EXTRA_DIST = run_hwpc_cxi_tests.sh README.md HWPC_CXI_GUIDE.md
endif # HWPC_CXI_ENABLE
endif # PROJECT_OMPI

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo hwpc_cxi_sendrecv_test prof *.log *.o *.trs Makefile
