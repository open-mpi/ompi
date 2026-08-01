# -*- Mode: Makefile; c-basic-offset:4 ; indent-tabs-mode:nil -*-
#
# SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#
# Copyright (c) 2026       Hewlett Packard Enterprise Development LP. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This test requires CXI hardware and multiple processes to run.
# It skips gracefully (exit 77) if run as a singleton during make check.
if HWPC_CXI_ENABLE
    noinst_PROGRAMS = hwpc_cxi_sendrecv_test

    TESTS = hwpc_cxi_sendrecv_test

    hwpc_cxi_sendrecv_test_SOURCES = hwpc_cxi_sendrecv_test.c
    hwpc_cxi_sendrecv_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_sendrecv_test_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

    # Distribute test scripts and documentation
    EXTRA_DIST = run_hwpc_cxi_validate.sh README.md HWPC_CXI_GUIDE.md
endif # HWPC_CXI_ENABLE

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo hwpc_cxi_sendrecv_test prof *.log *.o *.trs Makefile
