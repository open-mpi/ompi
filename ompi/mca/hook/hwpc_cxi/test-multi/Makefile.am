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

# This hardware-dependent suite is built and run manually, outside "make check".
EXTRA_DIST = \
    build_with_env_and_run.sh \
    run_hwpc_cxi_validate.sh \
    hwpc_cxi_compare.sh \
    README.md \
    HWPC_CXI_GUIDE.md \
    my_desired_cxi_counters.txt \
    my_desired_fake_cxi_counters.txt

if HWPC_CXI_ENABLE
    EXTRA_PROGRAMS = hwpc_cxi_sendrecv_test

    hwpc_cxi_sendrecv_test_SOURCES = hwpc_cxi_sendrecv_test.c
    hwpc_cxi_sendrecv_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_sendrecv_test_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/ompi/libopen_mpi.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

endif # HWPC_CXI_ENABLE

# Automake does not clean EXTRA_PROGRAMS.
CLEANFILES = hwpc_cxi_sendrecv_test$(EXEEXT)
