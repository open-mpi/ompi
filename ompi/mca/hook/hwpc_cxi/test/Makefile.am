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

# Unit tests for the hwpc_cxi hook module
# These tests are hardware-independent and should run during make check.
if HWPC_CXI_ENABLE
    check_PROGRAMS = \
        hwpc_cxi_constants_lookup_test \
        hwpc_cxi_constants_validation_test \
        hwpc_cxi_report_level_0_test \
        hwpc_cxi_mpi_init_test \
        hwpc_cxi_mpi_init_finalize_level_0_test \
        hwpc_cxi_mpi_init_finalize_level_1_test \
        hwpc_cxi_mpi_init_finalize_level_2_test \
        hwpc_cxi_mpi_init_finalize_level_5_test \
        hwpc_cxi_mpi_t_pvars_test \
        hwpc_cxi_mpi_t_pvars_enabled_test

    TESTS = $(check_PROGRAMS)

    AM_CPPFLAGS = -I$(top_srcdir)/ompi/mca/hook/hwpc_cxi

    ###############################################################################################
    # Unit tests just for sanity-checking API to the static constants portion of the hook module  #
    ###############################################################################################

    hwpc_cxi_constants_lookup_test_SOURCES = \
        hwpc_cxi_constants_lookup_test.c \
        ../hook_hwpc_cxi_constants.c

    hwpc_cxi_constants_validation_test_SOURCES = \
        hwpc_cxi_constants_validation_test.c \
        ../hook_hwpc_cxi_constants.c

    hwpc_cxi_constants_lookup_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_constants_validation_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)

    ########################################################################################
    # Unit tests for the entire hook module using the API externally exposed to MPI        #
    ########################################################################################

    hwpc_cxi_report_level_0_test_SOURCES = hwpc_cxi_report_level_0_test.c

    hwpc_cxi_report_level_0_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)

    hwpc_cxi_report_level_0_test_LDADD = \
        $(top_builddir)/ompi/mca/hook/hwpc_cxi/libmca_hook_hwpc_cxi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

    #########################################################################################################################
    # Integration tests for running the hook module through MPI/PMIX for the singleton process case: (world_num_ranks==1)   #
    #########################################################################################################################

    hwpc_cxi_mpi_init_test_SOURCES = hwpc_cxi_mpi_init_test.c
    hwpc_cxi_mpi_init_finalize_level_0_test_SOURCES = hwpc_cxi_mpi_init_finalize_level_0_test.c
    hwpc_cxi_mpi_init_finalize_level_1_test_SOURCES = hwpc_cxi_mpi_init_finalize_level_1_test.c
    hwpc_cxi_mpi_init_finalize_level_2_test_SOURCES = hwpc_cxi_mpi_init_finalize_level_2_test.c
    hwpc_cxi_mpi_init_finalize_level_5_test_SOURCES = hwpc_cxi_mpi_init_finalize_level_5_test.c
    hwpc_cxi_mpi_t_pvars_test_SOURCES = hwpc_cxi_mpi_t_pvars_test.c
    hwpc_cxi_mpi_t_pvars_enabled_test_SOURCES = hwpc_cxi_mpi_t_pvars_test.c
    hwpc_cxi_mpi_t_pvars_enabled_test_CPPFLAGS = $(AM_CPPFLAGS) -DHWPC_CXI_MPI_T_PVARS_ENABLED_TEST=1

    hwpc_cxi_mpi_init_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_mpi_init_finalize_level_0_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_mpi_init_finalize_level_1_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_mpi_init_finalize_level_2_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_mpi_init_finalize_level_5_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_mpi_t_pvars_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_mpi_t_pvars_enabled_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)

    hwpc_cxi_mpi_init_test_LDADD = \
        $(top_builddir)/ompi/mca/hook/hwpc_cxi/libmca_hook_hwpc_cxi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    hwpc_cxi_mpi_init_finalize_level_0_test_LDADD = \
        $(top_builddir)/ompi/mca/hook/hwpc_cxi/libmca_hook_hwpc_cxi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    hwpc_cxi_mpi_init_finalize_level_1_test_LDADD = \
        $(top_builddir)/ompi/mca/hook/hwpc_cxi/libmca_hook_hwpc_cxi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    hwpc_cxi_mpi_init_finalize_level_2_test_LDADD = \
        $(top_builddir)/ompi/mca/hook/hwpc_cxi/libmca_hook_hwpc_cxi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    hwpc_cxi_mpi_init_finalize_level_5_test_LDADD = \
        $(top_builddir)/ompi/mca/hook/hwpc_cxi/libmca_hook_hwpc_cxi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    hwpc_cxi_mpi_t_pvars_test_LDADD = \
        $(top_builddir)/ompi/mca/hook/hwpc_cxi/libmca_hook_hwpc_cxi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    hwpc_cxi_mpi_t_pvars_enabled_test_LDADD = \
        $(top_builddir)/ompi/mca/hook/hwpc_cxi/libmca_hook_hwpc_cxi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la


    # Distribute test scripts and documentation
    EXTRA_DIST = run_hwpc_cxi_validate.sh README.md HWPC_CXI_GUIDE.md
endif # HWPC_CXI_ENABLE

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo hwpc_cxi_*_test prof *.log *.o *.trs Makefile
