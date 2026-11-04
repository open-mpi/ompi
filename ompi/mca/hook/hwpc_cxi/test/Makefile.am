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

# Unit tests for the hwpc_cxi hook module, run by make check.
# The MPI_Init() tests skip (exit 77) on nodes without a Cassini device.
include $(top_srcdir)/Makefile.mca-dso-check

EXTRA_DIST = check_hwpc_cxi_dynamic_symbols.sh check_hwpc_cxi_mca_params.sh

if HWPC_CXI_ENABLE
    check_PROGRAMS = \
        hwpc_cxi_counters_test \
        hwpc_cxi_constants_lookup_test \
        hwpc_cxi_constants_validation_test \
        hwpc_cxi_mpi_init_finalize_level_0_test \
        hwpc_cxi_mpi_init_finalize_level_1_test \
        hwpc_cxi_mpi_init_finalize_level_2_test \
        hwpc_cxi_mpi_init_finalize_level_5_test \
        hwpc_cxi_mpi_t_pvars_test \
        hwpc_cxi_mpi_t_pvars_enabled_test

    TESTS = \
        hwpc_cxi_counters_test \
        hwpc_cxi_constants_lookup_test \
        hwpc_cxi_constants_validation_test \
        hwpc_cxi_mpi_init_finalize_level_0_test \
        hwpc_cxi_mpi_init_finalize_level_1_test \
        hwpc_cxi_mpi_init_finalize_level_2_test \
        hwpc_cxi_mpi_init_finalize_level_5_test \
        hwpc_cxi_mpi_t_pvars_test \
        hwpc_cxi_mpi_t_pvars_enabled_test

check-local: check-hwpc-cxi-dynamic-symbols check-hwpc-cxi-mca-params
.PHONY: check-hwpc-cxi-dynamic-symbols check-hwpc-cxi-mca-params
check-hwpc-cxi-dynamic-symbols:
	$(AM_V_at)$(SHELL) $(srcdir)/check_hwpc_cxi_dynamic_symbols.sh "$(abs_top_builddir)/ompi/.libs" "$(abs_top_builddir)/opal/include/opal_config.h" $(NM)
check-hwpc-cxi-mca-params:
	$(AM_V_at)$(SHELL) $(srcdir)/check_hwpc_cxi_mca_params.sh "$(abs_top_builddir)/ompi/tools/ompi_info/ompi_info$(EXEEXT)"
    AM_CPPFLAGS = -I$(top_srcdir)/ompi/mca/hook/hwpc_cxi

    hwpc_cxi_counters_test_SOURCES = hwpc_cxi_counters_test.c ../hook_hwpc_cxi_constants.c
    hwpc_cxi_counters_test_CPPFLAGS = $(AM_CPPFLAGS)
    hwpc_cxi_counters_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_counters_test_LDADD = \
        $(top_builddir)/ompi/libopen_mpi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

    ###############################################################################################
    # Unit tests just for sanity-checking API to the static constants portion of the hook module  #
    ###############################################################################################

    hwpc_cxi_constants_lookup_test_SOURCES = \
        hwpc_cxi_constants_lookup_test.c \
        ../hook_hwpc_cxi_constants.c

    hwpc_cxi_constants_validation_test_SOURCES = \
        hwpc_cxi_constants_validation_test.c \
        ../hook_hwpc_cxi_constants.c

    hwpc_cxi_constants_lookup_test_CPPFLAGS = $(AM_CPPFLAGS)
    hwpc_cxi_constants_validation_test_CPPFLAGS = $(AM_CPPFLAGS)
    hwpc_cxi_constants_lookup_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_constants_validation_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)

    #########################################################################################################################
    # Integration tests for running the hook module through MPI/PMIX for the singleton process case: (world_num_ranks==1)   #
    #########################################################################################################################

    hwpc_cxi_mpi_init_finalize_level_0_test_SOURCES = hwpc_cxi_mpi_init_finalize_level_0_test.c
    hwpc_cxi_mpi_init_finalize_level_1_test_SOURCES = hwpc_cxi_mpi_init_finalize_level_1_test.c
    hwpc_cxi_mpi_init_finalize_level_2_test_SOURCES = hwpc_cxi_mpi_init_finalize_level_2_test.c
    hwpc_cxi_mpi_init_finalize_level_5_test_SOURCES = hwpc_cxi_mpi_init_finalize_level_5_test.c
    hwpc_cxi_mpi_t_pvars_test_SOURCES = hwpc_cxi_mpi_t_pvars_test.c
    hwpc_cxi_mpi_t_pvars_enabled_test_SOURCES = hwpc_cxi_mpi_t_pvars_test.c
    hwpc_cxi_mpi_t_pvars_enabled_test_CPPFLAGS = $(AM_CPPFLAGS) -DHWPC_CXI_MPI_T_PVARS_ENABLED_TEST=1

    hwpc_cxi_mpi_init_finalize_level_0_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_mpi_init_finalize_level_1_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_mpi_init_finalize_level_2_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_mpi_init_finalize_level_5_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_mpi_t_pvars_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    hwpc_cxi_mpi_t_pvars_enabled_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)

    hwpc_cxi_mpi_init_finalize_level_0_test_LDADD = \
        $(top_builddir)/ompi/libopen_mpi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    hwpc_cxi_mpi_init_finalize_level_1_test_LDADD = \
        $(top_builddir)/ompi/libopen_mpi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    hwpc_cxi_mpi_init_finalize_level_2_test_LDADD = \
        $(top_builddir)/ompi/libopen_mpi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    hwpc_cxi_mpi_init_finalize_level_5_test_LDADD = \
        $(top_builddir)/ompi/libopen_mpi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    hwpc_cxi_mpi_t_pvars_test_LDADD = \
        $(top_builddir)/ompi/libopen_mpi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    hwpc_cxi_mpi_t_pvars_enabled_test_LDADD = \
        $(top_builddir)/ompi/libopen_mpi.la \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

endif # HWPC_CXI_ENABLE
