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

sources = \
    hook_hwpc_cxi.h \
    hook_hwpc_cxi_component.c \
    hook_hwpc_cxi_constants.h \
    hook_hwpc_cxi_constants.c \
    hook_hwpc_cxi_counters.c

EXTRA_DIST = help-hook-hwpc-cxi.txt

# This component will only ever be built statically -- never as a DSO.

noinst_LTLIBRARIES = libmca_hook_hwpc_cxi.la

libmca_hook_hwpc_cxi_la_SOURCES = $(sources)

# ompi/Makefile.am builds test/ after libmpi and ompi_info; listed here only for make dist.
DIST_SUBDIRS = test
