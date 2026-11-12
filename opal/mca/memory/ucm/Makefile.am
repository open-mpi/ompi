#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#

AM_CPPFLAGS = $(memory_ucm_CPPFLAGS)

# This component is only ever built statically (i.e., slurped into
# libopen-pal) -- it is never built as a DSO. It does not link against
# the UCX libraries: it only uses them if they are already loaded.
noinst_LTLIBRARIES = libmca_memory_ucm.la
libmca_memory_ucm_la_SOURCES = \
    memory_ucm.h \
    memory_ucm_component.c
libmca_memory_ucm_la_LDFLAGS = -module -avoid-version
