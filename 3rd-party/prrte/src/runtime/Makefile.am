# -*- makefile -*-
#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from src/Makefile.am

headers += \
        runtime/runtime.h \
        runtime/prte_locks.h \
        runtime/prte_globals.h \
        runtime/prte_quit.h \
        runtime/runtime_internals.h \
        runtime/prte_wait.h \
        runtime/prte_data_server.h \
        runtime/prte_progress_threads.h

libprrte_la_SOURCES += \
        runtime/prte_finalize.c \
        runtime/prte_init.c \
        runtime/prte_locks.c \
        runtime/prte_globals.c \
        runtime/prte_quit.c \
        runtime/data_type_support/prte_dt_copy_fns.c \
        runtime/data_type_support/prte_dt_print_fns.c \
        runtime/data_type_support/prte_dt_packing_fns.c \
        runtime/data_type_support/prte_dt_unpacking_fns.c \
        runtime/prte_mca_params.c \
        runtime/prte_wait.c \
        runtime/prte_data_server.c \
        runtime/prte_progress_threads.c
