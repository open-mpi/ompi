# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from orte/Makefile.am

dist_pkgdata_DATA += runtime/help-orte-runtime.txt

headers += \
	runtime/orte_wait.h \
        runtime/runtime.h \
        runtime/runtime_internal.h \
        runtime/runtime_types.h \
        runtime/params.h \
	runtime/orte_setup_hnp.h

libopen_rte_la_SOURCES += \
        runtime/orte_abort.c \
        runtime/orte_finalize.c \
        runtime/orte_init.c \
        runtime/orte_params.c \
        runtime/orte_init_stage1.c \
        runtime/orte_init_stage2.c \
        runtime/orte_monitor.c \
        runtime/orte_restart.c \
        runtime/orte_setup_hnp.c \
        runtime/orte_system_finalize.c \
        runtime/orte_system_init.c \
        runtime/orte_universe_exists.c \
        runtime/orte_wait.c
