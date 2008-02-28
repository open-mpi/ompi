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

dist_pkgdata_DATA += util/hostfile/help-hostfile.txt \
	util/dash_host/help-dash-host.txt

AM_LFLAGS = -Porte_util_hostfile_
LEX_OUTPUT_ROOT = lex.orte_util_hostfile_

headers += \
	util/context_fns.h \
	util/name_fns.h \
	util/pre_condition_transports.h \
        util/proc_info.h \
        util/session_dir.h \
        util/sys_info.h \
        util/hnp_contact.h \
        util/hostfile/hostfile.h \
        util/hostfile/hostfile_lex.h \
        util/dash_host/dash_host.h \
        util/comm/comm.h

libopen_rte_la_SOURCES += \
        util/error_strings.c \
	util/context_fns.c \
        util/name_fns.c \
	util/pre_condition_transports.c \
        util/proc_info.c \
        util/session_dir.c \
        util/sys_info.c \
        util/hnp_contact.c \
        util/hostfile/hostfile_lex.l \
        util/hostfile/hostfile.c \
        util/dash_host/dash_host.c \
        util/comm/comm.c

