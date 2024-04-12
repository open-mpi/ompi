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
# Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

SUBDIRS = hostfile

noinst_LTLIBRARIES = libprrteutil.la

AM_CPPFLAGS = $(LTDLINCL)

# Source code files

headers = \
        attr.h \
        bipartite_graph.h \
        bipartite_graph_internal.h \
        bit_ops.h \
        prte_cmd_line.h \
        crc.h \
        daemon_init.h \
        dash_host/dash_host.h \
        error_strings.h \
        ethtool.h \
        error.h \
        malloc.h \
        name_fns.h \
        nidmap.h \
        numtostr.h \
        proc_info.h \
        session_dir.h \
        stacktrace.h \
        sys_limits.h \
        uri.h

libprrteutil_la_SOURCES = \
        $(headers) \
        attr.c \
        bipartite_graph.c \
        crc.c \
        daemon_init.c \
        dash_host/dash_host.c \
        error_strings.c \
        ethtool.c \
        error.c \
        malloc.c \
        name_fns.c \
        nidmap.c \
        numtostr.c \
        proc_info.c \
        session_dir.c \
        stacktrace.c \
        sys_limits.c \
        uri.c

libprrteutil_la_LIBADD = \
        hostfile/libprrteutilhostfile.la
libprrteutil_la_DEPENDENCIES = \
        hostfile/libprrteutilhostfile.la

# Conditionally install the header files

if WANT_INSTALL_HEADERS
prtedir = $(prteincludedir)/$(subdir)
prte_HEADERS = $(headers)
endif
