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
# Copyright (c) 2007-2022 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2013      Intel, Inc. All rights reserved
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
# Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2021      Google, LLC. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

SUBDIRS = keyval

dist_opaldata_DATA = help-opal-util.txt

AM_LFLAGS = -Popal_show_help_yy
LEX_OUTPUT_ROOT = lex.opal_show_help_yy

noinst_LTLIBRARIES = libopalutil.la libopalutil_core.la

AM_CPPFLAGS = $(LTDLINCL)

# Source code files

headers = \
        alfg.h \
        arch.h \
        argv.h \
        basename.h \
	bipartite_graph.h \
	bipartite_graph_internal.h \
        bit_ops.h \
        clock_gettime.h \
        cmd_line.h \
        crc.h \
	ethtool.h \
        error.h \
	event.h \
        fd.h \
        few.h \
        if.h \
        keyval_parse.h \
        malloc.h \
        net.h \
        numtostr.h \
        opal_environ.h \
        opal_getcwd.h \
        os_dirpath.h \
        os_path.h \
        output.h \
        path.h \
        printf.h \
        proc.h \
        qsort.h \
        show_help.h \
        show_help_lex.h \
        stacktrace.h \
        string_copy.h \
        sys_limits.h \
        timings.h \
        uri.h \
        info_subscriber.h \
	info.h \
	minmax.h \
	sha256.h

libopalutil_la_SOURCES = \
        $(headers) \
	ethtool.c \
	event.c \
        if.c \
        net.c \
        info_subscriber.c \
        info.c

libopalutil_core_la_SOURCES = \
        alfg.c \
        arch.c \
        argv.c \
        basename.c \
	bipartite_graph.c \
        cmd_line.c \
        crc.c \
        error.c \
        fd.c \
        few.c \
        keyval_parse.c \
        malloc.c \
        numtostr.c \
        opal_environ.c \
        opal_getcwd.c \
        os_dirpath.c \
        os_path.c \
        output.c \
        path.c \
        printf.c \
        proc.c \
        qsort.c \
        sha256.c \
        show_help.c \
        show_help_lex.l \
        stacktrace.c \
        string_copy.c \
        sys_limits.c \
        uri.c

if OPAL_COMPILE_TIMING
libopalutil_core_la_SOURCES += timings.c
endif

libopalutil_core_la_LIBADD = \
        keyval/libopalutilkeyval.la
libopalutil_core_la_DEPENDENCIES = \
        keyval/libopalutilkeyval.la

# Conditionally install the header files

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
opal_HEADERS = $(headers)
endif

maintainer-clean-local:
	rm -f show_help_lex.c
