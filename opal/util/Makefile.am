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
# Copyright (c) 2007-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2013      Intel, Inc. All rights reserved
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

noinst_LTLIBRARIES = libopalutil.la

AM_CPPFLAGS = $(LTDLINCL)

# Source code files

headers = \
        alfg.h \
        arch.h \
        argv.h \
        basename.h \
        bit_ops.h \
        cmd_line.h \
        crc.h \
        daemon_init.h \
        error.h \
        few.h \
        fd.h \
        fd.c \
        if.h \
        keyval_parse.h \
        malloc.h \
        net.h \
        numtostr.h \
        opal_environ.h \
        opal_getcwd.h \
        opal_pty.h \
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
        strncpy.h \
        sys_limits.h \
        timings.h \
        uri.h

libopalutil_la_SOURCES = \
        $(headers) \
        alfg.c \
        arch.c \
        argv.c \
        basename.c \
        cmd_line.c \
        crc.c \
        daemon_init.c \
        error.c \
        few.c \
        if.c \
        keyval_parse.c \
        malloc.c \
        net.c \
        numtostr.c \
        opal_environ.c \
        opal_getcwd.c \
        opal_pty.c \
        os_dirpath.c \
        os_path.c \
        output.c \
        path.c \
        printf.c \
        proc.c \
        qsort.c \
        show_help.c \
        show_help_lex.l \
        stacktrace.c \
        strncpy.c \
        sys_limits.c \
        uri.c

if OPAL_COMPILE_TIMING
libopalutil_la_SOURCES += timings.c
endif

libopalutil_la_LIBADD = \
        keyval/libopalutilkeyval.la
libopalutil_la_DEPENDENCIES = \
        keyval/libopalutilkeyval.la

# Conditionally install the header files

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
opal_HEADERS = $(headers)
endif
