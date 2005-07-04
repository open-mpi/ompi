#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -DOMPI_PKGDATADIR=\"$(pkgdatadir)\"
AM_LFLAGS = -Pompi_show_help_yy
LEX_OUTPUT_ROOT = lex.ompi_show_help_yy

noinst_LTLIBRARIES = libopalutil.la

# Source code files

headers = \
        argv.h \
        basename.h \
        bit_ops.h \
        cmd_line.h \
        convert.h \
        daemon_init.h \
        few.h \
        if.h \
        malloc.h \
        numtostr.h \
        opal_environ.h \
        os_create_dirpath.h \
        os_path.h \
        output.h \
        path.h \
        pow2.h \
        printf.h \
        qsort.h \
        show_help.h \
        show_help_lex.h \
        stacktrace.h \
        strncpy.h \
        sys_info.h

libopalutil_la_SOURCES = \
        $(headers) \
        argv.c \
        basename.c \
        cmd_line.c \
        convert.c \
        daemon_init.c \
        few.c \
        if.c \
        malloc.c \
        numtostr.c \
        opal_environ.c \
        os_create_dirpath.c \
        os_path.c \
        output.c \
        path.c \
        pow2.c \
        printf.c \
        qsort.c \
        show_help.c \
        show_help_lex.l \
        stacktrace.c \
        strncpy.c \
        sys_info.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/opal/util
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
