# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libutil.la

# Source code files

headers = \
    argv.h \
    cmd_line.h \
    few.h \
    hibit.h \
    if.h \
    output.h \
    path.h \
    sys_info.h \
    os_path.h \
    os_create_dirpath.h \
    strncpy.h

libutil_la_SOURCES = \
    $(headers) \
    argv.c \
    cmd_line.c \
    few.c \
    if.c \
    output.c \
    path.c \
    sys_info.c \
    os_path.c \
    os_create_dirpath.c \
    strncpy.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/util
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
