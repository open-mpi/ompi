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
    if.h \
    output.h \
    path.h \
    reactor.h \
    strncpy.h

libutil_la_SOURCES = \
    $(headers) \
    argv.c \
    cmd_line.c \
    few.c \
    if.c \
    output.c \
    path.c \
    reactor.c \
    strncpy.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/util
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
