# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libutil.la

# Source code files

headers = \
    argv.h \
    few.h \
    lam_log.h \
    malloc.h \
    path.h \
    reactor.h \
    strncpy.h

libutil_la_SOURCES = \
    $(headers) \
    argv.c \
    few.c \
    lam_log.c \
    path.c \
    reactor.c \
    strncpy.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/lam/util
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
