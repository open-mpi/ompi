# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_builddir)/src/lam/event

noinst_LTLIBRARIES = libruntime.la

# Source code files

headers = \
	runtime.h

libruntime_la_SOURCES = \
	$(headers) \
        lam_abort.c \
        lam_finalize.c \
        lam_init.c \
        lam_rte_finalize.c \
        lam_rte_init.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/lam/runtime
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
