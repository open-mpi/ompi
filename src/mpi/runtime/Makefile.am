# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmpi_runtime.la

# Source code files

headers = \
        runtime.h

libmpi_runtime_la_SOURCES = \
        $(headers) \
        lam_mpi_init.c \
        lam_mpi_finalize.c \
        lam_mpi_globals.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/runtime
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
