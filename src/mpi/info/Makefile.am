# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# libraries
noinst_LTLIBRARIES = libmpi_info.la
# Source code files

headers = \
        info.h

libmpi_info_la_SOURCES = \
        $(headers) \
        info.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/info
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
