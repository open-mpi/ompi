# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmpi_request.la

# Source code files

headers = \
        request.h

libmpi_request_la_SOURCES = \
        $(headers) \
	request.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/request
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
