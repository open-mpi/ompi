# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmpi_datatype.la

# Source code files

headers = \
        datatype.h

libmpi_datatype_la_SOURCES = \
        $(headers) \
        datatype.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/datatype
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
