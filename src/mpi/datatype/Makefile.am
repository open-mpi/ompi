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
	datatype_copy.c \
	datatype_crc32.c \
	datatype_create.c \
	datatype_delete.c \
	datatype_memcpy.c \
	datatype_pack.c \
	datatype_sum32.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/datatype
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
