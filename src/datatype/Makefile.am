# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libdatatype.la

# Source code files

headers = \
        datatype.h

libdatatype_la_SOURCES = \
        $(headers) \
	datatype.c \
	datatype_copy.c \
	datatype_crc32.c \
	datatype_create.c \
	datatype_delete.c \
	datatype_memcpy.c \
	datatype_pack.c \
	datatype_sum32.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/datatype
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
