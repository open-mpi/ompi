# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libdatatype.la

# Source code files

headers = datatype.h datatype_internal.h

libdatatype_la_SOURCES = \
        $(headers) \
	dt_add.c dt_create.c dt_create_array.c dt_create_dup.c dt_create_indexed.c \
	dt_create_struct.c dt_create_vector.c dt_destroy.c dt_module.c dt_old_limits.c \
	dt_optimize.c dt_pack.c dt_unpack.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/datatype
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
