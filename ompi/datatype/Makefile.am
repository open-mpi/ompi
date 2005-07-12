# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libdatatype.la

# Source code files

headers = datatype.h datatype_internal.h dt_arch.h convertor.h

libdatatype_la_SOURCES = $(headers) \
	dt_add.c dt_create.c dt_create_array.c dt_create_dup.c dt_create_indexed.c \
	dt_create_struct.c dt_create_vector.c dt_destroy.c dt_module.c \
	dt_optimize.c dt_pack.c dt_sndrcv.c dt_unpack.c fake_stack.c dt_args.c \
	dt_arch.c dt_external32.c dt_match_size.c convertor.c new_pack.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/datatype
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
