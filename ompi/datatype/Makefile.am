# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

headers = \
	datatype.h \
	datatype_internal.h \
	datatype_prototypes.h \
	datatype_pack.h \
	datatype_unpack.h \
	datatype_checksum.h \
	datatype_memcpy.h \
	dt_arch.h \
	convertor.h \
    convertor_internal.h

noinst_LTLIBRARIES = \
        libdatatype_reliable.la \
        libdatatype.la

# these sources will be compiled with the special -D
libdatatype_reliable_la_SOURCES = datatype_pack.c datatype_unpack.c
libdatatype_reliable_la_CFLAGS = -DCHECKSUM $(AM_CFLAGS)

# these sources will be compiled with the normal CFLAGS only
libdatatype_la_SOURCES = \
	dt_add.c \
	dt_create.c \
	dt_create_dup.c \
	dt_create_indexed.c \
	dt_create_struct.c \
	dt_create_vector.c \
	dt_destroy.c \
	dt_module.c \
	dt_optimize.c \
	datatype_pack.c datatype_unpack.c \
	dt_sndrcv.c \
	fake_stack.c \
	dt_args.c \
	dt_arch.c \
	dt_copy.c \
	dt_external32.c \
	dt_match_size.c \
	convertor.c position.c \
	copy_functions.c \
	copy_functions_heterogeneous.c \
	dt_get_count.c

libdatatype_la_LIBADD = libdatatype_reliable.la

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/datatype
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
