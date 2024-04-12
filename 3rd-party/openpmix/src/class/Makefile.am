# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2007 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
# Copyright (c) 2016      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = \
    $(LTDLINCL)

noinst_LTLIBRARIES = libpmix_class.la

# Source code files
headers = \
        pmix_bitmap.h \
        pmix_object.h \
        pmix_list.h \
        pmix_pointer_array.h \
        pmix_hash_table.h \
        pmix_hotel.h \
        pmix_ring_buffer.h \
        pmix_value_array.h

sources = \
        pmix_bitmap.c \
        pmix_object.c \
        pmix_list.c \
        pmix_pointer_array.c \
        pmix_hash_table.c \
        pmix_hotel.c \
        pmix_ring_buffer.c \
        pmix_value_array.c

libpmix_class_la_SOURCES = $(headers) $(sources)

# Unconditionally install the header files

pmixdir = $(pmixincludedir)/$(subdir)
pmix_HEADERS = $(headers)
