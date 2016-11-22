# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_oshmemdata_DATA += base/help-oshmem-memheap.txt

AM_CPPFLAGS = -DOMPI_LIBMPI_NAME=\"$(OMPI_LIBMPI_NAME)\"

headers += \
        base/base.h

libmca_memheap_la_SOURCES += \
	base/memheap_base_frame.c \
	base/memheap_base_select.c \
	base/memheap_base_alloc.c \
	base/memheap_base_static.c \
	base/memheap_base_register.c \
	base/memheap_base_mkey.c
