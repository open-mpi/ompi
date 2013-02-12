# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CFLAGS = $(OSHMEM_CFLAGS)
AM_CPPFLAGS = $(openib_CPPFLAGS)

dist_pkgdata_DATA += base/help-shmem-mca.txt 

headers += \
        base/base.h 

libmca_memheap_la_SOURCES += \
	base/memheap_base_open.c \
	base/memheap_base_select.c \
	base/memheap_base_alloc.c \
	base/memheap_base_static.c \
	base/memheap_base_register.c \
	base/memheap_base_mkey.c \
	base/memheap_base_close.c

libmca_memheap_la_LDFLAGS += -module -avoid-version $(openib_LDFLAGS) 
libmca_memheap_la_LIBADD  += $(openib_LIBS)
