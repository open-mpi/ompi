#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved
# Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


if OSHMEM_PROFILING
    SUBDIRS = profile
endif

OSHMEM_API_SOURCES = \
	shmem_init.c \
	shmem_free.c \
	shmem_alloc.c \
	shmem_realloc.c \
	shmem_align.c \
	shmem_finalize.c \
	shmem_query.c \
	shmem_p.c \
	shmem_put.c \
	shmem_g.c \
	shmem_get.c \
	shmem_broadcast.c \
	shmem_collect.c \
	shmem_ptr.c \
	shmem_pe_accessible.c \
	shmem_addr_accessible.c \
	shmem_barrier.c \
	shmem_fence.c \
	shmem_quiet.c \
	shmem_wait.c \
	shmem_iget.c \
	shmem_iput.c \
	shmem_udcflush.c \
	shmem_udcflush_line.c \
	shmem_set_cache_inv.c \
	shmem_set_cache_line_inv.c \
	shmem_clear_cache_inv.c \
	shmem_clear_cache_line_inv.c \
	shmem_reduce.c \
	shmem_swap.c \
	shmem_cswap.c \
	shmem_fadd.c \
	shmem_finc.c \
	shmem_add.c \
	shmem_inc.c \
	shmem_clear_lock.c \
	shmem_set_lock.c \
	shmem_test_lock.c \
	shmem_lock.c \
	globalexit.c

AM_CPPFLAGS     = -DOSHMEM_PROFILING=0

noinst_LTLIBRARIES =
if PROJECT_OSHMEM
# Only build if we're building OSHMEM
noinst_LTLIBRARIES += liboshmem_c.la
endif

headers = 

liboshmem_c_la_SOURCES = $(OSHMEM_API_SOURCES)

if PROJECT_OSHMEM
if WANT_INSTALL_HEADERS
oshmemdir = $(ompiincludedir)/$(subdir)
oshmem_HEADERS = $(headers)
endif
endif
