#
# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#
# OSHMEM_PROFILING flag is enabled when we want our shmem_* symbols
# to be replaced by pshmem_*. In other words, this flag decides 
# whether "profile/defines.h" is included or not. "profile/defines.h"
# replaces all shmem_* symbols with pshmem_* symbols. In this directory
# we definately need it to be 1.
#
AM_CPPFLAGS = -DOSHMEM_PROFILING=1


noinst_LTLIBRARIES = libshmem_c_pshmem.la

headers = defines.h


SHMEM_API_SOURCES = \
	shmem_init.c \
	shmem_free.c \
	shmem_alloc.c \
	shmem_realloc.c \
	shmem_align.c \
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
	shmem_test_lock.c

#nodist_libshmem_c_pshmem_la_SOURCES = $(addprefix p, $(SHMEM_API_SOURCES))
nodist_libshmem_c_pshmem_la_SOURCES = \
	$(SHMEM_API_SOURCES)


#
# Sym link in the sources from the real MPI directory
#
$(nodist_libshmem_c_pshmem_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '1-'` ; \
		$(LN_S) $(top_srcdir)/oshmem/shmem/c/$$pname $@ ; \
	fi

if WANT_INSTALL_HEADERS
oshmemdir = $(includedir)/openshmem/oshmem/shmem/c/profile
oshmem_HEADERS = $(headers)
else
oshmemdir = $(includedir)
endif

# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libshmem_c_pshmem_la_SOURCES)

# Don't want these targets in here

tags-recursive:
tags:
TAGS:
GTAGS:
ID:
