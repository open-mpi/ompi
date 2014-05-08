#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved
# Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS     = -DOSHMEM_PROFILING=1

# This is guaranteed to be false if we're not building OSHMEM at all
if OSHMEM_BUILD_FORTRAN_BINDINGS
pshmem_fortran_lib       = liboshmem_fortran_pshmem.la
else
pshmem_fortran_lib       =
endif

noinst_LTLIBRARIES = $(pshmem_fortran_lib)

headers = prototypes_pshmem.h \
	pbindings.h \
	defines.h

nodist_liboshmem_fortran_pshmem_la_SOURCES = \
	pstart_pes_f.c \
	pnum_pes_f.c \
	pmy_pe_f.c \
	pshmem_barrier_all_f.c \
	pshpalloc_f.c \
	pshpdeallc_f.c \
	pshpclmove_f.c \
	pshmem_ptr_f.c \
	pshmem_pe_accessible_f.c \
	pshmem_addr_accessible_f.c \
	pshmem_put_f.c \
	pshmem_character_put_f.c \
	pshmem_double_put_f.c \
	pshmem_complex_put_f.c \
	pshmem_logical_put_f.c \
	pshmem_integer_put_f.c \
	pshmem_real_put_f.c \
	pshmem_put4_f.c \
	pshmem_put8_f.c \
	pshmem_put32_f.c \
	pshmem_put64_f.c \
	pshmem_put128_f.c \
	pshmem_putmem_f.c \
	pshmem_complex_iput_f.c \
	pshmem_double_iput_f.c \
	pshmem_integer_iput_f.c \
	pshmem_iput128_f.c \
	pshmem_iput32_f.c \
	pshmem_iput4_f.c \
	pshmem_iput64_f.c \
	pshmem_iput8_f.c \
	pshmem_logical_iput_f.c \
	pshmem_real_iput_f.c \
	pshmem_character_get_f.c \
	pshmem_complex_get_f.c \
	pshmem_double_get_f.c \
	pshmem_get128_f.c \
	pshmem_get32_f.c \
	pshmem_get4_f.c \
	pshmem_get64_f.c \
	pshmem_get8_f.c \
	pshmem_getmem_f.c \
	pshmem_integer_get_f.c \
	pshmem_logical_get_f.c \
	pshmem_real_get_f.c \
	pshmem_complex_iget_f.c \
	pshmem_double_iget_f.c \
	pshmem_iget128_f.c \
	pshmem_iget32_f.c \
	pshmem_iget4_f.c \
	pshmem_iget64_f.c \
	pshmem_iget8_f.c \
	pshmem_integer_iget_f.c \
	pshmem_logical_iget_f.c \
	pshmem_real_iget_f.c \
	pshmem_swap_f.c \
	pshmem_int4_swap_f.c \
	pshmem_int8_swap_f.c \
	pshmem_real4_swap_f.c \
	pshmem_real8_swap_f.c \
	pshmem_int4_cswap_f.c \
	pshmem_int8_cswap_f.c \
	pshmem_int4_fadd_f.c \
	pshmem_int8_fadd_f.c \
	pshmem_int4_finc_f.c \
	pshmem_int8_finc_f.c \
	pshmem_int4_add_f.c \
	pshmem_int8_add_f.c \
	pshmem_int4_wait_f.c \
	pshmem_int8_wait_f.c \
	pshmem_wait_f.c \
	pshmem_int4_wait_until_f.c \
	pshmem_int8_wait_until_f.c \
	pshmem_wait_until_f.c \
	pshmem_barrier_f.c \
	pshmem_and_to_all_f.c \
	pshmem_or_to_all_f.c \
	pshmem_xor_to_all_f.c \
	pshmem_max_to_all_f.c \
	pshmem_min_to_all_f.c \
	pshmem_sum_to_all_f.c \
	pshmem_prod_to_all_f.c \
	pshmem_collect_f.c \
	pshmem_broadcast_f.c \
	pshmem_lock_f.c \
	pshmem_cache_f.c \
	pshmem_int4_inc_f.c \
	pshmem_int8_inc_f.c \
	pshmem_quiet_f.c \
	pshmem_fence_f.c

#
# Sym link in the sources from the real OSHMEM directory
#
$(nodist_liboshmem_fortran_pshmem_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		$(LN_S) $(top_srcdir)/oshmem/shmem/fortran/$$pname $@ ; \
	fi

if PROJECT_OSHMEM
if WANT_INSTALL_HEADERS
oshmemdir = $(ompiincludedir)/$(subdir)
oshmem_HEADERS = $(headers)
endif
endif

# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_liboshmem_fortran_pshmem_la_SOURCES)

# Don't want these targets in here

tags-recursive:
tags:
TAGS:
GTAGS:
ID:
