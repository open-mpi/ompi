#
# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


AM_CFLAGS = $(OSHMEM_CFLAGS)

#
# OMPI_PRPOFILING_DEFINES flag s enabled when we want our MPI_* symbols
# to be replaced by PMPI_*. In other words, this flag decides 
# whether "profile/defines.h" is included or not. "profile/defines.h"
# replaces all MPI_* symbols with PMPI_* symbols. In this directory,
# we need it to be 0
#

AM_CPPFLAGS     = -DOSHMEM_PROFILING_DEFINES=0 -DOSHMEM_HAVE_WEAK_SYMBOLS=0

#
# The top directory always builds MPI_* bindings. The bottom directory
# always builds PMPI_* bindings. The cases where the top directory
# needs to be built are:
#
# 1. When profiling is disabled.
# 2. When profiling is enabled but weak symbol support is absent.
#
noinst_LTLIBRARIES = libshmem_f77.la
headers = prototypes_shmem.h \
          bindings.h \
          shmem_fortran_pointer.h

libshmem_f77_la_SOURCES = \
                    start_pes_f.c \
                    num_pes_f.c \
                    my_pe_f.c \
                    shmem_finalize_f.c \
                    shmem_barrier_all_f.c \
                    shpalloc_f.c \
                    shpdeallc_f.c \
                    shpclmove_f.c \
                    shmem_ptr_f.c \
                    shmem_pe_accessible_f.c \
                    shmem_addr_accessible_f.c \
                    shmem_put_f.c \
                    shmem_character_put_f.c \
                    shmem_double_put_f.c \
                    shmem_complex_put_f.c \
                    shmem_logical_put_f.c \
                    shmem_integer_put_f.c \
                    shmem_real_put_f.c \
                    shmem_put4_f.c \
                    shmem_put8_f.c \
                    shmem_put32_f.c \
                    shmem_put64_f.c \
                    shmem_putmem_f.c \
                    shmem_complex_iput_f.c \
                    shmem_double_iput_f.c \
                    shmem_integer_iput_f.c \
                    shmem_iput128_f.c \
                    shmem_iput32_f.c \
                    shmem_iput4_f.c \
                    shmem_iput64_f.c \
                    shmem_iput8_f.c \
                    shmem_logical_iput_f.c \
                    shmem_real_iput_f.c \
                    shmem_character_get_f.c \
                    shmem_complex_get_f.c \
                    shmem_double_get_f.c \
                    shmem_get128_f.c \
                    shmem_get32_f.c \
                    shmem_get4_f.c \
                    shmem_get64_f.c \
                    shmem_get8_f.c \
                    shmem_getmem_f.c \
                    shmem_integer_get_f.c \
                    shmem_logical_get_f.c \
                    shmem_real_get_f.c \
                    shmem_complex_iget_f.c \
                    shmem_double_iget_f.c \
                    shmem_iget128_f.c \
                    shmem_iget32_f.c \
                    shmem_iget4_f.c \
                    shmem_iget64_f.c \
                    shmem_iget8_f.c \
                    shmem_integer_iget_f.c \
                    shmem_logical_iget_f.c \
                    shmem_real_iget_f.c \
                    shmem_swap_f.c \
                    shmem_int4_swap_f.c \
                    shmem_int8_swap_f.c \
                    shmem_real4_swap_f.c \
                    shmem_real8_swap_f.c \
                    shmem_int4_cswap_f.c \
                    shmem_int8_cswap_f.c \
                    shmem_int4_fadd_f.c \
                    shmem_int8_fadd_f.c \
                    shmem_int4_finc_f.c \
                    shmem_int8_finc_f.c \
                    shmem_int4_add_f.c \
                    shmem_int8_add_f.c \
                    shmem_int4_wait_f.c \
                    shmem_int8_wait_f.c \
                    shmem_wait_f.c \
                    shmem_int4_wait_until_f.c \
                    shmem_int8_wait_until_f.c \
                    shmem_wait_until_f.c \
                    shmem_barrier_f.c \
                    shmem_and_to_all_f.c \
                    shmem_or_to_all_f.c \
                    shmem_xor_to_all_f.c \
                    shmem_max_to_all_f.c \
                    shmem_min_to_all_f.c \
                    shmem_sum_to_all_f.c \
                    shmem_prod_to_all_f.c \
                    shmem_collect_f.c \
                    shmem_broadcast_f.c \
                    shmem_lock_f.c \
                    shmem_cache_f.c \
                    shmem_int4_inc_f.c \
                    shmem_int8_inc_f.c \
                    shmem_quiet_f.c \
                    shmem_fence_f.c

if WANT_INSTALL_HEADERS
oshmemdir = $(includedir)/openshmem/oshmem/shmem/f77
oshmem_HEADERS = $(headers)
else
oshmemdir = $(includedir)
endif

