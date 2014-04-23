/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include "oshmem/shmem/fortran/bindings.h"
#include "oshmem/include/shmem.h"
#include "oshmem/shmem/shmem_api_logger.h"
#include "ompi/datatype/ompi_datatype.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_SET_CACHE_INV, shmem_set_cache_inv)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_SET_CACHE_LINE_INV, shmem_set_cache_line_inv)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_CLEAR_CACHE_INV, shmem_clear_cache_inv)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_CLEAR_CACHE_LINE_INV, shmem_clear_cache_line_inv)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_UDCFLUSH, shmem_udcflush)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_UDCFLUSH_LINE, shmem_udcflush_line)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_SET_CACHE_INV,
        shmem_set_cache_inv_,
        shmem_set_cache_inv__,
        shmem_set_cache_inv_f,
        (void), 
        ())

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_SET_CACHE_LINE_INV,
        shmem_set_cache_line_inv_,
        shmem_set_cache_line_inv__,
        shmem_set_cache_line_inv_f,
        (FORTRAN_POINTER_T target), 
        (target))

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_CLEAR_CACHE_INV,
        shmem_clear_cache_inv_,
        shmem_clear_cache_inv__,
        shmem_clear_cache_inv_f,
        (void), 
        ())

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_CLEAR_CACHE_LINE_INV,
        shmem_clear_cache_line_inv_,
        shmem_clear_cache_line_inv__,
        shmem_clear_cache_line_inv_f,
        (FORTRAN_POINTER_T target), 
        (target))

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_UDCFLUSH,
        shmem_udcflush_,
        shmem_udcflush__,
        shmem_udcflush_f,
        (void), 
        ())

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_UDCFLUSH_LINE,
        shmem_udcflush_line_,
        shmem_udcflush_line__,
        shmem_udcflush_line_f,
        (FORTRAN_POINTER_T target), 
        (target))

void shmem_set_cache_inv_f(void)
{
    shmem_set_cache_inv();
}

void shmem_set_cache_line_inv_f(FORTRAN_POINTER_T target)
{
    shmem_set_cache_line_inv(FPTR_2_VOID_PTR(target));
}

void shmem_clear_cache_inv_f(void)
{
    shmem_clear_cache_inv();
}

void shmem_clear_cache_line_inv_f(FORTRAN_POINTER_T target)
{
    shmem_clear_cache_line_inv(FPTR_2_VOID_PTR(target));
}

void shmem_udcflush_f(void)
{
    shmem_udcflush();
}

void shmem_udcflush_line_f(FORTRAN_POINTER_T target)
{
    shmem_udcflush_line(FPTR_2_VOID_PTR(target));
}
