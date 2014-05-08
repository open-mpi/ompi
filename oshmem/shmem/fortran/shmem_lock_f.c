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
#include "oshmem/shmem/shmem_lock.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_SET_LOCK, shmem_set_lock)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_CLEAR_LOCK, shmem_clear_lock)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_TEST_LOCK, shmem_test_lock)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_SET_LOCK,
        shmem_set_lock_,
        shmem_set_lock__,
        shmem_set_lock_f,
        (FORTRAN_POINTER_T lock), 
        (lock))

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_CLEAR_LOCK,
        shmem_clear_lock_,
        shmem_clear_lock__,
        shmem_clear_lock_f,
        (FORTRAN_POINTER_T lock), 
        (lock))

SHMEM_GENERATE_FORTRAN_BINDINGS_FUNCTION (MPI_Fint,
        SHMEM_TEST_LOCK,
        shmem_test_lock_,
        shmem_test_lock__,
        shmem_test_lock_f,
        (FORTRAN_POINTER_T lock), 
        (lock))

void shmem_set_lock_f(FORTRAN_POINTER_T lock)
{
    _shmem_set_lock(FPTR_2_VOID_PTR(lock), sizeof(MPI_Fint));
}

void shmem_clear_lock_f(FORTRAN_POINTER_T lock)
{
    _shmem_clear_lock(FPTR_2_VOID_PTR(lock), sizeof(MPI_Fint));
}

MPI_Fint shmem_test_lock_f(FORTRAN_POINTER_T lock)
{
    return _shmem_test_lock(FPTR_2_VOID_PTR(lock), sizeof(MPI_Fint));
}
