/*
 * Copyright (c) 2013-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem_config.h"

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"

#include "oshmem/runtime/runtime.h"

#include "oshmem/op/op.h"
#include "oshmem/mca/atomic/atomic.h"

/*
 * These routines perform an atomic fetch-and-add operation.
 * The fetch and add routines retrieve the value at address target on PE pe, and update
 * target with the result of adding value to the retrieved value. The operation must be completed
 * without the possibility of another process updating target between the time of the
 * fetch and the update.
 */

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_fadd = pshmem_int_atomic_fadd
#pragma weak shmem_long_atomic_fadd = pshmem_long_atomic_fadd
#pragma weak shmem_longlong_atomic_fadd = pshmem_longlong_atomic_fadd
#pragma weak shmemx_int32_fadd = pshmemx_int32_fadd
#pragma weak shmemx_int64_fadd = pshmemx_int64_fadd
/* backward compatibility */
#pragma weak shmem_int_fadd = pshmem_int_atomic_fadd
#pragma weak shmem_long_fadd = pshmem_long_atomic_fadd
#pragma weak shmem_longlong_fadd = pshmem_longlong_atomic_fadd
#pragma weak pshmem_int_fadd = pshmem_int_atomic_fadd
#pragma weak pshmem_long_fadd = pshmem_long_atomic_fadd
#pragma weak pshmem_longlong_fadd = pshmem_longlong_atomic_fadd
#include "oshmem/shmem/c/profile/defines.h"
#else
#pragma weak shmem_int_fadd = shmem_int_atomic_fadd
#pragma weak shmem_long_fadd = shmem_long_atomic_fadd
#pragma weak shmem_longlong_fadd = shmem_longlong_atomic_fadd
#endif

SHMEM_TYPE_FOP(_int, int, fadd, shmem, atomic_fadd)
SHMEM_TYPE_FOP(_long, long, fadd, shmem, atomic_fadd)
SHMEM_TYPE_FOP(_longlong, long long, fadd, shmem, atomic_fadd)
SHMEM_TYPE_FOP(_int32, int32_t, fadd, shmemx, fadd)
SHMEM_TYPE_FOP(_int64, int64_t, fadd, shmemx, fadd)
