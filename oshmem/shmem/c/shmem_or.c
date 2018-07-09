/*
 * Copyright (c) 2018      Mellanox Technologies, Inc.
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
#include "oshmem/include/shmemx.h"

#include "oshmem/runtime/runtime.h"
#include "oshmem/op/op.h"
#include "oshmem/mca/atomic/atomic.h"

/*
 * These routines perform an atomic 'or' operation.
 * The atomic 'or' routines cwoperates 'or' value to the data at address target on PE pe.
 * The operation must be completed without the possibility of another process updating
 * target between the time of the fetch or the update.
 */
#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_or      = pshmem_int_atomic_or
#pragma weak shmem_long_atomic_or     = pshmem_long_atomic_or
#pragma weak shmem_longlong_atomic_or = pshmem_longlong_atomic_or
#pragma weak shmemx_int32_atomic_or   = pshmemx_int32_atomic_or
#pragma weak shmemx_int64_atomic_or   = pshmemx_int64_atomic_or
#include "oshmem/shmem/c/profile/defines.h"
#endif

OSHMEM_TYPE_OP(_int, int, shmem, or)
OSHMEM_TYPE_OP(_long, long, shmem, or)
OSHMEM_TYPE_OP(_longlong, long long, shmem, or)
OSHMEM_TYPE_OP(_int32, int32_t, shmemx, or)
OSHMEM_TYPE_OP(_int64, int64_t, shmemx, or)
