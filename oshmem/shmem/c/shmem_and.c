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
 * These routines perform an atomic 'and' operation.
 * The atomic 'and' routines operates 'and' value to the data at address target on PE pe.
 * The operation must be completed without the possibility of another process updating
 * target between the time of the fetch and the update.
 */
#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_and      = pshmem_int_atomic_and
#pragma weak shmem_long_atomic_and     = pshmem_long_atomic_and
#pragma weak shmem_longlong_atomic_and = pshmem_longlong_atomic_and
#pragma weak shmemx_int32_atomic_and   = pshmemx_int32_atomic_and
#pragma weak shmemx_int64_atomic_and   = pshmemx_int64_atomic_and
#include "oshmem/shmem/c/profile/defines.h"
#endif

OSHMEM_TYPE_OP(_int, int, shmem, and)
OSHMEM_TYPE_OP(_long, long, shmem, and)
OSHMEM_TYPE_OP(_longlong, long long, shmem, and)
OSHMEM_TYPE_OP(_int32, int32_t, shmemx, and)
OSHMEM_TYPE_OP(_int64, int64_t, shmemx, and)
