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
 * These routines perform an atomic fetch-and-xor operation.
 * The fetch and xor routines retrieve the value at address target on PE pe, and update
 * target with the result of 'xor' operation value to the retrieved value. The operation
 * must be completed * without the possibility of another process updating target between
 * the time of the fetch and the update.
 */
#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_fxor      = pshmem_int_atomic_fxor
#pragma weak shmem_long_atomic_fxor     = pshmem_long_atomic_fxor
#pragma weak shmem_longlong_atomic_fxor = pshmem_longlong_atomic_fxor
#pragma weak shmemx_int32_atomic_fxor   = pshmemx_int32_atomic_fxor
#pragma weak shmemx_int64_atomic_fxor   = pshmemx_int64_atomic_fxor
#include "oshmem/shmem/c/profile/defines.h"
#endif

OSHMEM_TYPE_FOP(_int, int, shmem, fxor)
OSHMEM_TYPE_FOP(_long, long, shmem, fxor)
OSHMEM_TYPE_FOP(_longlong, long long, shmem, fxor)
OSHMEM_TYPE_FOP(_int32, int32_t, shmemx, fxor)
OSHMEM_TYPE_FOP(_int64, int64_t, shmemx, fxor)
