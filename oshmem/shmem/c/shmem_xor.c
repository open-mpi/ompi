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

#include "oshmem/runtime/runtime.h"
#include "oshmem/op/op.h"
#include "oshmem/mca/atomic/atomic.h"

/*
 * These routines perform an atomic xor operation.
 * The atomic "xor" routines operates "xor" value to the data at address target on PE pe.
 * The operation must be completed without the possibility of another process updating
 * target between the time of the fetch and the update.
 */

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_xor = pshmem_int_atomic_xor
#pragma weak shmem_long_atomic_xor = pshmem_long_atomic_xor
#pragma weak shmem_longlong_atomic_xor = pshmem_longlong_atomic_xor
#pragma weak shmemx_int32_xor = pshmemx_int32_xor
#pragma weak shmemx_int64_xor = pshmemx_int64_xor
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_OP(_int, int, xor, xor, shmem, atomic_xor)
SHMEM_TYPE_OP(_long, long, xor, xor, shmem, atomic_xor)
SHMEM_TYPE_OP(_longlong, long long, xor, xor, shmem, atomic_xor)
SHMEM_TYPE_OP(_int32, int32_t, xor, xor, shmemx, xor)
SHMEM_TYPE_OP(_int64, int64_t, xor, xor, shmemx, xor)
