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
 * These routines perform an atomic fetch-and-or operation.
 * The fetch and or routines retrieve the value at address target on PE pe, and update
 * target with the result of "or" value to the retrieved value. The operation must be completed
 * without the possibility of another process updating target between the time of the
 * fetch and the update.
 */

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_for = pshmem_int_atomic_for
#pragma weak shmem_long_atomic_for = pshmem_long_atomic_for
#pragma weak shmem_longlong_atomic_for = pshmem_longlong_atomic_for
#pragma weak shmemx_int32_for = pshmemx_int32_for
#pragma weak shmemx_int64_for = pshmemx_int64_for
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_FOP(_int, int, for, or, shmem, atomic_for)
SHMEM_TYPE_FOP(_long, long, for, or, shmem, atomic_for)
SHMEM_TYPE_FOP(_longlong, long long, for, or, shmem, atomic_for)
SHMEM_TYPE_FOP(_int32, int32_t, for, or, shmemx, for)
SHMEM_TYPE_FOP(_int64, int64_t, for, or, shmemx, for)
