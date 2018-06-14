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
 * These routines perform an atomic fetch-and-and operation.
 * The fetch and and routines retrieve the value at address target on PE pe, and update
 * target with the result of "and" value to the retrieved value. The operation must be completed
 * without the possibility of another process updating target between the time of the
 * fetch and the update.
 */

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_fand = pshmem_int_atomic_fand
#pragma weak shmem_long_atomic_fand = pshmem_long_atomic_fand
#pragma weak shmem_longlong_atomic_fand = pshmem_longlong_atomic_fand
#pragma weak shmemx_int32_fand = pshmemx_int32_fand
#pragma weak shmemx_int64_fand = pshmemx_int64_fand
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_FOP(_int, int, fand, and, shmem, atomic_fand)
SHMEM_TYPE_FOP(_long, long, fand, and, shmem, atomic_fand)
SHMEM_TYPE_FOP(_longlong, long long, fand, and, shmem, atomic_fand)
SHMEM_TYPE_FOP(_int32, int32_t, fand, and, shmemx, fand)
SHMEM_TYPE_FOP(_int64, int64_t, fand, and, shmemx, fand)
