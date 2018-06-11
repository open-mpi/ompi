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
 * shmem_swap performs an atomic swap operation.
 * The atomic swap routines write value to address target on PE pe, and return the previous
 * contents of target. The operation must be completed without the possibility of another
 * process updating target between the time of the fetch and the update.
 */

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_swap      = pshmem_int_atomic_swap
#pragma weak shmem_long_atomic_swap     = pshmem_long_atomic_swap
#pragma weak shmem_longlong_atomic_swap = pshmem_longlong_atomic_swap
#pragma weak shmem_float_atomic_swap    = pshmem_float_atomic_swap
#pragma weak shmem_double_atomic_swap   = pshmem_double_atomic_swap
#pragma weak shmemx_int32_swap          = pshmemx_int32_swap
#pragma weak shmemx_int64_swap          = pshmemx_int64_swap
/* Backward compatibility */
#pragma weak shmem_int_swap      = pshmem_int_atomic_swap
#pragma weak shmem_long_swap     = pshmem_long_atomic_swap
#pragma weak shmem_longlong_swap = pshmem_longlong_atomic_swap
#pragma weak shmem_float_swap    = pshmem_float_atomic_swap
#pragma weak shmem_double_swap   = pshmem_double_atomic_swap
#include "oshmem/shmem/c/profile/defines.h"
#else
#pragma weak shmem_int_swap      = shmem_int_atomic_swap
#pragma weak shmem_long_swap     = shmem_long_atomic_swap
#pragma weak shmem_longlong_swap = shmem_longlong_atomic_swap
#pragma weak shmem_float_swap    = shmem_float_atomic_swap
#pragma weak shmem_double_swap   = shmem_double_atomic_swap
#endif

SHMEM_TYPE_FOP(_int, int, swap, swap, shmem, atomic_swap)
SHMEM_TYPE_FOP(_long, long, swap, swap, shmem, atomic_swap)
SHMEM_TYPE_FOP(_longlong, long long, swap, swap, shmem, atomic_swap)
SHMEM_TYPE_FOP(_float, float, swap, swap, shmem, atomic_swap)
SHMEM_TYPE_FOP(_double, double, swap, swap, shmem, atomic_swap)
SHMEM_TYPE_FOP(_int32, int32_t, swap, swap, shmemx, swap)
SHMEM_TYPE_FOP(_int64, int64_t, swap, swap, shmemx, swap)
