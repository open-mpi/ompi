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
 * The atomic 'or' routines operates 'or' value to the data at address target on PE pe.
 * The operation must be completed without the possibility of another process updating
 * target between the time of the fetch or the update.
 */
#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_or       = pshmem_int_atomic_or
#pragma weak shmem_long_atomic_or      = pshmem_long_atomic_or
#pragma weak shmem_longlong_atomic_or  = pshmem_longlong_atomic_or
#pragma weak shmem_uint_atomic_or      = pshmem_uint_atomic_or
#pragma weak shmem_ulong_atomic_or     = pshmem_ulong_atomic_or
#pragma weak shmem_ulonglong_atomic_or = pshmem_ulonglong_atomic_or
#pragma weak shmem_int32_atomic_or     = pshmem_int32_atomic_or
#pragma weak shmem_int64_atomic_or     = pshmem_int64_atomic_or
#pragma weak shmem_uint32_atomic_or    = pshmem_uint32_atomic_or
#pragma weak shmem_uint64_atomic_or    = pshmem_uint64_atomic_or

#pragma weak shmem_ctx_int_atomic_or   = pshmem_ctx_int_atomic_or
#pragma weak shmem_ctx_long_atomic_or  = pshmem_ctx_long_atomic_or
#pragma weak shmem_ctx_longlong_atomic_or = pshmem_ctx_longlong_atomic_or
#pragma weak shmem_ctx_uint_atomic_or  = pshmem_ctx_uint_atomic_or
#pragma weak shmem_ctx_ulong_atomic_or = pshmem_ctx_ulong_atomic_or
#pragma weak shmem_ctx_ulonglong_atomic_or = pshmem_ctx_ulonglong_atomic_or
#pragma weak shmem_ctx_int32_atomic_or  = pshmem_ctx_int32_atomic_or
#pragma weak shmem_ctx_int64_atomic_or  = pshmem_ctx_int64_atomic_or
#pragma weak shmem_ctx_uint32_atomic_or = pshmem_ctx_uint32_atomic_or
#pragma weak shmem_ctx_uint64_atomic_or = pshmem_ctx_uint64_atomic_or

#pragma weak shmemx_int32_atomic_or    = pshmemx_int32_atomic_or
#pragma weak shmemx_int64_atomic_or    = pshmemx_int64_atomic_or
#pragma weak shmemx_uint32_atomic_or   = pshmemx_uint32_atomic_or
#pragma weak shmemx_uint64_atomic_or   = pshmemx_uint64_atomic_or
#include "oshmem/shmem/c/profile/defines.h"
#endif

OSHMEM_TYPE_OP(int, int, shmem, or)
OSHMEM_TYPE_OP(long, long, shmem, or)
OSHMEM_TYPE_OP(longlong, long long, shmem, or)
OSHMEM_TYPE_OP(uint, unsigned int, shmem, or)
OSHMEM_TYPE_OP(ulong, unsigned long, shmem, or)
OSHMEM_TYPE_OP(ulonglong, unsigned long long, shmem, or)
OSHMEM_TYPE_OP(int32, int32_t, shmem, or)
OSHMEM_TYPE_OP(int64, int64_t, shmem, or)
OSHMEM_TYPE_OP(uint32, uint32_t, shmem, or)
OSHMEM_TYPE_OP(uint64, uint64_t, shmem, or)

OSHMEM_CTX_TYPE_OP(int, int, shmem, or)
OSHMEM_CTX_TYPE_OP(long, long, shmem, or)
OSHMEM_CTX_TYPE_OP(longlong, long long, shmem, or)
OSHMEM_CTX_TYPE_OP(uint, unsigned int, shmem, or)
OSHMEM_CTX_TYPE_OP(ulong, unsigned long, shmem, or)
OSHMEM_CTX_TYPE_OP(ulonglong, unsigned long long, shmem, or)
OSHMEM_CTX_TYPE_OP(int32, int32_t, shmem, or)
OSHMEM_CTX_TYPE_OP(int64, int64_t, shmem, or)
OSHMEM_CTX_TYPE_OP(uint32, uint32_t, shmem, or)
OSHMEM_CTX_TYPE_OP(uint64, uint64_t, shmem, or)

OSHMEM_TYPE_OP(int32, int32_t, shmemx, or)
OSHMEM_TYPE_OP(int64, int64_t, shmemx, or)
OSHMEM_TYPE_OP(uint32, uint32_t, shmemx, or)
OSHMEM_TYPE_OP(uint64, uint64_t, shmemx, or)
