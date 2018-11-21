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
 * These routines perform an atomic fetch-and-and operation.
 * The fetch and and routines retrieve the value at address target on PE pe, and update
 * target with the result of 'and' operation value to the retrieved value. The operation
 * must be completed without the possibility of another process updating target between
 * the time of the fetch and the update.
 */
#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_int_atomic_fetch_and       = pshmem_int_atomic_fetch_and
#pragma weak shmem_long_atomic_fetch_and      = pshmem_long_atomic_fetch_and
#pragma weak shmem_longlong_atomic_fetch_and  = pshmem_longlong_atomic_fetch_and
#pragma weak shmem_uint_atomic_fetch_and      = pshmem_uint_atomic_fetch_and
#pragma weak shmem_ulong_atomic_fetch_and     = pshmem_ulong_atomic_fetch_and
#pragma weak shmem_ulonglong_atomic_fetch_and = pshmem_ulonglong_atomic_fetch_and
#pragma weak shmem_int32_atomic_fetch_and     = pshmem_int32_atomic_fetch_and
#pragma weak shmem_int64_atomic_fetch_and     = pshmem_int64_atomic_fetch_and
#pragma weak shmem_uint32_atomic_fetch_and    = pshmem_uint32_atomic_fetch_and
#pragma weak shmem_uint64_atomic_fetch_and    = pshmem_uint64_atomic_fetch_and

#pragma weak shmem_ctx_int_atomic_fetch_and   = pshmem_ctx_int_atomic_fetch_and
#pragma weak shmem_ctx_long_atomic_fetch_and  = pshmem_ctx_long_atomic_fetch_and
#pragma weak shmem_ctx_longlong_atomic_fetch_and = pshmem_ctx_longlong_atomic_fetch_and
#pragma weak shmem_ctx_uint_atomic_fetch_and  = pshmem_ctx_uint_atomic_fetch_and
#pragma weak shmem_ctx_ulong_atomic_fetch_and = pshmem_ctx_ulong_atomic_fetch_and
#pragma weak shmem_ctx_ulonglong_atomic_fetch_and = pshmem_ctx_ulonglong_atomic_fetch_and
#pragma weak shmem_ctx_int32_atomic_fetch_and  = pshmem_ctx_int32_atomic_fetch_and
#pragma weak shmem_ctx_int64_atomic_fetch_and  = pshmem_ctx_int64_atomic_fetch_and
#pragma weak shmem_ctx_uint32_atomic_fetch_and = pshmem_ctx_uint32_atomic_fetch_and
#pragma weak shmem_ctx_uint64_atomic_fetch_and = pshmem_ctx_uint64_atomic_fetch_and

#pragma weak shmemx_int32_atomic_fetch_and    = pshmemx_int32_atomic_fetch_and
#pragma weak shmemx_int64_atomic_fetch_and    = pshmemx_int64_atomic_fetch_and
#pragma weak shmemx_uint32_atomic_fetch_and   = pshmemx_uint32_atomic_fetch_and
#pragma weak shmemx_uint64_atomic_fetch_and   = pshmemx_uint64_atomic_fetch_and
#include "oshmem/shmem/c/profile/defines.h"
#endif

OSHMEM_TYPE_FOP(int, int, shmem, and)
OSHMEM_TYPE_FOP(long, long, shmem, and)
OSHMEM_TYPE_FOP(longlong, long long, shmem, and)
OSHMEM_TYPE_FOP(uint, unsigned int, shmem, and)
OSHMEM_TYPE_FOP(ulong, unsigned long, shmem, and)
OSHMEM_TYPE_FOP(ulonglong, unsigned long long, shmem, and)
OSHMEM_TYPE_FOP(int32, int32_t, shmem, and)
OSHMEM_TYPE_FOP(int64, int64_t, shmem, and)
OSHMEM_TYPE_FOP(uint32, uint32_t, shmem, and)
OSHMEM_TYPE_FOP(uint64, uint64_t, shmem, and)

OSHMEM_CTX_TYPE_FOP(int, int, shmem, and)
OSHMEM_CTX_TYPE_FOP(long, long, shmem, and)
OSHMEM_CTX_TYPE_FOP(longlong, long long, shmem, and)
OSHMEM_CTX_TYPE_FOP(uint, unsigned int, shmem, and)
OSHMEM_CTX_TYPE_FOP(ulong, unsigned long, shmem, and)
OSHMEM_CTX_TYPE_FOP(ulonglong, unsigned long long, shmem, and)
OSHMEM_CTX_TYPE_FOP(int32, int32_t, shmem, and)
OSHMEM_CTX_TYPE_FOP(int64, int64_t, shmem, and)
OSHMEM_CTX_TYPE_FOP(uint32, uint32_t, shmem, and)
OSHMEM_CTX_TYPE_FOP(uint64, uint64_t, shmem, and)

OSHMEM_TYPE_FOP(int32, int32_t, shmemx, and)
OSHMEM_TYPE_FOP(int64, int64_t, shmemx, and)
OSHMEM_TYPE_FOP(uint32, uint32_t, shmemx, and)
OSHMEM_TYPE_FOP(uint64, uint64_t, shmemx, and)
