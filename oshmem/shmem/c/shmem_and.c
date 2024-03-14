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
#pragma weak shmem_int_atomic_and       = pshmem_int_atomic_and
#pragma weak shmem_long_atomic_and      = pshmem_long_atomic_and
#pragma weak shmem_longlong_atomic_and  = pshmem_longlong_atomic_and
#pragma weak shmem_uint_atomic_and      = pshmem_uint_atomic_and
#pragma weak shmem_ulong_atomic_and     = pshmem_ulong_atomic_and
#pragma weak shmem_ulonglong_atomic_and = pshmem_ulonglong_atomic_and
#pragma weak shmem_int32_atomic_and     = pshmem_int32_atomic_and
#pragma weak shmem_int64_atomic_and     = pshmem_int64_atomic_and
#pragma weak shmem_uint32_atomic_and    = pshmem_uint32_atomic_and
#pragma weak shmem_uint64_atomic_and    = pshmem_uint64_atomic_and

#pragma weak shmem_ctx_int_atomic_and   = pshmem_ctx_int_atomic_and
#pragma weak shmem_ctx_long_atomic_and  = pshmem_ctx_long_atomic_and
#pragma weak shmem_ctx_longlong_atomic_and = pshmem_ctx_longlong_atomic_and
#pragma weak shmem_ctx_uint_atomic_and  = pshmem_ctx_uint_atomic_and
#pragma weak shmem_ctx_ulong_atomic_and = pshmem_ctx_ulong_atomic_and
#pragma weak shmem_ctx_ulonglong_atomic_and = pshmem_ctx_ulonglong_atomic_and
#pragma weak shmem_ctx_int32_atomic_and  = pshmem_ctx_int32_atomic_and
#pragma weak shmem_ctx_int64_atomic_and  = pshmem_ctx_int64_atomic_and
#pragma weak shmem_ctx_uint32_atomic_and = pshmem_ctx_uint32_atomic_and
#pragma weak shmem_ctx_uint64_atomic_and = pshmem_ctx_uint64_atomic_and

#pragma weak shmemx_int32_atomic_and    = pshmemx_int32_atomic_and
#pragma weak shmemx_int64_atomic_and    = pshmemx_int64_atomic_and
#pragma weak shmemx_uint32_atomic_and   = pshmemx_uint32_atomic_and
#pragma weak shmemx_uint64_atomic_and   = pshmemx_uint64_atomic_and
#include "oshmem/shmem/c/profile-defines.h"
#endif

OSHMEM_TYPE_OP(int, int, shmem, and)
OSHMEM_TYPE_OP(long, long, shmem, and)
OSHMEM_TYPE_OP(longlong, long long, shmem, and)
OSHMEM_TYPE_OP(uint, unsigned int, shmem, and)
OSHMEM_TYPE_OP(ulong, unsigned long, shmem, and)
OSHMEM_TYPE_OP(ulonglong, unsigned long long, shmem, and)
OSHMEM_TYPE_OP(int32, int32_t, shmem, and)
OSHMEM_TYPE_OP(int64, int64_t, shmem, and)
OSHMEM_TYPE_OP(uint32, uint32_t, shmem, and)
OSHMEM_TYPE_OP(uint64, uint64_t, shmem, and)

OSHMEM_CTX_TYPE_OP(int, int, shmem, and)
OSHMEM_CTX_TYPE_OP(long, long, shmem, and)
OSHMEM_CTX_TYPE_OP(longlong, long long, shmem, and)
OSHMEM_CTX_TYPE_OP(uint, unsigned int, shmem, and)
OSHMEM_CTX_TYPE_OP(ulong, unsigned long, shmem, and)
OSHMEM_CTX_TYPE_OP(ulonglong, unsigned long long, shmem, and)
OSHMEM_CTX_TYPE_OP(int32, int32_t, shmem, and)
OSHMEM_CTX_TYPE_OP(int64, int64_t, shmem, and)
OSHMEM_CTX_TYPE_OP(uint32, uint32_t, shmem, and)
OSHMEM_CTX_TYPE_OP(uint64, uint64_t, shmem, and)

OSHMEM_TYPE_OP(int32, int32_t, shmemx, and)
OSHMEM_TYPE_OP(int64, int64_t, shmemx, and)
OSHMEM_TYPE_OP(uint32, uint32_t, shmemx, and)
OSHMEM_TYPE_OP(uint64, uint64_t, shmemx, and)
