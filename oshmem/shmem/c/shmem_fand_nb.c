/*
 * Copyright (c) 2021      NVIDIA Corporation.
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
 * These routines perform an atomic nonblocking fetch-and-and operation.
 * The fetch and and routines retrieve the value at address target on PE pe, and update
 * target with the result of 'and' operation value to the retrieved value. The operation
 * must be completed without the possibility of another process updating target between
 * the time of the fetch and the update.
 */
#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"

/* Nonblocking Atomic Fetch and And */
#pragma weak shmem_ctx_uint_atomic_fetch_and_nbi              = pshmem_ctx_uint_atomic_fetch_and_nbi
#pragma weak shmem_ctx_ulong_atomic_fetch_and_nbi             = pshmem_ctx_ulong_atomic_fetch_and_nbi
#pragma weak shmem_ctx_ulonglong_atomic_fetch_and_nbi         = pshmem_ctx_ulonglong_atomic_fetch_and_nbi
#pragma weak shmem_ctx_int32_atomic_fetch_and_nbi             = pshmem_ctx_int32_atomic_fetch_and_nbi
#pragma weak shmem_ctx_int64_atomic_fetch_and_nbi             = pshmem_ctx_int64_atomic_fetch_and_nbi
#pragma weak shmem_ctx_uint32_atomic_fetch_and_nbi            = pshmem_ctx_uint32_atomic_fetch_and_nbi
#pragma weak shmem_ctx_uint64_atomic_fetch_and_nbi            = pshmem_ctx_uint64_atomic_fetch_and_nbi

#pragma weak shmem_uint_atomic_fetch_and_nbi       			  = pshmem_uint_atomic_fetch_and_nbi
#pragma weak shmem_ulong_atomic_fetch_and_nbi      			  = pshmem_ulong_atomic_fetch_and_nbi
#pragma weak shmem_ulonglong_atomic_fetch_and_nbi  			  = pshmem_ulonglong_atomic_fetch_and_nbi
#pragma weak shmem_int32_atomic_fetch_and_nbi      			  = pshmem_int32_atomic_fetch_and_nbi
#pragma weak shmem_int64_atomic_fetch_and_nbi      			  = pshmem_int64_atomic_fetch_and_nbi
#pragma weak shmem_uint32_atomic_fetch_and_nbi     			  = pshmem_uint32_atomic_fetch_and_nbi
#pragma weak shmem_uint64_atomic_fetch_and_nbi     			  = pshmem_uint64_atomic_fetch_and_nbi

#include "oshmem/shmem/c/profile-defines.h"
#endif

OSHMEM_TYPE_FOP_NBI(uint, unsigned int, shmem, and)
OSHMEM_TYPE_FOP_NBI(ulong, unsigned long, shmem, and)
OSHMEM_TYPE_FOP_NBI(ulonglong, unsigned long long, shmem, and)
OSHMEM_TYPE_FOP_NBI(int32, int32_t, shmem, and)
OSHMEM_TYPE_FOP_NBI(int64, int64_t, shmem, and)
OSHMEM_TYPE_FOP_NBI(uint32, uint32_t, shmem, and)
OSHMEM_TYPE_FOP_NBI(uint64, uint64_t, shmem, and)

OSHMEM_CTX_TYPE_FOP_NBI(uint, unsigned int, shmem, and)
OSHMEM_CTX_TYPE_FOP_NBI(ulong, unsigned long, shmem, and)
OSHMEM_CTX_TYPE_FOP_NBI(ulonglong, unsigned long long, shmem, and)
OSHMEM_CTX_TYPE_FOP_NBI(int32, int32_t, shmem, and)
OSHMEM_CTX_TYPE_FOP_NBI(int64, int64_t, shmem, and)
OSHMEM_CTX_TYPE_FOP_NBI(uint32, uint32_t, shmem, and)
OSHMEM_CTX_TYPE_FOP_NBI(uint64, uint64_t, shmem, and)

