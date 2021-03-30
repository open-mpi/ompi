/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_ATOMIC_H_INCLUDED
#define MPL_ATOMIC_H_INCLUDED

#include "mplconfig.h"
#include <stdint.h>

typedef struct MPL_atomic_int_t MPL_atomic_int_t;
typedef struct MPL_atomic_int32_t MPL_atomic_int32_t;
typedef struct MPL_atomic_uint32_t MPL_atomic_uint32_t;
typedef struct MPL_atomic_int64_t MPL_atomic_int64_t;
typedef struct MPL_atomic_uint64_t MPL_atomic_uint64_t;
typedef struct MPL_atomic_ptr_t MPL_atomic_ptr_t;

/* By default, we use stronger atomic sematics for load and store. However,
 * the relaxed semantics still can be used where it deemed approprate.
 */
#define MPL_atomic_load_int MPL_atomic_acquire_load_int
#define MPL_atomic_load_int32 MPL_atomic_acquire_load_int32
#define MPL_atomic_load_uint32 MPL_atomic_acquire_load_uint32
#define MPL_atomic_load_int64 MPL_atomic_acquire_load_int64
#define MPL_atomic_load_uint64 MPL_atomic_acquire_load_uint64
#define MPL_atomic_load_ptr MPL_atomic_acquire_load_ptr
#define MPL_atomic_store_int MPL_atomic_release_store_int
#define MPL_atomic_store_int32 MPL_atomic_release_store_int32
#define MPL_atomic_store_uint32 MPL_atomic_release_store_uint32
#define MPL_atomic_store_int64 MPL_atomic_release_store_int64
#define MPL_atomic_store_uint64 MPL_atomic_release_store_uint64
#define MPL_atomic_store_ptr MPL_atomic_release_store_ptr

/* Forward declarations of atomic functions */
/* MPL_atomic_relaxed_load */
static int MPL_atomic_relaxed_load_int(const MPL_atomic_int_t * ptr);
static int32_t MPL_atomic_relaxed_load_int32(const MPL_atomic_int32_t * ptr);
static uint32_t MPL_atomic_relaxed_load_uint32(const MPL_atomic_uint32_t * ptr);
static int64_t MPL_atomic_relaxed_load_int64(const MPL_atomic_int64_t * ptr);
static uint64_t MPL_atomic_relaxed_load_uint64(const MPL_atomic_uint64_t * ptr);
static void *MPL_atomic_relaxed_load_ptr(const MPL_atomic_ptr_t * ptr);
/* MPL_atomic_acquire_load */
static int MPL_atomic_acquire_load_int(const MPL_atomic_int_t * ptr);
static int32_t MPL_atomic_acquire_load_int32(const MPL_atomic_int32_t * ptr);
static uint32_t MPL_atomic_acquire_load_uint32(const MPL_atomic_uint32_t * ptr);
static int64_t MPL_atomic_acquire_load_int64(const MPL_atomic_int64_t * ptr);
static uint64_t MPL_atomic_acquire_load_uint64(const MPL_atomic_uint64_t * ptr);
static void *MPL_atomic_acquire_load_ptr(const MPL_atomic_ptr_t * ptr);
/* MPL_atomic_relaxed_store */
static void MPL_atomic_relaxed_store_int(MPL_atomic_int_t * ptr, int val);
static void MPL_atomic_relaxed_store_int32(MPL_atomic_int32_t * ptr, int32_t val);
static void MPL_atomic_relaxed_store_uint32(MPL_atomic_uint32_t * ptr, uint32_t val);
static void MPL_atomic_relaxed_store_int64(MPL_atomic_int64_t * ptr, int64_t val);
static void MPL_atomic_relaxed_store_uint64(MPL_atomic_uint64_t * ptr, uint64_t val);
static void MPL_atomic_relaxed_store_ptr(MPL_atomic_ptr_t * ptr, void *val);
/* MPL_atomic_release_store */
static void MPL_atomic_release_store_int(MPL_atomic_int_t * ptr, int val);

static void MPL_atomic_release_store_int32(MPL_atomic_int32_t * ptr, int32_t val);
static void MPL_atomic_release_store_uint32(MPL_atomic_uint32_t * ptr, uint32_t val);
static void MPL_atomic_release_store_int64(MPL_atomic_int64_t * ptr, int64_t val);
static void MPL_atomic_release_store_uint64(MPL_atomic_uint64_t * ptr, uint64_t val);
static void MPL_atomic_release_store_ptr(MPL_atomic_ptr_t * ptr, void *val);
/* MPL_atomic_swap */
static int MPL_atomic_swap_int(MPL_atomic_int_t * ptr, int val);
static int32_t MPL_atomic_swap_int32(MPL_atomic_int32_t * ptr, int32_t val);
static uint32_t MPL_atomic_swap_uint32(MPL_atomic_uint32_t * ptr, uint32_t val);
static int64_t MPL_atomic_swap_int64(MPL_atomic_int64_t * ptr, int64_t val);
static uint64_t MPL_atomic_swap_uint64(MPL_atomic_uint64_t * ptr, uint64_t val);
static void *MPL_atomic_swap_ptr(MPL_atomic_ptr_t * ptr, void *val);
/* MPL_atomic_cas (compare-and-swap) */
static int MPL_atomic_cas_int(MPL_atomic_int_t * ptr, int oldv, int newv);
static int32_t MPL_atomic_cas_int32(MPL_atomic_int32_t * ptr, int32_t oldv, int32_t newv);
static uint32_t MPL_atomic_cas_uint32(MPL_atomic_uint32_t * ptr, uint32_t oldv, uint32_t newv);
static int64_t MPL_atomic_cas_int64(MPL_atomic_int64_t * ptr, int64_t oldv, int64_t newv);
static uint64_t MPL_atomic_cas_uint64(MPL_atomic_uint64_t * ptr, uint64_t oldv, uint64_t newv);
static void *MPL_atomic_cas_ptr(MPL_atomic_ptr_t * ptr, void *oldv, void *newv);
/* MPL_atomic_fetch_add */
static int MPL_atomic_fetch_add_int(MPL_atomic_int_t * ptr, int val);
static int32_t MPL_atomic_fetch_add_int32(MPL_atomic_int32_t * ptr, int32_t val);
static uint32_t MPL_atomic_fetch_add_uint32(MPL_atomic_uint32_t * ptr, uint32_t val);
static int64_t MPL_atomic_fetch_add_int64(MPL_atomic_int64_t * ptr, int64_t val);
static uint64_t MPL_atomic_fetch_add_uint64(MPL_atomic_uint64_t * ptr, uint64_t val);
/* MPL_atomic_fetch_sub */
static int MPL_atomic_fetch_sub_int(MPL_atomic_int_t * ptr, int val);
static int32_t MPL_atomic_fetch_sub_int32(MPL_atomic_int32_t * ptr, int32_t val);
static uint32_t MPL_atomic_fetch_sub_uint32(MPL_atomic_uint32_t * ptr, uint32_t val);
static int64_t MPL_atomic_fetch_sub_int64(MPL_atomic_int64_t * ptr, int64_t val);
static uint64_t MPL_atomic_fetch_sub_uint64(MPL_atomic_uint64_t * ptr, uint64_t val);

/* MPL_atomic_barrier */
static void MPL_atomic_write_barrier(void);
static void MPL_atomic_read_barrier(void);
static void MPL_atomic_read_write_barrier(void);
static void MPL_atomic_compiler_barrier(void);

#if defined(MPL_USE_NO_ATOMIC_PRIMITIVES)
#include "mpl_atomic_none.h"
#elif defined(MPL_HAVE_C11_ATOMICS)
#include "mpl_atomic_c11.h"
#elif defined(MPL_HAVE_GCC_INTRINSIC_ATOMIC)
#include "mpl_atomic_gcc_atomic.h"
#elif defined(MPL_HAVE_GCC_INTRINSIC_SYNC)
#include "mpl_atomic_gcc_sync.h"
#elif defined(MPL_HAVE_NT_INTRINSICS)
#include "mpl_atomic_nt_intrinsics.h"
#elif defined(MPL_USE_LOCK_BASED_PRIMITIVES)
#include "mpl_atomic_by_lock.h"
#else
#error no primitives implementation specified
#endif

#endif /* MPL_ATOMIC_H_INCLUDED */
