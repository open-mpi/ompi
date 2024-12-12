/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_ATOMIC_NT_INTRINSICS_H_INCLUDED
#define MPL_ATOMIC_NT_INTRINSICS_H_INCLUDED

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <intrin.h>

#define MPLI_ATOMIC_INITIALIZER(val_) { (val_) }

#define MPL_ATOMIC_INT_T_INITIALIZER(val_)    MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_INT32_T_INITIALIZER(val_)  MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_UINT32_T_INITIALIZER(val_) MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_INT64_T_INITIALIZER(val_)  MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_UINT64_T_INITIALIZER(val_) MPLI_ATOMIC_INITIALIZER(val_)
#if MPL_SIZEOF_VOID_P == 4
#define MPL_ATOMIC_PTR_T_INITIALIZER(val_) \
        MPLI_ATOMIC_INITIALIZER((long)(val_))
#elif MPL_SIZEOF_VOID_P == 8
#define MPL_ATOMIC_PTR_T_INITIALIZER(val_) \
        MPLI_ATOMIC_INITIALIZER((__int64)(val_))
#else
#error "MPL_SIZEOF_VOID_P not valid"
#endif

/*
 * NOTE: the current implementations assumes the following:
 * - _Interlocked builtins do not support int, uint32_t, uint64_t, and a void
 *   pointer, so they are internally converted to long, long, __int64, and
 *   long/__int64, respectively.
 * - Any normal read and write satisfy the relaxed memory ordering.
 * - Full memory barriers are used for acquire/release loads in a naive way.
 * TODO: read-write barriers guarantees nothing about interthread memory
 *       synchronization (see MSDN + _ReadWriteBarrier).  Use std::atomic.
 * Someone with more Windows expertise should feel free to improve these.
 */

#define MPLI_ATOMIC_DECL_FUNC_COMMON(TYPE, NAME, ATOMIC_TYPE, CAST_FROM_ATOMIC,\
                                     CAST_TO_ATOMIC, SUFFIX)                   \
struct MPL_atomic_ ## NAME ## _t {                                             \
    ATOMIC_TYPE volatile v;                                                    \
};                                                                             \
static inline TYPE MPL_atomic_relaxed_load_ ## NAME                            \
                                (const struct MPL_atomic_ ## NAME ## _t * ptr) \
{                                                                              \
    return CAST_FROM_ATOMIC(ptr->v);                                           \
}                                                                              \
static inline TYPE MPL_atomic_acquire_load_ ## NAME                            \
                                (const struct MPL_atomic_ ## NAME ## _t * ptr) \
{                                                                              \
    TYPE val = CAST_FROM_ATOMIC(ptr->v);                                       \
    _ReadWriteBarrier();                                                       \
    return val;                                                                \
}                                                                              \
static inline void MPL_atomic_relaxed_store_ ## NAME                           \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    ptr->v = CAST_TO_ATOMIC(val);                                              \
}                                                                              \
static inline void MPL_atomic_release_store_ ## NAME                           \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    _ReadWriteBarrier();                                                       \
    ptr->v = CAST_TO_ATOMIC(val);                                              \
}                                                                              \
static inline TYPE MPL_atomic_cas_ ## NAME                                     \
                (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE oldv, TYPE newv) \
{                                                                              \
    return CAST_FROM_ATOMIC(_InterlockedCompareExchange ## SUFFIX              \
                            ((ATOMIC_TYPE volatile *)&ptr->v,                  \
                             CAST_TO_ATOMIC(newv), CAST_TO_ATOMIC(oldv)));     \
}                                                                              \
static inline TYPE MPL_atomic_swap_ ## NAME                                    \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    return CAST_FROM_ATOMIC(_InterlockedExchange ## SUFFIX                     \
                            ((ATOMIC_TYPE volatile *)&ptr->v,                  \
                             CAST_TO_ATOMIC(val)));                            \
}

#define MPLI_ATOMIC_DECL_FUNC_FAA(TYPE, NAME, ATOMIC_TYPE, CAST_FROM_ATOMIC,   \
                                 CAST_TO_ATOMIC, SUFFIX)                       \
static inline TYPE MPL_atomic_fetch_add_ ## NAME                               \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    return CAST_FROM_ATOMIC(_InterlockedExchangeAdd ## SUFFIX                  \
                             (&ptr->v, CAST_TO_ATOMIC(val)));                  \
}                                                                              \
static inline TYPE MPL_atomic_fetch_sub_ ## NAME                               \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    return CAST_FROM_ATOMIC(_InterlockedExchangeAdd ## SUFFIX                  \
                             (&ptr->v, -CAST_TO_ATOMIC(val)));                 \
}

#define MPLI_ATOMIC_CAST_FROM_ATOMIC_VAL(TYPE)      (TYPE)
#define MPLI_ATOMIC_CAST_TO_ATOMIC_VAL(ATOMIC_TYPE) (ATOMIC_TYPE)
#define MPLI_ATOMIC_CAST_FROM_ATOMIC_PTR(TYPE)      (TYPE)(LONG_PTR)
#define MPLI_ATOMIC_CAST_TO_ATOMIC_PTR(ATOMIC_TYPE) (ATOMIC_TYPE)(LONG_PTR)

#define MPLI_ATOMIC_DECL_FUNC_VAL(TYPE, NAME, ATOMIC_TYPE, SUFFIX) \
        MPLI_ATOMIC_DECL_FUNC_COMMON(TYPE, NAME, ATOMIC_TYPE, \
            MPLI_ATOMIC_CAST_FROM_ATOMIC_VAL(TYPE), \
            MPLI_ATOMIC_CAST_TO_ATOMIC_VAL(ATOMIC_TYPE), SUFFIX) \
        MPLI_ATOMIC_DECL_FUNC_FAA(TYPE, NAME, ATOMIC_TYPE, \
            MPLI_ATOMIC_CAST_FROM_ATOMIC_VAL(TYPE), \
            MPLI_ATOMIC_CAST_TO_ATOMIC_VAL(ATOMIC_TYPE), SUFFIX)

#define MPLI_ATOMIC_DECL_FUNC_PTR(TYPE, NAME, ATOMIC_TYPE, SUFFIX) \
        MPLI_ATOMIC_DECL_FUNC_COMMON(TYPE, NAME, ATOMIC_TYPE, \
            MPLI_ATOMIC_CAST_FROM_ATOMIC_PTR(TYPE), \
            MPLI_ATOMIC_CAST_TO_ATOMIC_PTR(ATOMIC_TYPE), SUFFIX)

/* int */
MPLI_ATOMIC_DECL_FUNC_VAL(int, int, long, /* empty */)
/* int32_t */
MPLI_ATOMIC_DECL_FUNC_VAL(int32_t, int32, long, /* empty */)
/* uint32_t */
MPLI_ATOMIC_DECL_FUNC_VAL(uint32_t, uint32, long, /* empty */)
/* int64_t */
MPLI_ATOMIC_DECL_FUNC_VAL(int64_t, int64, __int64, 64)
/* uint64_t */
MPLI_ATOMIC_DECL_FUNC_VAL(uint64_t, uint64, __int64, 64)
/* void * */
#if MPL_SIZEOF_VOID_P == 4
MPLI_ATOMIC_DECL_FUNC_PTR(void *, ptr, long, /* empty */)
#elif MPL_SIZEOF_VOID_P == 8
MPLI_ATOMIC_DECL_FUNC_PTR(void *, ptr, __int64, 64)
#else
#error "MPL_SIZEOF_VOID_P not valid"
#endif
#undef MPLI_ATOMIC_DECL_FUNC_COMMON
#undef MPLI_ATOMIC_DECL_FUNC_FAA
#undef MPLI_ATOMIC_CAST_FROM_ATOMIC_VAL
#undef MPLI_ATOMIC_CAST_TO_ATOMIC_VAL
#undef MPLI_ATOMIC_CAST_FROM_ATOMIC_PTR
#undef MPLI_ATOMIC_CAST_TO_ATOMIC_PTR
#undef MPLI_ATOMIC_DECL_FUNC_VAL
#undef MPLI_ATOMIC_DECL_FUNC_PTR
/* Barriers */
static inline void MPL_atomic_write_barrier(void)
{
    _WriteBarrier();
}

static inline void MPL_atomic_read_barrier(void)
{
    _ReadBarrier();
}

static inline void MPL_atomic_read_write_barrier(void)
{
    _ReadWriteBarrier();
}

static inline void MPL_atomic_compiler_barrier(void)
{
    /* FIXME: there must be a more efficient way to implement this. */
    _ReadWriteBarrier();
}

#endif /* MPL_ATOMIC_NT_INTRINSICS_H_INCLUDED */
