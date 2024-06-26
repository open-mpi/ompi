/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_ATOMIC_C11_H_INCLUDED
#define MPL_ATOMIC_C11_H_INCLUDED

#ifdef __INTEL_COMPILER
/*
 * Function prototypes of C11 atomic functions in ICC 19.0.4 are different from
 * the C11 standard, causing countless "dropping qualifiers" warnings.
 * The following pragmas are to suppress these warnings.
 */
#pragma warning(push)
#pragma warning(disable: 2330)
#endif

#include <stdint.h>
#include <stdatomic.h>

#if __STDC_VERSION__ >= 201710L
// C17 obsoletes ATOMIC_VAR_INIT.
#define MPLI_ATOMIC_INITIALIZER(val_) { (val_) }
#else
#define MPLI_ATOMIC_INITIALIZER(val_) { ATOMIC_VAR_INIT(val_) }
#endif

#define MPL_ATOMIC_INT_T_INITIALIZER(val_)    MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_INT32_T_INITIALIZER(val_)  MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_UINT32_T_INITIALIZER(val_) MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_INT64_T_INITIALIZER(val_)  MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_UINT64_T_INITIALIZER(val_) MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_PTR_T_INITIALIZER(val_) \
        MPLI_ATOMIC_INITIALIZER((intptr_t)(val_))

#define MPLI_ATOMIC_DECL_FUNC_COMMON(TYPE, NAME, ATOMIC_TYPE, CAST_TYPE)       \
struct MPL_atomic_ ## NAME ## _t {                                             \
    ATOMIC_TYPE v;                                                             \
};                                                                             \
static inline TYPE MPL_atomic_relaxed_load_ ## NAME                            \
                                (const struct MPL_atomic_ ## NAME ## _t * ptr) \
{                                                                              \
    return (TYPE)atomic_load_explicit(&ptr->v, memory_order_relaxed);          \
}                                                                              \
static inline TYPE MPL_atomic_acquire_load_ ## NAME                            \
                                (const struct MPL_atomic_ ## NAME ## _t * ptr) \
{                                                                              \
    return (TYPE)atomic_load_explicit(&ptr->v, memory_order_acquire);          \
}                                                                              \
static inline void MPL_atomic_relaxed_store_ ## NAME                           \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    atomic_store_explicit(&ptr->v, (CAST_TYPE)val, memory_order_relaxed);      \
}                                                                              \
static inline void MPL_atomic_release_store_ ## NAME                           \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    atomic_store_explicit(&ptr->v, (CAST_TYPE)val, memory_order_release);      \
}                                                                              \
static inline TYPE MPL_atomic_cas_ ## NAME                                     \
                (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE oldv, TYPE newv) \
{                                                                              \
    CAST_TYPE oldv_tmp = (CAST_TYPE)oldv;                                      \
    atomic_compare_exchange_strong_explicit(&ptr->v, &oldv_tmp,                \
                                            (CAST_TYPE)newv,                   \
                                            memory_order_acq_rel,              \
                                            memory_order_acquire);             \
    return (TYPE)oldv_tmp;                                                     \
}                                                                              \
static inline TYPE MPL_atomic_swap_ ## NAME                                    \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    return (TYPE)atomic_exchange_explicit(&ptr->v, (CAST_TYPE)val,             \
                                          memory_order_acq_rel);               \
}

#define MPLI_ATOMIC_DECL_FUNC_FAA(TYPE, NAME, ATOMIC_TYPE, CAST_TYPE)          \
static inline TYPE MPL_atomic_fetch_add_ ## NAME                               \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    return (TYPE)atomic_fetch_add_explicit(&ptr->v, (CAST_TYPE)val,            \
                                           memory_order_acq_rel);              \
}                                                                              \
static inline TYPE MPL_atomic_fetch_sub_ ## NAME                               \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    return (TYPE)atomic_fetch_sub_explicit(&ptr->v, (CAST_TYPE)val,            \
                                           memory_order_acq_rel);              \
}

#define MPLI_ATOMIC_DECL_FUNC_VAL(TYPE, NAME, ATOMIC_TYPE, CAST_TYPE) \
        MPLI_ATOMIC_DECL_FUNC_COMMON(TYPE, NAME, ATOMIC_TYPE, CAST_TYPE) \
        MPLI_ATOMIC_DECL_FUNC_FAA(TYPE, NAME, ATOMIC_TYPE, CAST_TYPE)

#define MPLI_ATOMIC_DECL_FUNC_PTR(TYPE, NAME, ATOMIC_TYPE, CAST_TYPE) \
        MPLI_ATOMIC_DECL_FUNC_COMMON(TYPE, NAME, ATOMIC_TYPE, CAST_TYPE)

MPLI_ATOMIC_DECL_FUNC_VAL(int, int, atomic_int, int)
MPLI_ATOMIC_DECL_FUNC_VAL(int32_t, int32, atomic_int_fast32_t, int_fast32_t)
MPLI_ATOMIC_DECL_FUNC_VAL(uint32_t, uint32, atomic_uint_fast32_t, uint_fast32_t)
MPLI_ATOMIC_DECL_FUNC_VAL(int64_t, int64, atomic_int_fast64_t, int_fast64_t)
MPLI_ATOMIC_DECL_FUNC_VAL(uint64_t, uint64, atomic_uint_fast64_t, uint_fast64_t)
MPLI_ATOMIC_DECL_FUNC_PTR(void *, ptr, atomic_intptr_t, intptr_t)
#undef MPLI_ATOMIC_DECL_FUNC_COMMON
#undef MPLI_ATOMIC_DECL_FUNC_FAA
#undef MPLI_ATOMIC_DECL_FUNC_VAL
#undef MPLI_ATOMIC_DECL_FUNC_PTR
static inline void MPL_atomic_write_barrier(void)
{
    atomic_thread_fence(memory_order_release);
}

static inline void MPL_atomic_read_barrier(void)
{
    atomic_thread_fence(memory_order_acquire);
}

static inline void MPL_atomic_read_write_barrier(void)
{
    atomic_thread_fence(memory_order_acq_rel);
}

static inline void MPL_atomic_compiler_barrier(void)
{
    /* atomic_signal_fence performs a compiler barrier without any overhead */
    atomic_signal_fence(memory_order_acq_rel);
}

#ifdef __INTEL_COMPILER
#pragma warning(pop)
#endif

#endif /* MPL_ATOMIC_C11_H_INCLUDED */
