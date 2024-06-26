/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_ATOMIC_GCC_ATOMIC_H_INCLUDED
#define MPL_ATOMIC_GCC_ATOMIC_H_INCLUDED

#include <stdint.h>

#ifdef __SUNPRO_C
/* Solaris Studio 12.6 shows warnings if an argument of __atomic builtins is
 * qualified with const or volatile.  The following pragma suppresses it. */
#pragma error_messages (off, E_ARG_INCOMPATIBLE_WITH_ARG_L)
#endif

#define MPLI_ATOMIC_INITIALIZER(val_) { (val_) }

#define MPL_ATOMIC_INT_T_INITIALIZER(val_)    MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_INT32_T_INITIALIZER(val_)  MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_UINT32_T_INITIALIZER(val_) MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_INT64_T_INITIALIZER(val_)  MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_UINT64_T_INITIALIZER(val_) MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_PTR_T_INITIALIZER(val_)    MPLI_ATOMIC_INITIALIZER(val_)

#define MPLI_ATOMIC_DECL_FUNC_COMMON(TYPE, NAME)                               \
struct MPL_atomic_ ## NAME ## _t {                                             \
     TYPE volatile v;                                                          \
};                                                                             \
static inline TYPE MPL_atomic_relaxed_load_ ## NAME                            \
                                (const struct MPL_atomic_ ## NAME ## _t * ptr) \
{                                                                              \
    return __atomic_load_n(&ptr->v, __ATOMIC_RELAXED);                         \
}                                                                              \
static inline TYPE MPL_atomic_acquire_load_ ## NAME                            \
                                (const struct MPL_atomic_ ## NAME ## _t * ptr) \
{                                                                              \
    return __atomic_load_n(&ptr->v, __ATOMIC_ACQUIRE);                         \
}                                                                              \
static inline void MPL_atomic_relaxed_store_ ## NAME                           \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    __atomic_store_n(&ptr->v, val, __ATOMIC_RELAXED);                          \
}                                                                              \
static inline void MPL_atomic_release_store_ ## NAME                           \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    __atomic_store_n(&ptr->v, val, __ATOMIC_RELEASE);                          \
}                                                                              \
static inline TYPE MPL_atomic_cas_ ## NAME                                     \
                (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE oldv, TYPE newv) \
{                                                                              \
    __atomic_compare_exchange_n(&ptr->v, &oldv, newv, 0, __ATOMIC_ACQ_REL,     \
                                __ATOMIC_ACQUIRE);                             \
    return oldv;                                                               \
}                                                                              \
static inline TYPE MPL_atomic_swap_ ## NAME                                    \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    return __atomic_exchange_n(&ptr->v, val, __ATOMIC_ACQ_REL);                \
}

#define MPLI_ATOMIC_DECL_FUNC_FAA(TYPE, NAME)                                   \
static inline TYPE MPL_atomic_fetch_add_ ## NAME                               \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    return __atomic_fetch_add(&ptr->v, val, __ATOMIC_ACQ_REL);                 \
}                                                                              \
static inline TYPE MPL_atomic_fetch_sub_ ## NAME                               \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    return __atomic_fetch_sub(&ptr->v, val, __ATOMIC_ACQ_REL);                 \
}

#define MPLI_ATOMIC_DECL_FUNC_VAL(TYPE, NAME) \
        MPLI_ATOMIC_DECL_FUNC_COMMON(TYPE, NAME) \
        MPLI_ATOMIC_DECL_FUNC_FAA(TYPE, NAME)

#define MPLI_ATOMIC_DECL_FUNC_PTR(TYPE, NAME) \
        MPLI_ATOMIC_DECL_FUNC_COMMON(TYPE, NAME)

/* int */
MPLI_ATOMIC_DECL_FUNC_VAL(int, int)
/* int32_t */
MPLI_ATOMIC_DECL_FUNC_VAL(int32_t, int32)
/* uint32_t */
MPLI_ATOMIC_DECL_FUNC_VAL(uint32_t, uint32)
/* int64_t */
MPLI_ATOMIC_DECL_FUNC_VAL(int64_t, int64)
/* uint64_t */
MPLI_ATOMIC_DECL_FUNC_VAL(uint64_t, uint64)
/* void * */
MPLI_ATOMIC_DECL_FUNC_PTR(void *, ptr)
#undef MPLI_ATOMIC_DECL_FUNC_COMMON
#undef MPLI_ATOMIC_DECL_FUNC_FAA
#undef MPLI_ATOMIC_DECL_FUNC_VAL
#undef MPLI_ATOMIC_DECL_FUNC_PTR
static inline void MPL_atomic_write_barrier(void)
{
    __atomic_thread_fence(__ATOMIC_RELEASE);
}

static inline void MPL_atomic_read_barrier(void)
{
    __atomic_thread_fence(__ATOMIC_ACQUIRE);
}

static inline void MPL_atomic_read_write_barrier(void)
{
    __atomic_thread_fence(__ATOMIC_ACQ_REL);
}

static inline void MPL_atomic_compiler_barrier(void)
{
    /* atomic_signal_fence performs a compiler barrier without any overhead */
    __atomic_signal_fence(__ATOMIC_ACQ_REL);
}

#ifdef __SUNPRO_C
#pragma error_messages (default, E_ARG_INCOMPATIBLE_WITH_ARG_L)
#endif

#endif /* MPL_ATOMIC_GCC_ATOMIC_H_INCLUDED */
