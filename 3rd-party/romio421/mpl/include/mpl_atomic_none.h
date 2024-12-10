/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_ATOMIC_NONE_H_INCLUDED
#define MPL_ATOMIC_NONE_H_INCLUDED

#include <stdint.h>

#define MPLI_ATOMIC_INITIALIZER(val_) { (val_) }

#define MPL_ATOMIC_INT_T_INITIALIZER(val_)    MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_INT32_T_INITIALIZER(val_)  MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_UINT32_T_INITIALIZER(val_) MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_INT64_T_INITIALIZER(val_)  MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_UINT64_T_INITIALIZER(val_) MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_PTR_T_INITIALIZER(val_)    MPLI_ATOMIC_INITIALIZER(val_)

/* The following implementation assumes that loads/stores are atomic on the
 * current platform, even though this may not be true at all. */

#define MPLI_ATOMIC_DECL_FUNC_COMMON(TYPE, NAME)                               \
struct MPL_atomic_ ## NAME ## _t {                                             \
    TYPE v;                                                                    \
};                                                                             \
static inline TYPE MPL_atomic_relaxed_load_ ## NAME                            \
                                (const struct MPL_atomic_ ## NAME ## _t * ptr) \
{                                                                              \
    return ptr->v;                                                             \
}                                                                              \
static inline TYPE MPL_atomic_acquire_load_ ## NAME                            \
                                (const struct MPL_atomic_ ## NAME ## _t * ptr) \
{                                                                              \
    return ptr->v;                                                             \
}                                                                              \
static inline void MPL_atomic_relaxed_store_ ## NAME                           \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    ptr->v = val;                                                              \
}                                                                              \
static inline void MPL_atomic_release_store_ ## NAME                           \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    ptr->v = val;                                                              \
}                                                                              \
static inline TYPE MPL_atomic_cas_ ## NAME                                     \
                (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE oldv, TYPE newv) \
{                                                                              \
    TYPE prev = ptr->v;                                                        \
    if (prev == oldv)                                                          \
        ptr->v = newv;                                                         \
    return prev;                                                               \
}                                                                              \
static inline TYPE MPL_atomic_swap_ ## NAME                                    \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    TYPE prev = ptr->v;                                                        \
    ptr->v = val;                                                              \
    return prev;                                                               \
}

#define MPLI_ATOMIC_DECL_FUNC_FAA(TYPE, NAME)                                  \
static inline TYPE MPL_atomic_fetch_add_ ## NAME                               \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    TYPE prev = ptr->v;                                                        \
    ptr->v += val;                                                             \
    return prev;                                                               \
}                                                                              \
static inline TYPE MPL_atomic_fetch_sub_ ## NAME                               \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    TYPE prev = ptr->v;                                                        \
    ptr->v -= val;                                                             \
    return prev;                                                               \
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
/* Null barriers */
static inline void MPL_atomic_write_barrier(void)
{
    ;
}

static inline void MPL_atomic_read_barrier(void)
{
    ;
}

static inline void MPL_atomic_read_write_barrier(void)
{
    ;
}

static inline void MPL_atomic_compiler_barrier(void)
{
    ;
}

#endif /* MPL_ATOMIC_NONE_H_INCLUDED */
