/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_ATOMIC_BY_LOCK_H_INCLUDED
#define MPL_ATOMIC_BY_LOCK_H_INCLUDED

#include <stdint.h>
#include <assert.h>
#include <pthread.h>

/* defined in mpl_atomic.c */
extern pthread_mutex_t MPLI_emulation_lock;

#define MPLI_ATOMIC_CS_ENTER()                    \
    do {                                          \
        pthread_mutex_lock(&MPLI_emulation_lock); \
    } while (0)

#define MPLI_ATOMIC_CS_EXIT()                       \
    do {                                            \
        pthread_mutex_unlock(&MPLI_emulation_lock); \
    } while (0)


#define MPLI_ATOMIC_INITIALIZER(val_) { (val_) }

#define MPL_ATOMIC_INT_T_INITIALIZER(val_)    MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_INT32_T_INITIALIZER(val_)  MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_UINT32_T_INITIALIZER(val_) MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_INT64_T_INITIALIZER(val_)  MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_UINT64_T_INITIALIZER(val_) MPLI_ATOMIC_INITIALIZER(val_)
#define MPL_ATOMIC_PTR_T_INITIALIZER(val_)    MPLI_ATOMIC_INITIALIZER(val_)

#define MPLI_ATOMIC_DECL_FUNC_COMMON(TYPE, NAME)                               \
struct MPL_atomic_ ## NAME ## _t {                                             \
    volatile TYPE v;                                                           \
};                                                                             \
static inline TYPE MPL_atomic_relaxed_load_ ## NAME                            \
                                (const struct MPL_atomic_ ## NAME ## _t * ptr) \
{                                                                              \
    TYPE val;                                                                  \
    MPLI_ATOMIC_CS_ENTER();                                                    \
    val = (TYPE)ptr->v;                                                        \
    MPLI_ATOMIC_CS_EXIT();                                                     \
    return val;                                                                \
}                                                                              \
static inline TYPE MPL_atomic_acquire_load_ ## NAME                            \
                                (const struct MPL_atomic_ ## NAME ## _t * ptr) \
{                                                                              \
    TYPE val;                                                                  \
    MPLI_ATOMIC_CS_ENTER();                                                    \
    val = (TYPE)ptr->v;                                                        \
    MPLI_ATOMIC_CS_EXIT();                                                     \
    return val;                                                                \
}                                                                              \
static inline void MPL_atomic_relaxed_store_ ## NAME                           \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    MPLI_ATOMIC_CS_ENTER();                                                    \
    ptr->v = val;                                                              \
    MPLI_ATOMIC_CS_EXIT();                                                     \
}                                                                              \
static inline void MPL_atomic_release_store_ ## NAME                           \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    MPLI_ATOMIC_CS_ENTER();                                                    \
    ptr->v = val;                                                              \
    MPLI_ATOMIC_CS_EXIT();                                                     \
}                                                                              \
static inline TYPE MPL_atomic_cas_ ## NAME                                     \
                (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE oldv, TYPE newv) \
{                                                                              \
    TYPE prev;                                                                 \
    MPLI_ATOMIC_CS_ENTER();                                                    \
    prev = (TYPE)ptr->v;                                                       \
    if (prev == oldv)                                                          \
        ptr->v = newv;                                                         \
    MPLI_ATOMIC_CS_EXIT();                                                     \
    return prev;                                                               \
}                                                                              \
static inline TYPE MPL_atomic_swap_ ## NAME                                    \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    TYPE prev;                                                                 \
    MPLI_ATOMIC_CS_ENTER();                                                    \
    prev = (TYPE)ptr->v;                                                       \
    ptr->v = val;                                                              \
    MPLI_ATOMIC_CS_EXIT();                                                     \
    return prev;                                                               \
}

#define MPLI_ATOMIC_DECL_FUNC_FAA(TYPE, NAME)                                  \
static inline TYPE MPL_atomic_fetch_add_ ## NAME                               \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    TYPE prev;                                                                 \
    MPLI_ATOMIC_CS_ENTER();                                                    \
    prev = (TYPE)ptr->v;                                                       \
    ptr->v += val;                                                             \
    MPLI_ATOMIC_CS_EXIT();                                                     \
    return prev;                                                               \
}                                                                              \
static inline TYPE MPL_atomic_fetch_sub_ ## NAME                               \
                            (struct MPL_atomic_ ## NAME ## _t * ptr, TYPE val) \
{                                                                              \
    TYPE prev;                                                                 \
    MPLI_ATOMIC_CS_ENTER();                                                    \
    prev = (TYPE)ptr->v;                                                       \
    ptr->v -= val;                                                             \
    MPLI_ATOMIC_CS_EXIT();                                                     \
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
/* lock/unlock provides barrier */
static inline void MPL_atomic_write_barrier(void)
{
    MPLI_ATOMIC_CS_ENTER();
    MPLI_ATOMIC_CS_EXIT();
}

static inline void MPL_atomic_read_barrier(void)
{
    MPLI_ATOMIC_CS_ENTER();
    MPLI_ATOMIC_CS_EXIT();
}

static inline void MPL_atomic_read_write_barrier(void)
{
    MPLI_ATOMIC_CS_ENTER();
    MPLI_ATOMIC_CS_EXIT();
}

static inline void MPL_atomic_compiler_barrier(void)
{
    __asm__ __volatile__("":::"memory");
}

#undef MPLI_ATOMIC_CS_ENTER
#undef MPLI_ATOMIC_CS_EXIT

#endif /* MPL_ATOMIC_BY_LOCK_H_INCLUDED */
