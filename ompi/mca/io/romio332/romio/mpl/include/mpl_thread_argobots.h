/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2017 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/* This file is used when configured with (MPICH_THREAD_PACKAGE_NAME ==
 * MPICH_THREAD_PACKAGE_ARGOBOTS) */

#ifndef MPL_THREAD_ARGOBOTS_H_INCLUDED
#define MPL_THREAD_ARGOBOTS_H_INCLUDED

#include "mpl.h"
#include "abt.h"

#include <errno.h>

typedef ABT_mutex MPL_thread_mutex_t;
typedef ABT_cond MPL_thread_cond_t;
typedef ABT_thread_id MPL_thread_id_t;
typedef ABT_key MPL_thread_tls_t;

/* ======================================================================
 *    Creation and misc
 * ======================================================================*/

/* MPL_thread_create() defined in mpiu_thread_argobots.c */
typedef void (*MPL_thread_func_t) (void *data);
void MPL_thread_create(MPL_thread_func_t func, void *data, MPL_thread_id_t * idp, int *errp);

#define MPL_thread_exit()
#define MPL_thread_self(id_) ABT_thread_self_id(id_)
#define MPL_thread_same(id1_, id2_, same_)  ABT_thread_equal(id1_, id2_, same_)

/* ======================================================================
 *    Scheduling
 * ======================================================================*/

#define MPL_thread_yield ABT_thread_yield

/* ======================================================================
 *    Mutexes
 * ======================================================================*/
#define MPL_thread_mutex_create(mutex_ptr_, err_ptr_)                         \
    do {                                                                      \
        int err__;                                                            \
        err__ = ABT_mutex_create(mutex_ptr_);                                 \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_mutex_create", err__,          \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)

#define MPL_thread_mutex_destroy(mutex_ptr_, err_ptr_)                        \
    do {                                                                      \
        int err__;                                                            \
        err__ = ABT_mutex_free(mutex_ptr_);                                   \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_mutex_free", err__,            \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)

#define MPL_thread_mutex_lock(mutex_ptr_, err_ptr_)                           \
    do {                                                                      \
        int err__;                                                            \
        err__ = ABT_mutex_lock(*mutex_ptr_);                                  \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_mutex_lock", err__,            \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)

#define MPL_thread_mutex_trylock(mutex_ptr_, err_ptr_, cs_acq_ptr)      \
    do {                                                                \
        int err__;                                                      \
        *(int*)cs_acq_ptr = 1;                                          \
        err__ = ABT_mutex_trylock(mutex_ptr_);                          \
        if (unlikely(err__ != ABT_SUCCESS && err__ != ABT_ERR_MUTEX_LOCKED)) { \
            *(int*)cs_acq_ptr = 0;                                      \
            MPL_internal_sys_error_printf("ABT_mutex_trylock", err__,   \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        }                                                               \
        else {                                                          \
            if (unlikely(err__ != 0))                                   \
                *(int*)cs_acq_ptr = 0;                                  \
             err__ = 0;                                                 \
        }                                                               \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#define MPL_thread_mutex_lock_low(mutex_ptr_, err_ptr_)                       \
    do {                                                                      \
        int err__;                                                            \
        err__ = ABT_mutex_lock_low(*mutex_ptr_);                              \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_mutex_lock_low", err__,        \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)


#define MPL_thread_mutex_unlock(mutex_ptr_, err_ptr_)                         \
    do {                                                                      \
        int err__;                                                            \
        err__ = ABT_mutex_unlock(*mutex_ptr_);                                \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_mutex_unlock", err__,          \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)

#define MPL_thread_mutex_unlock_se(mutex_ptr_, err_ptr_)                      \
    do {                                                                      \
        int err__;                                                            \
        err__ = ABT_mutex_unlock_se(*mutex_ptr_);                             \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_mutex_unlock_se", err__,       \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)

/* ======================================================================
 *    Condition Variables
 * ======================================================================*/

#define MPL_thread_cond_create(cond_ptr_, err_ptr_)                           \
    do {                                                                      \
        int err__;                                                            \
        err__ = ABT_cond_create((cond_ptr_));                                 \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_cond_create", err__,           \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)

#define MPL_thread_cond_destroy(cond_ptr_, err_ptr_)                          \
    do {                                                                      \
        int err__;                                                            \
        err__ = ABT_cond_free(cond_ptr_);                                     \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_cond_free", err__,             \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)

#define MPL_thread_cond_wait(cond_ptr_, mutex_ptr_, err_ptr_)                   \
    do {                                                                        \
        int err__;                                                              \
        MPL_DBG_MSG_FMT(THREAD,TYPICAL,                                         \
                        (MPL_DBG_FDEST,                                         \
                         "Enter cond_wait on cond=%p mutex=%p",                 \
                         (cond_ptr_),(mutex_ptr_)));                            \
        do {                                                                    \
            err__ = ABT_cond_wait((*cond_ptr_), *mutex_ptr_);                   \
        } while (err__ == EINTR);                                               \
        *(int *)(err_ptr_) = err__;                                             \
        if (unlikely(err__))                                                    \
            MPL_internal_sys_error_printf("ABT_cond_free", err__,                 \
                   "    %s:%d error in cond_wait on cond=%p mutex=%p err__=%d", \
                   __FILE__, __LINE__);       \
        MPL_DBG_MSG_FMT(THREAD,TYPICAL,(MPL_DBG_FDEST,                          \
                                        "Exit cond_wait on cond=%p mutex=%p",   \
                                        (cond_ptr_),(mutex_ptr_)));             \
    } while (0)

#define MPL_thread_cond_broadcast(cond_ptr_, err_ptr_)                        \
    do {                                                                      \
        int err__;                                                            \
        MPL_DBG_MSG_P(THREAD,TYPICAL,                                         \
                      "About to cond_broadcast on MPL_thread_cond %p",        \
                      (cond_ptr_));                                           \
        err__ = ABT_cond_broadcast((*cond_ptr_));                             \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_cond_broadcast", err__,        \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)

#define MPL_thread_cond_signal(cond_ptr_, err_ptr_)                           \
    do {                                                                      \
        int err__;                                                            \
        MPL_DBG_MSG_P(THREAD,TYPICAL,                                         \
                      "About to cond_signal on MPL_thread_cond %p",           \
                      (cond_ptr_));                                           \
        err__ = ABT_cond_signal((*cond_ptr_));                                \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_cond_signal", err__,           \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)

/* ======================================================================
 *    Thread Local Storage
 * ======================================================================*/

#define MPL_thread_tls_create(exit_func_ptr_, tls_ptr_, err_ptr_)         \
    do {                                                                  \
        int err__;                                                        \
        err__ = ABT_key_create((exit_func_ptr_), (tls_ptr_));             \
        if (unlikely(err__))                                              \
        MPL_internal_sys_error_printf("ABT_key_create", err__,            \
                                      "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = 0;                                           \
    } while (0)

#define MPL_thread_tls_destroy(tls_ptr_, err_ptr_)                        \
    do {                                                                  \
        int err__;                                                        \
        err__ = ABT_key_free(tls_ptr_);                                   \
        if (unlikely(err__))                                              \
        MPL_internal_sys_error_printf("ABT_key_free", err__,              \
                                      "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                       \
    } while (0)

#define MPL_thread_tls_set(tls_ptr_, value_, err_ptr_)                    \
    do {                                                                  \
        int err__;                                                        \
        err__ = ABT_key_set(*(tls_ptr_), (value_));                       \
        if (unlikely(err__))                                              \
        MPL_internal_sys_error_printf("ABT_key_set", err__,               \
                                      "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                       \
    } while (0)

#define MPL_thread_tls_get(tls_ptr_, value_ptr_, err_ptr_)                \
    do {                                                                  \
        int err__;                                                        \
        err__ = ABT_key_get(*(tls_ptr_), (value_ptr_));                   \
        if (unlikely(err__))                                              \
        MPL_internal_sys_error_printf("ABT_key_get", err__,               \
                                      "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                       \
    } while (0)

#endif /* MPL_THREAD_ARGOBOTS_H_INCLUDED */
