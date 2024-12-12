/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/* This file is used when configured with (MPICH_THREAD_PACKAGE_NAME ==
 * MPICH_THREAD_PACKAGE_ARGOBOTS) */

#ifndef MPL_THREAD_ARGOBOTS_H_INCLUDED
#define MPL_THREAD_ARGOBOTS_H_INCLUDED

#include "mpl.h"
#include "abt.h"

#include <errno.h>
#include <assert.h>

typedef ABT_mutex MPL_thread_mutex_t;
typedef ABT_cond MPL_thread_cond_t;
typedef uintptr_t MPL_thread_id_t;
typedef ABT_key MPL_thread_tls_key_t;

/* ======================================================================
 *    Creation and misc
 * ======================================================================*/

/* MPL_thread_init()/MPL_thread_finalize() can be called in a nested manner
 * (e.g., MPI_T_init_thread() and MPI_Init_thread()), but Argobots internally
 * maintains a counter so it is okay. */
#define MPL_thread_init(err_ptr_)                                             \
    do {                                                                      \
        int err__;                                                            \
        err__ = ABT_init(0, NULL);                                            \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_init", err__,                  \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)

#define MPL_thread_finalize(err_ptr_)                                         \
    do {                                                                      \
        int err__;                                                            \
        err__ = ABT_finalize();                                               \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_finalize", err__,              \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                           \
    } while (0)

/* MPL_thread_create() defined in mpiu_thread_argobots.c */
typedef void (*MPL_thread_func_t) (void *data);
void MPL_thread_create(MPL_thread_func_t func, void *data, MPL_thread_id_t * idp, int *errp);

#define MPL_thread_exit()
#define MPL_thread_self(idp_)                                                 \
    do {                                                                      \
        ABT_thread self_thread_tmp_ = ABT_THREAD_NULL;                        \
        ABT_thread_self(&self_thread_tmp_);                                   \
        uintptr_t id_tmp_;                                                    \
        if (self_thread_tmp_ == ABT_THREAD_NULL) {                            \
            /* It seems that an external thread calls this function. */       \
            /* Use Pthreads ID instead. */                                    \
            id_tmp_ = (uintptr_t)pthread_self();                              \
            /* Set a bit to the last bit.                                     \
             * Note that the following shifts bits first because pthread_t    \
             * might use the last bit if, for example, pthread_t saves an ID  \
             * starting from zero; overwriting the last bit can cause a       \
             * conflict.  The last bit that is shifted out is less likely to  \
             * be significant. */                                             \
            id_tmp_ = (id_tmp_ << 1) | (uintptr_t)0x1;                        \
        } else {                                                              \
            id_tmp_ = (uintptr_t)self_thread_tmp_;                            \
            /* If ID is that of Argobots, the last bit is not set because     \
             * ABT_thread points to an aligned memory region.  Since          \
             * ABT_thread is not modified, this ID can be directly used by    \
             * MPL_thread_join().  Let's check it. */                         \
            assert(!(id_tmp_ & (uintptr_t)0x1));                              \
        }                                                                     \
        *(idp_) = id_tmp_;                                                    \
    } while (0)
#define MPL_thread_join(id_) ABT_thread_free((ABT_thread *)&(id_))
#define MPL_thread_same(idp1_, idp2_, same_)                                  \
    do {                                                                      \
        /*                                                                    \
         * TODO: strictly speaking, Pthread-Pthread and Pthread-Argobots IDs  \
         * are not arithmetically comparable, while it is okay on most        \
         * platforms.  This should be fixed.  Note that Argobots-Argobots ID  \
         * comparison is okay.                                                \
         */                                                                   \
        *(same_) = (*(idp1_) == *(idp2_)) ? TRUE : FALSE;                     \
    } while (0)

/* See mpl_thread_posix.h for interface description. */
void MPL_thread_set_affinity(MPL_thread_id_t thread, int *affinity_arr, int affinity_size,
                             int *err);

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

#define MPL_thread_mutex_lock(mutex_ptr_, err_ptr_, prio_)                    \
    do {                                                                      \
        int err__;                                                            \
        if (prio_ == MPL_THREAD_PRIO_HIGH) {                                  \
            err__ = ABT_mutex_lock(*mutex_ptr_);                              \
        } else {                                                              \
            assert(prio_ == MPL_THREAD_PRIO_LOW);                             \
            err__ = ABT_mutex_lock_low(*mutex_ptr_);                          \
        }                                                                     \
        if (unlikely(err__))                                                  \
            MPL_internal_sys_error_printf("ABT_mutex_lock", err__,            \
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

#define MPL_NO_COMPILER_TLS     /* Cannot use compiler tls with argobots */

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
