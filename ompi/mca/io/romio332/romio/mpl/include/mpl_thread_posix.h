/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/*
 * Threads
 */
#ifndef MPL_THREAD_POSIX_H_INCLUDED
#define MPL_THREAD_POSIX_H_INCLUDED

#include "mpl.h"        /* for MPL_sched_yield */

#include <errno.h>
#include <pthread.h>

typedef pthread_mutex_t MPL_thread_mutex_t;
typedef pthread_cond_t MPL_thread_cond_t;
typedef pthread_t MPL_thread_id_t;
typedef pthread_key_t MPL_thread_tls_t;

#if defined(MPL_NEEDS_PTHREAD_MUTEXATTR_SETTYPE_DECL)
int pthread_mutexattr_settype(pthread_mutexattr_t * attr, int kind);
#endif /* MPL_NEEDS_PTHREAD_MUTEXATTR_SETTYPE_DECL */

typedef void (*MPL_thread_func_t) (void *data);
void MPL_thread_create(MPL_thread_func_t func, void *data, MPL_thread_id_t * id, int *err);

#define MPL_thread_exit()                       \
    do {                                        \
        pthread_exit(NULL);                     \
    } while (0)

#define MPL_thread_self(id_)                    \
    do {                                        \
        *(id_) = pthread_self();                \
    } while (0)

#define MPL_thread_same(id1_, id2_, same_)                              \
    do {                                                                \
        *(same_) = pthread_equal(*(id1_), *(id2_)) ? TRUE : FALSE;      \
    } while (0)

#define MPL_thread_yield MPL_sched_yield


/*
 *    Mutexes
 */

/* FIXME: mutex creation and destruction should be implemented as routines
   because there is no reason to use macros (these are not on the performance
   critical path).  Making these macros requires that any code that might use
   these must load all of the pthread.h (or other thread library) support.
 */

/* FIXME: using constant initializer if available */

/* FIXME: convert errors to an MPL_THREAD_ERR value */

#if !defined(MPL_PTHREAD_MUTEX_ERRORCHECK_VALUE)

#define MPL_thread_mutex_create(mutex_ptr_, err_ptr_)                   \
    do {                                                                \
        int err__;                                                      \
                                                                        \
        err__ = pthread_mutex_init(mutex_ptr_, NULL);                   \
        if (unlikely(err__))                                            \
            MPL_internal_sys_error_printf("pthread_mutex_init", err__,  \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#else /* defined(MPL_PTHREAD_MUTEX_ERRORCHECK_VALUE) */

#define MPL_thread_mutex_create(mutex_ptr_, err_ptr_)                   \
    do {                                                                \
        int err__;                                                      \
        pthread_mutexattr_t attr__;                                     \
                                                                        \
        pthread_mutexattr_init(&attr__);                                \
        pthread_mutexattr_settype(&attr__, MPL_PTHREAD_MUTEX_ERRORCHECK_VALUE); \
        err__ = pthread_mutex_init(mutex_ptr_, &attr__);                \
        if (unlikely(err__))                                            \
            MPL_internal_sys_error_printf("pthread_mutex_init", err__,  \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#endif /* defined(MPL_PTHREAD_MUTEX_ERRORCHECK_VALUE) */

#define MPL_thread_mutex_destroy(mutex_ptr_, err_ptr_)                  \
    do {                                                                \
        int err__;                                                      \
                                                                        \
        err__ = pthread_mutex_destroy(mutex_ptr_);                      \
        if (unlikely(err__))                                            \
            MPL_internal_sys_error_printf("pthread_mutex_destroy", err__, \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)


#define MPL_thread_mutex_lock(mutex_ptr_, err_ptr_)                     \
    do {                                                                \
        int err__;                                                      \
        err__ = pthread_mutex_lock(mutex_ptr_);                         \
        if (unlikely(err__)) {                                          \
            MPL_internal_sys_error_printf("pthread_mutex_lock", err__,  \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        }                                                               \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#define MPL_thread_mutex_trylock(mutex_ptr_, err_ptr_, cs_acq_ptr)      \
    do {                                                                \
        int err__;                                                      \
        *(int*)cs_acq_ptr = 1;                                          \
        err__ = pthread_mutex_trylock(mutex_ptr_);                      \
        if (unlikely(err__ != 0 && err__ != EBUSY)) {                   \
            *(int*)cs_acq_ptr = 0;                                      \
            MPL_internal_sys_error_printf("pthread_mutex_trylock", err__,  \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        }                                                               \
        else {                                                          \
            if (unlikely(err__ != 0))                                   \
                *(int*)cs_acq_ptr = 0;                                  \
             err__ = 0;                                                 \
        }                                                               \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#define MPL_thread_mutex_unlock(mutex_ptr_, err_ptr_)                   \
    do {                                                                \
        int err__;                                                      \
                                                                        \
        err__ = pthread_mutex_unlock(mutex_ptr_);                       \
        if (unlikely(err__)) {                                          \
            MPL_internal_sys_error_printf("pthread_mutex_unlock", err__, \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        }                                                               \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)


/*
 * Condition Variables
 */

#define MPL_thread_cond_create(cond_ptr_, err_ptr_)                     \
    do {                                                                \
        int err__;                                                      \
                                                                        \
        err__ = pthread_cond_init((cond_ptr_), NULL);                   \
        if (unlikely(err__))                                            \
            MPL_internal_sys_error_printf("pthread_cond_init", err__,   \
                                          "    %s:%d\n", __FILE__, __LINE__); \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#define MPL_thread_cond_destroy(cond_ptr_, err_ptr_)                    \
    do {                                                                \
        int err__;                                                      \
                                                                        \
        err__ = pthread_cond_destroy(cond_ptr_);                        \
        if (unlikely(err__))                                            \
            MPL_internal_sys_error_printf("pthread_cond_destroy", err__, \
            "    %s:%d\n", __FILE__, __LINE__);                         \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#define MPL_thread_cond_wait(cond_ptr_, mutex_ptr_, err_ptr_)           \
    do {                                                                \
        int err__;                                                      \
                                                                        \
        /* The latest pthread specification says that cond_wait         \
         * routines aren't allowed to return EINTR, but some of the     \
         * older implementations still do. */                           \
        do {                                                            \
            err__ = pthread_cond_wait((cond_ptr_), mutex_ptr_);         \
        } while (err__ == EINTR);                                       \
        if (unlikely(err__))                                            \
            MPL_internal_sys_error_printf("pthread_cond_wait", err__,   \
                                          "    %s:%d\n", __FILE__, __LINE__); \
                                                                        \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#define MPL_thread_cond_broadcast(cond_ptr_, err_ptr_)                  \
    do {                                                                \
        int err__;                                                      \
                                                                        \
        err__ = pthread_cond_broadcast(cond_ptr_);                      \
        if (unlikely(err__))                                            \
            MPL_internal_sys_error_printf("pthread_cond_broadcast", err__, \
                                          "    %s:%d\n", __FILE__, __LINE__); \
                                                                        \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#define MPL_thread_cond_signal(cond_ptr_, err_ptr_)                     \
    do {                                                                \
        int err__;                                                      \
                                                                        \
        err__ = pthread_cond_signal(cond_ptr_);                         \
        if (unlikely(err__))                                            \
            MPL_internal_sys_error_printf("pthread_cond_signal", err__, \
                                          "    %s:%d\n", __FILE__, __LINE__); \
                                                                        \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)


/*
 * Thread Local Storage
 */

#define MPL_thread_tls_create(exit_func_ptr_, tls_ptr_, err_ptr_)       \
    do {                                                                \
        int err__;                                                      \
                                                                        \
        err__ = pthread_key_create((tls_ptr_), (exit_func_ptr_));       \
        if (unlikely(err__))                                            \
            MPL_internal_sys_error_printf("pthread_key_create", err__,  \
                                          "    %s:%d\n", __FILE__, __LINE__); \
                                                                        \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#define MPL_thread_tls_destroy(tls_ptr_, err_ptr_)                      \
    do {                                                                \
        int err__;                                                      \
                                                                        \
        err__ = pthread_key_delete(*(tls_ptr_));                        \
        if (unlikely(err__))                                            \
            MPL_internal_sys_error_printf("pthread_key_delete", err__,  \
                                          "    %s:%d\n", __FILE__, __LINE__); \
                                                                        \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#define MPL_thread_tls_set(tls_ptr_, value_, err_ptr_)                  \
    do {                                                                \
        int err__;                                                      \
                                                                        \
        err__ = pthread_setspecific(*(tls_ptr_), (value_));             \
        if (unlikely(err__))                                            \
            MPL_internal_sys_error_printf("pthread_setspecific", err__, \
                                          "    %s:%d\n", __FILE__, __LINE__); \
                                                                        \
        *(int *)(err_ptr_) = err__;                                     \
    } while (0)

#define MPL_thread_tls_get(tls_ptr_, value_ptr_, err_ptr_)      \
    do {                                                        \
        *(value_ptr_) = pthread_getspecific(*(tls_ptr_));       \
                                                                \
        *(int *)(err_ptr_) = MPL_THREAD_SUCCESS;                \
    } while (0)

#endif /* MPL_THREAD_POSIX_H_INCLUDED */
