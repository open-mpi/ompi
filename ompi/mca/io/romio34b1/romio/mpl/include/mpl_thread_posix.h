/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/*
 * Threads
 */
#ifndef MPL_THREAD_POSIX_H_INCLUDED
#define MPL_THREAD_POSIX_H_INCLUDED

#include "mpl.h"        /* for MPL_sched_yield */

#include <errno.h>
#include <pthread.h>

#define MPL_POSIX_MUTEX_NATIVE 0
#define MPL_POSIX_MUTEX_TICKETLOCK 1

#if MPL_POSIX_MUTEX_NAME == MPL_POSIX_MUTEX_TICKETLOCK
#include "mpl_posix_mutex_ticketlock.h"
#else
#include "mpl_posix_mutex_native.h"
#endif

typedef pthread_t MPL_thread_id_t;
typedef pthread_key_t MPL_thread_tls_key_t;

typedef void (*MPL_thread_func_t) (void *data);
void MPL_thread_create(MPL_thread_func_t func, void *data, MPL_thread_id_t * id, int *err);

#define MPL_thread_init(err_ptr_)               \
    do {                                        \
        *(int *)(err_ptr_) = 0;                 \
    } while (0)

#define MPL_thread_finalize(err_ptr_)           \
    do {                                        \
        *(int *)(err_ptr_) = 0;                 \
    } while (0)

#define MPL_thread_exit()                       \
    do {                                        \
        pthread_exit(NULL);                     \
    } while (0)

#define MPL_thread_self(id_)                    \
    do {                                        \
        *(id_) = pthread_self();                \
    } while (0)

#define MPL_thread_join(id_)                    \
    do {                                        \
        pthread_join(id_, NULL);                \
    } while (0)

#define MPL_thread_same(id1_, id2_, same_)                              \
    do {                                                                \
        *(same_) = pthread_equal(*(id1_), *(id2_)) ? TRUE : FALSE;      \
    } while (0)

#define MPL_thread_yield MPL_sched_yield

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
        *(int *)(err_ptr_) = MPL_SUCCESS;                \
    } while (0)

#endif /* MPL_THREAD_POSIX_H_INCLUDED */
