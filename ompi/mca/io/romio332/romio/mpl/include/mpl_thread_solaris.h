/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#ifndef MPL_THREAD_SOLARIS_H_INCLUDED
#define MPL_THREAD_SOLARIS_H_INCLUDED

#include <thread.h>
#include <synch.h>

typedef mutex_t MPL_thread_mutex_t;
typedef cond_t MPL_thread_cond_t;
typedef thread_t MPL_thread_id_t;
typedef thread_key_t MPL_thread_tls_t;

typedef void (*MPL_thread_func_t) (void *data);
void MPL_thread_create(MPL_thread_func_t func, void *data, MPL_thread_id_t * id, int *err);

/*
 * Threads
 */

#define MPL_thread_exit()                       \
    do {                                        \
        thr_exit(NULL);                         \
    } while (0)

#define MPL_thread_self(id_ptr_)                \
    do {                                        \
        *(id_ptr_) = thr_self();                \
    } while (0)

#define MPL_thread_same(id1_ptr_, id2_ptr_, same_ptr_)                  \
    do {                                                                \
        *(same_ptr_) = (*(id1_ptr_) == *(id2_ptr_)) ? TRUE : FALSE;     \
    } while (0)

#define MPL_thread_yield thr_yield


/*
 *    Mutexes
 */

#define MPL_thread_mutex_create(mutex_ptr_, err_ptr_)                   \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            mutex_init(mutex_ptr_, USYNC_THREAD, NULL);                 \
        }                                                               \
        else {                                                          \
            *(err_ptr_) = mutex_init(mutex_ptr_, USYNC_THREAD, NULL);   \
            /* FIXME: convert error to an MPL_THREAD_ERR value */       \
        }                                                               \
    } while (0)

#define MPL_thread_mutex_destroy(mutex_ptr_, err_ptr_)                  \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            mutex_destroy(mutex_ptr_);                                  \
        }                                                               \
        else {                                                          \
            *(err_ptr_) = mutex_destroy(mutex_ptr_);                    \
            /* FIXME: convert error to an MPL_THREAD_ERR value */       \
        }                                                               \
    } while (0)

#define MPL_thread_mutex_lock(mutex_ptr_, err_ptr_)                     \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            mutex_lock(mutex_ptr_);                                     \
        }                                                               \
        else {                                                          \
            *(err_ptr_) = mutex_lock(mutex_ptr_);                       \
            /* FIXME: convert error to an MPL_THREAD_ERR value */       \
        }                                                               \
    } while (0)

#define MPL_thread_mutex_trylock(mutex_ptr_, err_ptr_, cs_acq_ptr)      \
    do {                                                                \
        int err__;                                                      \
        *(int*)cs_acq_ptr = 1;                                          \
        err__ = mutex_trylock(mutex_ptr_);                              \
        if (unlikely(err__ != 0 && err__ != EBUSY)) {                   \
            *(int*)cs_acq_ptr = 0;                                      \
        }                                                               \
        else {                                                          \
            if (unlikely(err__ != 0))                                   \
                *(int*)cs_acq_ptr = 0;                                  \
             err__ = 0;                                                 \
        }                                                               \
        if (err_ptr_ != NULL)                                           \
            *(int *)(err_ptr_) = err__;                                 \
    } while (0)

#define MPL_thread_mutex_unlock(mutex_ptr_, err_ptr_)                   \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            mutex_unlock(mutex_ptr_);                                   \
        }                                                               \
        else {                                                          \
            *(err_ptr_) = mutex_unlock(mutex_ptr_);                     \
            /* FIXME: convert error to an MPL_THREAD_ERR value */       \
        }                                                               \
    } while (0)


/*
 * Condition Variables
 */

#define MPL_thread_cond_create(cond_ptr_, err_ptr_)                     \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            cond_init(cond_ptr_, NULL, NULL);                           \
        }                                                               \
        else {                                                          \
            *(err_ptr_) == cond_init(cond_ptr_, NULL, NULL);            \
            /* FIXME: convert error to an MPL_THREAD_ERR value */       \
        }                                                               \
    } while (0)

#define MPL_thread_cond_destroy(cond_ptr_, err_ptr_)                    \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            cond_destroy(cond_ptr_);                                    \
        }                                                               \
        else {                                                          \
            *(err_ptr_) = cond_destroy(cond_ptr_);                      \
            /* FIXME: convert error to a MPL_THREAD_ERR value */        \
        }                                                               \
    } while (0)

#define MPL_thread_cond_wait(cond_ptr_, mutex_ptr_, err_ptr_)           \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            cond_wait((cond_ptr_), (mutex_ptr_));                       \
        }                                                               \
        else {                                                          \
            *(err_ptr_) = cond_wait((cond_ptr_), (mutex_ptr_));         \
            /* FIXME: convert error to a MPL_THREAD_ERR value */        \
        }                                                               \
    } while (0)

#define MPL_thread_cond_broadcast(cond_ptr_, err_ptr_)                  \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            cond_broadcast(cond_ptr_);                                  \
        }                                                               \
        else {                                                          \
            *(err_ptr_) = cond_broadcast(cond_ptr_);                    \
            /* FIXME: convert error to a MPL_THREAD_ERR value */        \
        }                                                               \
    } while (0)

#define MPL_thread_cond_signal(cond_ptr_, err_ptr_)                     \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            cond_signal(cond_ptr_);                                     \
        }                                                               \
        else {                                                          \
            *(err_ptr_) = cond_signal(cond_ptr_);                       \
            /* FIXME: convert error to a MPL_THREAD_ERR value */        \
        }                                                               \
    } while (0)


/*
 * Thread Local Storage
 */
#define MPL_thread_tls_create(exit_func_ptr_, tls_ptr_, err_ptr_)       \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            thr_keycreate((tls_ptr_), (exit_func_ptr_));                \
        }                                                               \
        else {                                                          \
            *(err_ptr_) = thr_keycreate((tls_ptr_), (exit_func_ptr_));  \
            /* FIXME: convert error to a MPL_THREAD_ERR value */        \
        }                                                               \
    } while (0)

#define MPL_thread_tls_destroy(tls_ptr_, err_ptr_)                      \
    do {                                                                \
        /*                                                              \
         * FIXME: Solaris threads does not have a key destroy.  We      \
         * need to create equivalent functionality to prevent a         \
         * callback from occuring when a thread exits after the TLS is  \
         * destroyed.  This is the only way to prevent subsystems that  \
         * have shutdown from continuing to receive callbacks.          \
         */                                                             \
        if ((err_ptr_) != NULL) {                                       \
            *(err_ptr_) = MPL_THREAD_SUCCESS;                           \
        }                                                               \
    } while (0)

#define MPL_thread_tls_set(tls_ptr_, value_, err_ptr_)                  \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            thr_setspecific(*(tls_ptr_), (value_));                     \
        }                                                               \
        else {                                                          \
            *(err_ptr_) = thr_setspecific(*(tls_ptr_), (value_));       \
            /* FIXME: convert error to a MPL_THREAD_ERR value */        \
        }                                                               \
    } while (0)

#define MPL_thread_tls_get(tls_ptr_, value_ptr_, err_ptr_)              \
    do {                                                                \
        if ((err_ptr_) == NULL) {                                       \
            thr_setspecific(*(tls_ptr_), (value_ptr_));                 \
        }                                                               \
        else {                                                          \
            *(err_ptr_) = thr_setspecific(*(tls_ptr_), (value_ptr_));   \
            /* FIXME: convert error to a MPL_THREAD_ERR value */        \
        }                                                               \
    } while (0)

#endif /* MPL_THREAD_SOLARIS_H_INCLUDED */
