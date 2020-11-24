/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/*
 * Interprocess Mutex
 */
#ifndef MPL_PROC_MUTEX_POSIX_H_INCLUDED
#define MPL_PROC_MUTEX_POSIX_H_INCLUDED

#include <errno.h>
#include <pthread.h>

typedef pthread_mutex_t MPL_proc_mutex_t;

#if defined(MPL_NEEDS_PTHREAD_MUTEXATTR_SETTYPE_DECL)
int pthread_mutexattr_settype(pthread_mutexattr_t * attr, int kind);
#endif /* MPL_NEEDS_PTHREAD_MUTEXATTR_SETTYPE_DECL */

#ifdef MPL_HAVE_PTHREAD_MUTEXATTR_SETPSHARED
static inline int MPL_proc_mutex_enabled(void)
{
    int mutex_err;
    pthread_mutexattr_t attr;

    /* Test for PTHREAD_PROCESS_SHARED support.  Some platforms do not support
     * this capability even though it is a part of the pthreads core API (e.g.,
     * FreeBSD does not support this as of version 9.1) */
    pthread_mutexattr_init(&attr);
    mutex_err = pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED);
    pthread_mutexattr_destroy(&attr);
    return !mutex_err;  /* if no error, return 1 */
}

#define PROC_MUTEX_EINTR_PRINT_BREAK(func_name, err__, err_ptr_) {           \
        MPL_internal_sys_error_printf(func_name, err__,                      \
                                      "    %s:%d\n", __FILE__, __LINE__);    \
        *(int *)(err_ptr_) = MPL_PROC_MUTEX_EINTR;                           \
        break; /* do not wrap by do-while (0) to break from caller's loop.*/  \
    }

#ifdef MPL_PTHREAD_MUTEX_ERRORCHECK_VALUE
#define PROC_MUTEX_SET_MUTEXATTR_SETTYPE                                                \
    do {                                                                                \
        err__ = pthread_mutexattr_settype(&attr__, MPL_PTHREAD_MUTEX_ERRORCHECK_VALUE); \
    } while (0)
#else
#define PROC_MUTEX_SET_MUTEXATTR_SETTYPE do {} while (0)
#endif /* MPL_PTHREAD_MUTEX_ERRORCHECK_VALUE */

#define MPL_proc_mutex_create(mutex_ptr_, err_ptr_)                                         \
    do {                                                                                    \
        int err__;                                                                          \
        pthread_mutexattr_t attr__;                                                         \
        *(int *)(err_ptr_) = MPL_PROC_MUTEX_SUCCESS;                                        \
                                                                                            \
        err__ = pthread_mutexattr_init(&attr__);                                            \
        if (unlikely(err__))                                                                \
            PROC_MUTEX_EINTR_PRINT_BREAK("pthread_mutexattr_init", err__, err_ptr_);        \
        err__ = pthread_mutexattr_setpshared(&attr__, PTHREAD_PROCESS_SHARED);              \
        if (unlikely(err__))                                                                \
            PROC_MUTEX_EINTR_PRINT_BREAK("pthread_mutexattr_setpshared", err__, err_ptr_);  \
        PROC_MUTEX_SET_MUTEXATTR_SETTYPE;                                                   \
        if (unlikely(err__))                                                                \
            PROC_MUTEX_EINTR_PRINT_BREAK("pthread_mutexattr_settype", err__, err_ptr_);     \
        err__ = pthread_mutex_init(mutex_ptr_, &attr__);                                    \
        if (unlikely(err__))                                                                \
            PROC_MUTEX_EINTR_PRINT_BREAK("pthread_mutex_init", err__, err_ptr_);            \
        err__ = pthread_mutexattr_destroy(&attr__);                                         \
        if (unlikely(err__))                                                                \
            PROC_MUTEX_EINTR_PRINT_BREAK("pthread_mutexattr_destroy", err__, err_ptr_);     \
    } while (0)

#define MPL_proc_mutex_destroy(mutex_ptr_, err_ptr_)                                \
    do {                                                                            \
        int err__;                                                                  \
        *(int *)(err_ptr_) = MPL_PROC_MUTEX_SUCCESS;                                \
        err__ = pthread_mutex_destroy(mutex_ptr_);                                  \
        if (unlikely(err__))                                                        \
            PROC_MUTEX_EINTR_PRINT_BREAK("pthread_mutex_destroy", err__, err_ptr_); \
    } while (0)


#define MPL_proc_mutex_lock(mutex_ptr_, err_ptr_)                                 \
    do {                                                                          \
        int err__;                                                                \
        *(int *)(err_ptr_) = MPL_PROC_MUTEX_SUCCESS;                              \
        err__ = pthread_mutex_lock(mutex_ptr_);                                   \
        if (unlikely(err__))                                                      \
            PROC_MUTEX_EINTR_PRINT_BREAK("pthread_mutex_lock", err__, err_ptr_);  \
    } while (0)


#define MPL_proc_mutex_unlock(mutex_ptr_, err_ptr_)                                 \
    do {                                                                            \
        int err__;                                                                  \
        *(int *)(err_ptr_) = MPL_PROC_MUTEX_SUCCESS;                                \
        err__ = pthread_mutex_unlock(mutex_ptr_);                                   \
        if (unlikely(err__))                                                        \
            PROC_MUTEX_EINTR_PRINT_BREAK("pthread_mutex_unlock", err__, err_ptr_);  \
    } while (0)

#else
static inline int MPL_proc_mutex_enabled(void)
{
    return 0;   /* always disabled */
}

#define MPL_proc_mutex_create(mutex_ptr_, err_ptr_)  { *((int*)err_ptr_) = MPL_PROC_MUTEX_EINVAL;}
#define MPL_proc_mutex_destroy(mutex_ptr_, err_ptr_) { *((int*)err_ptr_) = MPL_PROC_MUTEX_EINVAL;}
#define MPL_proc_mutex_lock(mutex_ptr_, err_ptr_)  { *((int*)err_ptr_) = MPL_PROC_MUTEX_EINVAL;}
#define MPL_proc_mutex_unlock(mutex_ptr_, err_ptr_) { *((int*)err_ptr_) = MPL_PROC_MUTEX_EINVAL;}
#endif /* MPL_HAVE_PTHREAD_MUTEXATTR_SETPSHARED */

#endif /* MPL_PROC_MUTEX_POSIX_H_INCLUDED */
