/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_POSIX_MUTEX_NATIVE_H_INCLUDED
#define MPL_POSIX_MUTEX_NATIVE_H_INCLUDED

typedef pthread_mutex_t MPL_thread_mutex_t;
typedef pthread_cond_t MPL_thread_cond_t;

#if defined(MPL_NEEDS_PTHREAD_MUTEXATTR_SETTYPE_DECL)
int pthread_mutexattr_settype(pthread_mutexattr_t * attr, int kind);
#endif /* MPL_NEEDS_PTHREAD_MUTEXATTR_SETTYPE_DECL */

/* FIXME: mutex creation and destruction should be implemented as routines
   because there is no reason to use macros (these are not on the performance
   critical path).  Making these macros requires that any code that might use
   these must load all of the pthread.h (or other thread library) support.
 */

/* FIXME: using constant initializer if available */

/* FIXME: convert errors to an MPL_ERR_THREAD value */

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


#define MPL_thread_mutex_lock(mutex_ptr_, err_ptr_, prio_)              \
    do {                                                                \
        int err__;                                                      \
        err__ = pthread_mutex_lock(mutex_ptr_);                         \
        if (unlikely(err__)) {                                          \
            MPL_internal_sys_error_printf("pthread_mutex_lock", err__,  \
                                          "    %s:%d\n", __FILE__, __LINE__); \
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


#endif /* MPL_POSIX_MUTEX_NATIVE_H_INCLUDED */
