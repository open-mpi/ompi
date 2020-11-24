/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_THREAD_PRIV_H_INCLUDED
#define MPL_THREAD_PRIV_H_INCLUDED

#if MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE && !defined(MPL_TLS)
/* We need to provide a function that will cleanup the storage attached
 * to the key.  */
void MPLI_cleanup_tls(void *a);

/* In the case where the thread level is set in MPI_Init_thread, we
   need a blended version of the non-threaded and the thread-multiple
   definitions.

   The approach is to have TWO MPLI_per_thread_t pointers.  One is local
   (The MPL_THREADPRIV_DECL is used in the routines local definitions),
   as in the threaded version of these macros.  This is set by using a routine
   to get thread-private storage.  The second is a preallocated, extern
   MPLI_per_thread_t struct, as in the single threaded case.  Based on
   whether MPL is initialized with thread safety, one or the other is used.
 */

#define MPL_THREADPRIV_KEY_CREATE(key, var, err_ptr_, class_)           \
    do {                                                                \
        void *thread_ptr;                                               \
                                                                        \
        MPL_thread_tls_create(MPLI_cleanup_tls, &(key) , err_ptr_);     \
        if (unlikely(*((int *) err_ptr_)))                              \
            break;                                                      \
        thread_ptr = MPL_calloc(1, sizeof(var), class_);                \
        if (unlikely(!thread_ptr)) {                                    \
            *((int *) err_ptr_) = MPL_THREAD_ERROR;                     \
            break;                                                      \
        }                                                               \
        MPL_thread_tls_set(&(key), thread_ptr, err_ptr_);               \
    } while (0)

#define MPL_THREADPRIV_KEY_GET_ADDR(is_threaded, key, var, addr, err_ptr_)  \
    do {                                                                \
        if (is_threaded) {                                              \
            void *thread_ptr;                                           \
            MPL_thread_tls_get(&(key), &thread_ptr, err_ptr_);          \
            if (unlikely(*((int *) err_ptr_)))                          \
                break;                                                  \
            if (!thread_ptr) {                                          \
                thread_ptr = MPL_calloc(1, sizeof(var), MPL_MEM_OTHER); \
                if (unlikely(!thread_ptr)) {                            \
                    *((int *) err_ptr_) = MPL_THREAD_ERROR;             \
                    break;                                              \
                }                                                       \
                MPL_thread_tls_set(&(key), thread_ptr, err_ptr_);       \
                if (unlikely(*((int *) err_ptr_)))                      \
                    break;                                              \
            }                                                           \
            addr = thread_ptr;                                          \
        }                                                               \
        else {                                                          \
            addr = &(var);                                              \
        }                                                               \
    } while (0)

#define MPL_THREADPRIV_KEY_DESTROY(key, err_ptr_)       \
    do {                                                \
        void *thread_ptr;                               \
                                                        \
        MPL_thread_tls_get(&(key), &thread_ptr, err_ptr_); \
        if (unlikely(*((int *) err_ptr_)))                 \
            break;                                      \
                                                        \
        if (thread_ptr)                                 \
            MPL_free(thread_ptr);                       \
                                                        \
        MPL_thread_tls_set(&(key), NULL, err_ptr_);     \
        if (unlikely(*((int *) err_ptr_)))              \
            break;                                      \
                                                        \
        MPL_thread_tls_destroy(&(key), err_ptr_);       \
    } while (0)

#else /* MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE || defined(MPL_TLS) */

/* We have proper thread-local storage (TLS) support from the compiler, which
 * should yield the best performance and simplest code, so we'll use that. */
#define MPL_THREADPRIV_KEY_CREATE(...)
#define MPL_THREADPRIV_KEY_GET_ADDR(is_threaded, key, var, addr, err_ptr_) \
    do {                                                                \
        addr = &(var);                                                  \
        *((int *) err_ptr_) = MPL_THREAD_SUCCESS;                       \
    } while (0)
#define MPL_THREADPRIV_KEY_DESTROY(...)

#endif

#endif /* MPL_THREAD_PRIV_H_INCLUDED */
