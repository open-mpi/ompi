/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_POSIX_MUTEX_TICKETLOCK_H_INCLUDED
#define MPL_POSIX_MUTEX_TICKETLOCK_H_INCLUDED

#include "mpl_atomic.h"

#ifdef MPL_HAVE_SCHED_H
#include <sched.h>
#define MPLI_TICKETLOCK_YIELD() sched_yield()
#else
#define MPLI_TICKETLOCK_YIELD() do { } while (0)
#endif

/* Ticket lock implementation */

typedef struct {
    MPL_atomic_int_t num;
    MPL_atomic_int_t next;
} MPLI_ticket_lock;

#define MPLI_ticket_lock_init(lock_) \
    do { \
        MPL_atomic_store_int(&(lock_)->num, 0); \
        MPL_atomic_store_int(&(lock_)->next, 0); \
    } while (0)

#define MPLI_ticket_lock_lock(lock_) \
    do { \
        int ticket = MPL_atomic_fetch_add_int(&(lock_)->num, 1); \
        while (MPL_atomic_load_int(&(lock_)->next) != ticket) { \
	    MPLI_TICKETLOCK_YIELD(); \
        } \
    } while (0)

#define MPLI_ticket_lock_unlock(lock_) \
    do { \
        MPL_atomic_fetch_add_int(&(lock_)->next, 1); \
    } while (0)

/* MPL wrappers */

typedef MPLI_ticket_lock MPL_thread_mutex_t;
typedef int MPL_thread_cond_t;

#define MPL_thread_mutex_create(mutex_ptr_, err_ptr_) \
    do { \
        MPLI_ticket_lock_init(mutex_ptr_); \
        *(int *)(err_ptr_) = 0; \
    } while (0)

#define MPL_thread_mutex_destroy(mutex_ptr_, err_ptr_) \
    do { \
        *(int *)(err_ptr_) = 0; \
    } while (0)

#define MPL_thread_mutex_lock(mutex_ptr_, err_ptr_, prio_) \
    do { \
        MPLI_ticket_lock_lock(mutex_ptr_); \
        *(int *)(err_ptr_) = 0; \
    } while (0)

#define MPL_thread_mutex_unlock(mutex_ptr_, err_ptr_) \
    do { \
        MPLI_ticket_lock_unlock(mutex_ptr_); \
        *(int *)(err_ptr_) = 0; \
    } while (0)

/* No support for condition variable */
#define MPL_thread_cond_create(cond_ptr_, err_ptr_)             assert(0)
#define MPL_thread_cond_destroy(cond_ptr_, err_ptr_)            assert(0)
#define MPL_thread_cond_wait(cond_ptr_, mutex_ptr_, err_ptr_)   assert(0)
#define MPL_thread_cond_broadcast(cond_ptr_, err_ptr_)          assert(0)
#define MPL_thread_cond_signal(cond_ptr_, err_ptr_)             assert(0)

#endif /* MPL_POSIX_MUTEX_TICKETLOCK_H_INCLUDED */
