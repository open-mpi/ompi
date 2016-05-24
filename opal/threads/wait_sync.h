/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal/sys/atomic.h"
#include "opal/threads/condition.h"
#include <pthread.h>

BEGIN_C_DECLS

typedef struct ompi_wait_sync_t {
    int32_t count;
    int32_t status;
    pthread_cond_t condition;
    pthread_mutex_t lock;
    struct ompi_wait_sync_t *next;
    struct ompi_wait_sync_t *prev;
} ompi_wait_sync_t;

#define REQUEST_PENDING        (void*)0L
#define REQUEST_COMPLETED      (void*)1L

#if OPAL_ENABLE_MULTI_THREADS

#define OPAL_ATOMIC_ADD_32(a,b)         opal_atomic_add_32(a,b)
#define OPAL_ATOMIC_SWP_PTR(a,b)        opal_atomic_swap_ptr(a,b)
#define SYNC_WAIT(sync)                 sync_wait_mt(sync)
#define PTHREAD_COND_INIT(a,b)          pthread_cond_init(a,b)
#define PTHREAD_MUTEX_INIT(a,b)         pthread_mutex_init(a,b)

#define WAIT_SYNC_RELEASE(sync)                       \
    do {                                              \
       pthread_cond_destroy(&(sync)->condition);      \
       pthread_mutex_destroy(&(sync)->lock);          \
    } while(0)

#define WAIT_SYNC_SIGNAL(sync)                        \
    do {                                              \
        pthread_mutex_lock(&(sync->lock));            \
        pthread_cond_signal(&sync->condition);        \
        pthread_mutex_unlock(&(sync->lock));          \
    } while(0)

#else

#define OPAL_ATOMIC_ADD_32(a,b)         (*(a) += (b))
#define OPAL_ATOMIC_SWP_PTR(a,b)        *(a) = (b)
#define PTHREAD_COND_INIT(a,b)
#define PTHREAD_MUTEX_INIT(a,b)
#define SYNC_WAIT(sync)                 sync_wait_st(sync)
#define WAIT_SYNC_RELEASE(sync)
#define WAIT_SYNC_SIGNAL(sync)

#endif /* OPAL_ENABLE_MULTI_THREADS */

OPAL_DECLSPEC int sync_wait_mt(ompi_wait_sync_t *sync);
OPAL_DECLSPEC int sync_wait_st(ompi_wait_sync_t *sync);

#define WAIT_SYNC_INIT(sync,c)                        \
    do {                                              \
       (sync)->count = c;                             \
       (sync)->next = NULL;                           \
       (sync)->prev = NULL;                           \
       (sync)->status = 0;                            \
       PTHREAD_COND_INIT(&(sync)->condition, NULL);   \
       PTHREAD_MUTEX_INIT(&(sync)->lock, NULL);       \
    } while(0)

static inline void wait_sync_update(ompi_wait_sync_t *sync, int updates, int req_status)
{
    if( OPAL_LIKELY(OPAL_SUCCESS == req_status) ) {
        if( 0 == (OPAL_ATOMIC_ADD_32(&sync->count, -updates)) ) {
            WAIT_SYNC_SIGNAL(sync);
        }
    } else {
        OPAL_ATOMIC_CMPSET_32(&(sync->count), 0, 0);
        sync->status = -1;
        WAIT_SYNC_SIGNAL(sync);
    }
}

END_C_DECLS
