/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OPAL_MCA_THREADS_WAIT_SYNC_H)
#define OPAL_MCA_THREADS_WAIT_SYNC_H

#include "opal/sys/atomic.h"
#include "opal/threads/condition.h"

BEGIN_C_DECLS

extern int opal_max_thread_in_progress;

#include MCA_wait_sync_IMPLEMENTATION_HEADER

#define REQUEST_PENDING        (void*)0L
#define REQUEST_COMPLETED      (void*)1L

/**
 * Update the status of the synchronization primitive. If an error is
 * reported the synchronization is completed and the signal
 * triggered. The status of the synchronization will be reported to
 * the waiting threads.
 */
static inline void wait_sync_update(ompi_wait_sync_t *sync, int updates, int status)
{
    if( OPAL_LIKELY(OPAL_SUCCESS == status) ) {
        if( 0 != (OPAL_THREAD_ADD_FETCH32(&sync->count, -updates)) ) {
            return;
        }
    } else {
        /* this is an error path so just use the atomic */
        sync->status = OPAL_ERROR;
        opal_atomic_wmb ();
        opal_atomic_swap_32 (&sync->count, 0);
    }
    WAIT_SYNC_SIGNAL(sync);
}

END_C_DECLS

#endif /* defined(OPAL_MCA_THREADS_WAIT_SYNC_H) */
