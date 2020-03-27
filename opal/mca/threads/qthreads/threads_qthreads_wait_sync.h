/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#ifndef OPAL_MCA_THREADS_QTHREADS_THREADS_QTHREADS_WAIT_SYNC_H
#define OPAL_MCA_THREADS_QTHREADS_THREADS_QTHREADS_WAIT_SYNC_H 1

#include "opal/mca/threads/qthreads/threads_qthreads.h"
#include <abt.h>

typedef struct ompi_wait_sync_t {
    opal_atomic_int32_t count;
    int32_t status;
    ABT_cond condition;
    ABT_mutex lock;
    struct ompi_wait_sync_t *next;
    struct ompi_wait_sync_t *prev;
    volatile bool signaling;
} ompi_wait_sync_t;

#define SYNC_WAIT(sync) (opal_using_threads() ? ompi_sync_wait_mt (sync) : sync_wait_st (sync))

/* The loop in release handles a race condition between the signaling
 * thread and the destruction of the condition variable. The signaling
 * member will be set to false after the final signaling thread has
 * finished operating on the sync object. This is done to avoid
 * extra atomics in the signalling function and keep it as fast
 * as possible. Note that the race window is small so spinning here
 * is more optimal than sleeping since this macro is called in
 * the critical path. */
#define WAIT_SYNC_RELEASE(sync)        do { } while (0)
#define WAIT_SYNC_RELEASE_NOWAIT(sync) do { } while (0)
#define WAIT_SYNC_SIGNAL(sync)         do { } while (0)
#define WAIT_SYNC_SIGNALLED(sync)      do { } while (0)

OPAL_DECLSPEC int ompi_sync_wait_mt(ompi_wait_sync_t *sync);
static inline int sync_wait_st (ompi_wait_sync_t *sync)
{
    return sync->status;
}

#define WAIT_SYNC_INIT(sync,c)         do { } while (0)

#endif /* OPAL_MCA_THREADS_QTHREADS_THREADS_QTHREADS_WAIT_SYNC_H */
