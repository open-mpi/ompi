/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2012-2013 Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_rdma.h"
#include "osc_rdma_header.h"
#include "osc_rdma_data_move.h"
#include "osc_rdma_frag.h"

#include "mpi.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/base/base.h"
#include "opal/include/opal_stdint.h"

/* target-side tracking of a lock request */
struct ompi_osc_rdma_pending_lock_t {
    opal_list_item_t super;
    int peer;
    int lock_type;
    uint64_t serial_number;
};
typedef struct ompi_osc_rdma_pending_lock_t ompi_osc_rdma_pending_lock_t;
OBJ_CLASS_INSTANCE(ompi_osc_rdma_pending_lock_t, opal_list_item_t,
                   NULL, NULL);


/* origin-side tracking of a lock request */
struct ompi_osc_rdma_outstanding_lock_t {
    opal_list_item_t super;
    int target;
    int32_t lock_acks_received;
    int32_t unlock_acks_received;
    int32_t flush_acks_received;
    uint64_t serial_number;
    int32_t type;
};
typedef struct ompi_osc_rdma_outstanding_lock_t ompi_osc_rdma_outstanding_lock_t;
OBJ_CLASS_INSTANCE(ompi_osc_rdma_outstanding_lock_t, opal_list_item_t,
                   NULL, NULL);

static int ompi_osc_activate_next_lock (ompi_osc_rdma_module_t *module);
static inline int queue_lock (ompi_osc_rdma_module_t *module, int requestor,
                              int lock_type, uint64_t serial_number);

/**
 * Find the first outstanding lock to a target.
 *
 * @param[in] module   - OSC RDMA module
 * @param[in] target   - Target rank
 *
 * @returns an outstanding lock on success
 *
 * This function traverses the outstanding_locks list in the module
 * looking for a lock that matches target. The caller must hold the
 * module lock.
 */
static inline ompi_osc_rdma_outstanding_lock_t *find_outstanding_lock (ompi_osc_rdma_module_t *module, int target)
{
    ompi_osc_rdma_outstanding_lock_t *lock;

    OPAL_LIST_FOREACH(lock, &module->outstanding_locks, ompi_osc_rdma_outstanding_lock_t) {
        if (lock->target == target) {
            return lock;
        }
    }

    return NULL;
}

static inline ompi_osc_rdma_outstanding_lock_t *find_outstanding_lock_by_serial (ompi_osc_rdma_module_t *module, uint64_t serial_number)
{
    ompi_osc_rdma_outstanding_lock_t *lock;

    OPAL_LIST_FOREACH(lock, &module->outstanding_locks, ompi_osc_rdma_outstanding_lock_t) {
        if (lock->serial_number == serial_number) {
            return lock;
        }
    }

    return NULL;
}

static inline int ompi_osc_rdma_lock_self (ompi_osc_rdma_module_t *module, ompi_osc_rdma_outstanding_lock_t *lock)
{
    const int my_rank = ompi_comm_rank (module->comm);

    if ((MPI_LOCK_SHARED == lock->type && MPI_LOCK_EXCLUSIVE != module->lock_status) ||
        (MPI_LOCK_EXCLUSIVE == lock->type && 0 == module->lock_status)) {
        /* we can aquire the lock immediately */
        module->lock_status = lock->type;
        if (MPI_LOCK_SHARED == lock->type) {
            module->shared_count++;
        }

        lock->lock_acks_received++;
    } else {
        /* queue the lock */
        queue_lock (module, my_rank, lock->type, lock->serial_number);
    }

    /* If locking local, can't be non-blocking according to the
       standard.  We need to wait for the ack here. */
    while (0 == lock->lock_acks_received) {
        opal_condition_wait(&module->cond, &module->lock);
    }

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "local lock aquired"));

    return OMPI_SUCCESS;
}

static inline void ompi_osc_rdma_unlock_self (ompi_osc_rdma_module_t *module, ompi_osc_rdma_outstanding_lock_t *lock)
{
    if (!(MPI_LOCK_SHARED == lock->type && 0 == --module->shared_count)) {
        module->lock_status = 0;
        ompi_osc_activate_next_lock (module);
    }

    /* need to ensure we make progress */
    opal_progress();

    lock->unlock_acks_received++;
}

static inline int ompi_osc_rdma_lock_remote (ompi_osc_rdma_module_t *module, int target, ompi_osc_rdma_outstanding_lock_t *lock)
{
    ompi_osc_rdma_header_lock_t lock_req;
    int ret;

    /* generate a lock request */
    lock_req.base.type = OMPI_OSC_RDMA_HDR_TYPE_LOCK_REQ;
    lock_req.base.flags = OMPI_OSC_RDMA_HDR_FLAG_VALID | OMPI_OSC_RDMA_HDR_FLAG_PASSIVE_TARGET;
    lock_req.lock_type = lock->type;
    lock_req.serial_number = lock->serial_number;

    ret = ompi_osc_rdma_control_send (module, target, &lock_req, sizeof (lock_req));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    /* make sure the request gets sent, so we can start eager sending... */
    ret = ompi_osc_rdma_frag_flush_target (module, target);

    return ret;
}

static inline int ompi_osc_rdma_unlock_remote (ompi_osc_rdma_module_t *module, int target, ompi_osc_rdma_outstanding_lock_t *lock)
{
    ompi_osc_rdma_header_unlock_t unlock_req;

    unlock_req.base.type = OMPI_OSC_RDMA_HDR_TYPE_UNLOCK_REQ;
    unlock_req.base.flags = OMPI_OSC_RDMA_HDR_FLAG_VALID | OMPI_OSC_RDMA_HDR_FLAG_PASSIVE_TARGET;
    unlock_req.frag_count = module->epoch_outgoing_frag_count[target];
    unlock_req.lock_type = lock->type;

    /* send control message with unlock request and count */
    return ompi_osc_rdma_control_send (module, target, &unlock_req, sizeof (unlock_req));
}



int ompi_osc_rdma_lock(int lock_type, int target, int assert, ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_outstanding_lock_t *lock;
    ompi_osc_rdma_peer_t *peer = module->peers + target;
    int ret = OMPI_SUCCESS;

    /* Check if no_locks is set. TODO: we also need to track whether we are in an
     * active target epoch. Fence can make this tricky to track. */
    if (NULL == module->passive_eager_send_active || module->sc_group) {
        return OMPI_ERR_RMA_SYNC;
    }

    assert(module->epoch_outgoing_frag_count[target] == 0);

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "osc rdma: lock %d %d", target, lock_type));

    /* delay all eager sends until we've heard back.. */
    OPAL_THREAD_LOCK(&module->lock);
    module->passive_eager_send_active[target] = false;
    module->passive_target_access_epoch = true;

    /* when the lock ack returns we will be in an access epoch with this peer */
    peer->access_epoch = true;

    /* create lock item */
    lock = OBJ_NEW(ompi_osc_rdma_outstanding_lock_t);
    if (OPAL_UNLIKELY(NULL == lock)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    lock->target = target;
    lock->lock_acks_received = 0;
    lock->unlock_acks_received = 0;
    lock->serial_number = module->lock_serial_number++;
    lock->type = lock_type;
    opal_list_append(&module->outstanding_locks, &lock->super);

    if (0 == (assert & MPI_MODE_NOCHECK)) {
        if (ompi_comm_rank (module->comm) != target) {
            ret = ompi_osc_rdma_lock_remote (module, target, lock);
        } else {
            ret = ompi_osc_rdma_lock_self (module, lock);
        }

        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            goto exit_error;
        }
    } else {
        lock->lock_acks_received = 1;
    }

    OPAL_THREAD_UNLOCK(&module->lock);

    return OMPI_SUCCESS;

exit_error:

    OPAL_THREAD_UNLOCK(&module->lock);
    opal_list_remove_item(&module->outstanding_locks, &lock->super);
    OBJ_RELEASE(lock);

    /* return */
    return ret;
}


int ompi_osc_rdma_unlock(int target, ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_outstanding_lock_t *lock = NULL;
    ompi_osc_rdma_peer_t *peer = module->peers + target;
    int ret = OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&module->lock);

    lock = find_outstanding_lock (module, target);
    if (OPAL_UNLIKELY(NULL == lock)) {
        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "ompi_osc_rdma_unlock: target %d is not locked in window %s",
                             target, win->w_name));
        OPAL_THREAD_LOCK(&module->lock);
        return OMPI_ERR_RMA_SYNC;
    }

    if (ompi_comm_rank (module->comm) != target) {
        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "osc rdma: unlock %d, lock_acks_received = %d", target,
                             lock->lock_acks_received));

        /* wait until ack has arrived from target */
        while (0 == lock->lock_acks_received) {
            opal_condition_wait(&module->cond, &module->lock);
        }

        ret = ompi_osc_rdma_unlock_remote (module, target, lock);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            goto cleanup;
        }

        /* start all sendreqs to target */
        ret = ompi_osc_rdma_frag_flush_target(module, target);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            goto cleanup;
        }

        /* wait for all the requests and the unlock ack (meaning remote completion) */
        while (module->outgoing_frag_count != module->outgoing_frag_signal_count ||
               0 == lock->unlock_acks_received) {
            opal_condition_wait(&module->cond, &module->lock);
        }

        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "ompi_osc_rdma_unlock: unlock of %d complete", target));
    } else {
        ompi_osc_rdma_unlock_self (module, lock);
    }

    module->passive_eager_send_active[target] = false;
    module->epoch_outgoing_frag_count[target] = 0;
    module->passive_target_access_epoch = false;

    peer->access_epoch = false;

    /* delete the lock */
    opal_list_remove_item (&module->outstanding_locks, &lock->super);
    OBJ_RELEASE(lock);

 cleanup:
    OPAL_THREAD_UNLOCK(&module->lock);

    return ret;
}


int ompi_osc_rdma_lock_all(int assert, struct ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    int ret, my_rank = ompi_comm_rank (module->comm);
    ompi_osc_rdma_outstanding_lock_t *lock;

    /* Check if no_locks is set. TODO: we also need to track whether we are in an active
     * target epoch. Fence can make this tricky to track. */
    if (NULL == module->passive_eager_send_active) {
        return OMPI_ERR_RMA_SYNC;
    }

    /* delay all eager sends until we've heard back.. */
    OPAL_THREAD_LOCK(&module->lock);
    for (int i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
        module->passive_eager_send_active[i] = false;
    }
    module->passive_target_access_epoch = true;
    module->all_access_epoch = true;

    /* create lock item */
    lock = OBJ_NEW(ompi_osc_rdma_outstanding_lock_t);
    lock->target = -1;
    lock->lock_acks_received = 0;
    lock->unlock_acks_received = 0;
    lock->serial_number = module->lock_serial_number++;
    lock->type = MPI_LOCK_SHARED;
    opal_list_append(&module->outstanding_locks, &lock->super);

    /* if nocheck is not specified, send a lock request to everyone
       and wait for the local response */
    if (0 != (assert & MPI_MODE_NOCHECK)) {
        ret = ompi_osc_rdma_lock_self (module, lock);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            goto exit_error;
        }

        for (int i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
            if (my_rank == i) {
                continue;
            }

            ret = ompi_osc_rdma_lock_remote (module, i, lock);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                opal_list_remove_item(&module->outstanding_locks, &lock->super);
            }
        }
    } else {
        lock->lock_acks_received = ompi_comm_size(module->comm);
    }

    OPAL_THREAD_UNLOCK(&module->lock);

    return OMPI_SUCCESS;

 exit_error:

    OPAL_THREAD_UNLOCK(&module->lock);
    opal_list_remove_item(&module->outstanding_locks, &lock->super);
    OBJ_RELEASE(lock);

    /* return */
    return ret;
}


int ompi_osc_rdma_unlock_all (struct ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    int my_rank = ompi_comm_rank (module->comm);
    ompi_osc_rdma_outstanding_lock_t *lock;
    int ret;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_unlock_all entering..."));

    OPAL_THREAD_LOCK(&module->lock);

    lock = find_outstanding_lock (module, -1);
    if (OPAL_UNLIKELY(NULL == lock)) {
        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "ompi_osc_rdma_unlock_all: not locked in window %s",
                             win->w_name));
        OPAL_THREAD_LOCK(&module->lock);
        return OMPI_ERR_RMA_SYNC;
    }

    /* wait for lock acks */
    while (ompi_comm_size(module->comm) != lock->lock_acks_received) {
        opal_condition_wait(&module->cond, &module->lock);
    }

    /* send unlock messages to all of my peers */
    for (int i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
        if (my_rank == i) {
            continue;
        }

        ret = ompi_osc_rdma_unlock_remote (module, i, lock);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            goto cleanup;
        }
    }

    /* unlock myself */
    ompi_osc_rdma_unlock_self (module, lock);

    /* start all sendreqs to target */
    ret = ompi_osc_rdma_frag_flush_all(module);
    if (OMPI_SUCCESS != ret) goto cleanup;

    /* wait for all the requests and the unlock ack (meaning remote completion) */
    while (module->outgoing_frag_count != module->outgoing_frag_signal_count ||
           ompi_comm_size(module->comm) != lock->unlock_acks_received) {
        opal_condition_wait(&module->cond, &module->lock);
    }

    /* reset all fragment counters */
    memset (module->epoch_outgoing_frag_count, 0, ompi_comm_size(module->comm) * sizeof (module->epoch_outgoing_frag_count[0]));
    memset (module->passive_eager_send_active, 0, ompi_comm_size(module->comm) * sizeof (module->passive_eager_send_active[0]));

    opal_list_remove_item (&module->outstanding_locks, &lock->super);
    OBJ_RELEASE(lock);

    module->passive_target_access_epoch = false;
    module->all_access_epoch = false;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_unlock_all complete"));

 cleanup:
    OPAL_THREAD_UNLOCK(&module->lock);

    return ret;
}


int ompi_osc_rdma_sync (struct ompi_win_t *win)
{
    opal_progress();
    return OMPI_SUCCESS;
}

static int ompi_osc_rdma_flush_lock (ompi_osc_rdma_module_t *module, ompi_osc_rdma_outstanding_lock_t *lock,
                                     int target)
{
    ompi_osc_rdma_header_flush_t flush_req;
    int peer_count, ret, flush_count;
    int my_rank = ompi_comm_rank (module->comm);

    if (-1 == lock->target) {
        peer_count = ompi_comm_size(module->comm);
    } else {
        peer_count = 1;
    }

    /* wait until ack has arrived from target, since we need to be
       able to eager send before we can transfer all the data... */
    while (peer_count > lock->lock_acks_received) {
        opal_condition_wait(&module->cond, &module->lock);
    }

    lock->flush_acks_received = 0;

    flush_req.base.type = OMPI_OSC_RDMA_HDR_TYPE_FLUSH_REQ;
    flush_req.base.flags = OMPI_OSC_RDMA_HDR_FLAG_VALID | OMPI_OSC_RDMA_HDR_FLAG_PASSIVE_TARGET;
    flush_req.serial_number = lock->serial_number;

    if (-1 == target) {
        /* NTH: no local flush */
        flush_count = ompi_comm_size(module->comm) - 1;
        for (int i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
            if (i == my_rank) {
                continue;
            }

            flush_req.frag_count = module->epoch_outgoing_frag_count[i];

            /* send control message with flush request and count */
            ret = ompi_osc_rdma_control_send (module, i, &flush_req, sizeof (flush_req));
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return ret;
            }

            /* start all sendreqs to target */
            ret = ompi_osc_rdma_frag_flush_target (module, i);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return ret;
            }
        }
    } else {
        flush_req.frag_count = module->epoch_outgoing_frag_count[target];
        flush_count = 1;
        /* send control message with flush request and count */
        ret = ompi_osc_rdma_control_send (module, target, &flush_req, sizeof (flush_req));
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }

        /* start all sendreqs to target */
        ret = ompi_osc_rdma_frag_flush_target (module, target);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }
    }

    /* wait for all the requests and the flush ack (meaning remote completion) */
    while (module->outgoing_frag_count != module->outgoing_frag_signal_count ||
           flush_count != lock->flush_acks_received) {
        opal_condition_wait(&module->cond, &module->lock);
    }

    if (-1 == target) {
        memset (module->epoch_outgoing_frag_count, 0, peer_count * sizeof (module->epoch_outgoing_frag_count[0]));
    } else {
        module->epoch_outgoing_frag_count[target] = 0;
    }

    return OMPI_SUCCESS;
}

int ompi_osc_rdma_flush (int target, struct ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_outstanding_lock_t *lock;
    int ret;

    assert (0 <= target);

    /* flush is only allowed from within a passive target epoch */
    if (!module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_flush starting..."));

    if (ompi_comm_rank (module->comm) == target) {
        /* nothing to flush */
        opal_progress ();
        return OMPI_SUCCESS;
    }

    OPAL_THREAD_LOCK(&module->lock);

    lock = find_outstanding_lock (module, target);
    if (NULL == lock) {
        lock = find_outstanding_lock (module, -1);
    }
    if (OPAL_UNLIKELY(NULL == lock)) {
        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "ompi_osc_rdma_flush: target %d is not locked in window %s",
                             target, win->w_name));
        OPAL_THREAD_LOCK(&module->lock);
        return OMPI_ERR_RMA_SYNC;
    }

    ret = ompi_osc_rdma_flush_lock (module, lock, target);

    OPAL_THREAD_UNLOCK(&module->lock);

    return ret;
}


int ompi_osc_rdma_flush_all (struct ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_outstanding_lock_t *lock;
    int ret = OMPI_SUCCESS;

    /* flush is only allowed from within a passive target epoch */
    if (!module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    if (OPAL_UNLIKELY(0 == opal_list_get_size (&module->outstanding_locks))) {
        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "ompi_osc_rdma_flush_all: no targets are locked in window %s",
                             win->w_name));
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_THREAD_LOCK(&module->lock);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_flush_all entering..."));

    /* flush all locks */
    OPAL_LIST_FOREACH(lock, &module->outstanding_locks, ompi_osc_rdma_outstanding_lock_t) {
        ret = ompi_osc_rdma_flush_lock (module, lock, lock->target);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_flush_all complete"));

    OPAL_THREAD_UNLOCK(&module->lock);

    return ret;
}


int ompi_osc_rdma_flush_local (int target, struct ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    int ret;

    /* flush is only allowed from within a passive target epoch */
    if (!module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_THREAD_LOCK(&module->lock);

    ret = ompi_osc_rdma_frag_flush_target(module, target);
    if (OMPI_SUCCESS != ret) goto cleanup;

    /* wait for all the requests */
    while (module->outgoing_frag_count != module->outgoing_frag_signal_count) {
        opal_condition_wait(&module->cond, &module->lock);
    }

 cleanup:
    OPAL_THREAD_UNLOCK(&module->lock);

    return ret;
}


int ompi_osc_rdma_flush_local_all (struct ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    int ret = OMPI_SUCCESS;

    /* flush is only allowed from within a passive target epoch */
    if (!module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_THREAD_LOCK(&module->lock);

    ret = ompi_osc_rdma_frag_flush_all(module);
    if (OMPI_SUCCESS != ret) goto cleanup;

    /* wait for all the requests */
    while (module->outgoing_frag_count != module->outgoing_frag_signal_count) {
        opal_condition_wait(&module->cond, &module->lock);
    }

 cleanup:
    OPAL_THREAD_UNLOCK(&module->lock);

    return ret;
}

/* target side operation to acknowledge to initiator side that the
   lock is now held by the initiator */
static inline int activate_lock (ompi_osc_rdma_module_t *module, int requestor,
                                 uint64_t serial_number)
{
    ompi_osc_rdma_outstanding_lock_t *lock;

    if (ompi_comm_rank (module->comm) != requestor) {
        ompi_osc_rdma_header_lock_ack_t lock_ack;

        lock_ack.base.type = OMPI_OSC_RDMA_HDR_TYPE_LOCK_ACK;
        lock_ack.base.flags = OMPI_OSC_RDMA_HDR_FLAG_VALID;
        lock_ack.source = ompi_comm_rank(module->comm);
        lock_ack.windx = ompi_comm_get_cid(module->comm);
        lock_ack.serial_number = serial_number;

        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "osc rdma: sending lock to %d", requestor));

        /* we don't want to send any data, since we're the exposure
           epoch only, so use an unbuffered send */
        return ompi_osc_rdma_control_send_unbuffered (module, requestor, &lock_ack, sizeof (lock_ack));
    }


    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "osc rdma: releasing local lock"));

    lock = find_outstanding_lock (module, requestor);
    if (NULL == lock) {
        lock = find_outstanding_lock (module, -1);
        if (OPAL_UNLIKELY(NULL == lock)) {
            OPAL_OUTPUT_VERBOSE((5, ompi_osc_base_framework.framework_output,
                                 "lock could not be located"));
        }
    }

    lock->lock_acks_received++;
    opal_condition_broadcast (&module->cond);

    return OMPI_SUCCESS;
}


/* target side operation to create a pending lock request for a lock
   request that could not be satisfied */
static inline int queue_lock (ompi_osc_rdma_module_t *module, int requestor,
                              int lock_type, uint64_t serial_number)
{
    ompi_osc_rdma_pending_lock_t *pending =
        OBJ_NEW(ompi_osc_rdma_pending_lock_t);
    if (NULL == pending) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    pending->peer = requestor;
    pending->lock_type = lock_type;
    pending->serial_number = serial_number;

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "osc rdma: queueing lock request from %d", requestor));

    opal_list_append(&module->locks_pending, &pending->super);

    return OMPI_SUCCESS;
}

static int ompi_osc_activate_next_lock (ompi_osc_rdma_module_t *module) {
    /* release any other pending locks we can */
    ompi_osc_rdma_pending_lock_t *pending_lock, *next;
    int ret = OMPI_SUCCESS;

    OPAL_LIST_FOREACH_SAFE(pending_lock, next, &module->locks_pending,
                           ompi_osc_rdma_pending_lock_t) {
        if (MPI_LOCK_SHARED == pending_lock->lock_type) {
            OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                                 "ompi_osc_activate_next_lock: release pending lock of type MPI_LOCK_SHARED to peer %d\n",
                                 pending_lock->peer));
            /* acquire shared lock */
            module->lock_status = MPI_LOCK_SHARED;
            module->shared_count++;
            ret = activate_lock(module, pending_lock->peer, pending_lock->serial_number);

            opal_list_remove_item (&module->locks_pending, &pending_lock->super);
            OBJ_RELEASE(pending_lock);
        } else {
            if (0 == module->lock_status) {
                OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                                     "ompi_osc_activate_next_lock: release pending lock of type MPI_LOCK_EXCLUSIVE to peer %d\n",
                                     pending_lock->peer));
                /* acquire exclusive lock */
                module->lock_status = MPI_LOCK_EXCLUSIVE;
                ret = activate_lock(module, pending_lock->peer, pending_lock->serial_number);
                opal_list_remove_item (&module->locks_pending, &pending_lock->super);
                OBJ_RELEASE(pending_lock);
            }
            /* if the lock was acquired (ie, status was 0), then
               we're done.  If the lock was not acquired, we're
               also done, because all the shared locks have to
               finish first */
            break;
        }

        if (OMPI_SUCCESS != ret) {
            break;
        }
    }

    return ret;
}


/* target side function called when the initiator sends a lock
   request.  Lock will either be activated and acknowledged or
   queued. */
int ompi_osc_rdma_process_lock (ompi_osc_rdma_module_t* module, int source,
                                ompi_osc_rdma_header_lock_t* lock_header)
{
    int ret;

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_process_lock: processing lock request from %d. current lock state = %d, shared_count = %d",
                         source, module->lock_status, module->shared_count));

    if (MPI_LOCK_SHARED == lock_header->lock_type) {
        if (module->lock_status != MPI_LOCK_EXCLUSIVE) {
            /* acquire shared lock */
            module->lock_status = MPI_LOCK_SHARED;
            module->shared_count++;
            ret = activate_lock(module, source, lock_header->serial_number);
        } else {
            /* lock not available, queue */
            ret = queue_lock(module, source, lock_header->lock_type, lock_header->serial_number);
        }
    } else {
        if (0 == module->lock_status) {
            /* acquire exclusive lock */
            module->lock_status = MPI_LOCK_EXCLUSIVE;
            ret = activate_lock(module, source, lock_header->serial_number);
        } else {
            /* lock not available, queue */
            ret = queue_lock(module, source, lock_header->lock_type, lock_header->serial_number);
        }
    }

    return ret;
}


/* initiator-side function called when the target acks the lock
   request. */
void ompi_osc_rdma_process_lock_ack (ompi_osc_rdma_module_t *module,
                                     ompi_osc_rdma_header_lock_ack_t *lock_ack_header)
{
    ompi_osc_rdma_outstanding_lock_t *lock, *next;

    OPAL_LIST_FOREACH_SAFE(lock, next, &module->outstanding_locks, ompi_osc_rdma_outstanding_lock_t) {
        if (lock->serial_number == lock_ack_header->serial_number) {

            OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                                 "osc rdma: lock ack %d", lock_ack_header->source));

            lock->lock_acks_received++;
            module->passive_eager_send_active[lock_ack_header->source] = true;
            return;
        }
    }

    opal_output(ompi_osc_base_framework.framework_output,
                "osc rdma: lock ack %d, %ld for unfindable lock request",
                lock_ack_header->source, (unsigned long) lock_ack_header->serial_number);
}

void ompi_osc_rdma_process_flush_ack (ompi_osc_rdma_module_t *module, int source,
                                      ompi_osc_rdma_header_flush_ack_t *flush_ack_header) {
    ompi_osc_rdma_outstanding_lock_t *lock;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_process_unlock_ack: processing flush ack from %d for lock %" PRIu64,
                         source, flush_ack_header->serial_number));

    /* NTH: need to verify that this will work as expected */
    lock = find_outstanding_lock_by_serial (module, flush_ack_header->serial_number);
    assert (NULL != lock);

    lock->flush_acks_received++;

    opal_condition_broadcast(&module->cond);
}

void ompi_osc_rdma_process_unlock_ack (ompi_osc_rdma_module_t *module, int source,
                                       ompi_osc_rdma_header_unlock_ack_t *unlock_ack_header) {
    ompi_osc_rdma_outstanding_lock_t *lock;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_process_unlock_ack: processing unlock ack from %d",
                         source));

    /* NTH: need to verify that this will work as expected */
    lock = find_outstanding_lock (module, source);
    if (NULL == lock) {
        lock = find_outstanding_lock(module, -1);
        assert (NULL != lock);
    }

    lock->unlock_acks_received++;
}

/**
 * Process an unlock request.
 *
 * @param[in] module        - OSC RDMA module
 * @param[in] source        - Source rank
 * @param[in] unlock_header - Incoming unlock header
 *
 * This functions is the target-side functio for handling an unlock
 * request. Once all pending operations from the target are complete
 * this functions sends an unlock acknowledgement then attempts to
 * active a pending lock if the lock becomes free.
 */
int ompi_osc_rdma_process_unlock (ompi_osc_rdma_module_t *module, int source,
                                  ompi_osc_rdma_header_unlock_t *unlock_header)
{
    ompi_osc_rdma_header_unlock_ack_t unlock_ack;
    int ret;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_process_unlock entering (finished %d/%d)...",
                         module->passive_incoming_frag_count[source],
                         module->passive_incoming_frag_signal_count[source]));

    /* we cannot block when processing an incoming request */
    if (module->passive_incoming_frag_signal_count[source] !=
        module->passive_incoming_frag_count[source]) {
        return OMPI_ERR_WOULD_BLOCK;
    }

    unlock_ack.base.type = OMPI_OSC_RDMA_HDR_TYPE_UNLOCK_ACK;
    unlock_ack.base.flags = OMPI_OSC_RDMA_HDR_FLAG_VALID;

    ret = ompi_osc_rdma_control_send_unbuffered (module, source, &unlock_ack, sizeof (unlock_ack));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    module->passive_incoming_frag_signal_count[source] = 0;
    module->passive_incoming_frag_count[source]        = 0;

    OPAL_THREAD_LOCK(&module->lock);

    if (unlock_header->lock_type == MPI_LOCK_EXCLUSIVE || 0 == --module->shared_count) {
        module->lock_status = 0;

        ompi_osc_activate_next_lock (module);
    }

    OPAL_THREAD_UNLOCK(&module->lock);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc rdma: finished processing unlock fragment"));

    return ret;
}

int ompi_osc_rdma_process_flush (ompi_osc_rdma_module_t *module, int source,
                                 ompi_osc_rdma_header_flush_t *flush_header)
{
    ompi_osc_rdma_header_flush_ack_t flush_ack;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_process_flush entering (finished %d/%d)...",
                         module->passive_incoming_frag_count[source],
                         module->passive_incoming_frag_signal_count[source]));

    /* we cannot block when processing an incoming request */
    if (module->passive_incoming_frag_signal_count[source] !=
        module->passive_incoming_frag_count[source]) {
        return OMPI_ERR_WOULD_BLOCK;
    }

    module->passive_incoming_frag_signal_count[source] = 0;
    module->passive_incoming_frag_count[source]        = 0;

    flush_ack.base.type = OMPI_OSC_RDMA_HDR_TYPE_FLUSH_ACK;
    flush_ack.base.flags = OMPI_OSC_RDMA_HDR_FLAG_VALID;
    flush_ack.serial_number = flush_header->serial_number;

    return ompi_osc_rdma_control_send_unbuffered (module, source, &flush_ack, sizeof (flush_ack));
}
