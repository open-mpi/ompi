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
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
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

#include "osc_pt2pt.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_data_move.h"
#include "osc_pt2pt_frag.h"

#include "mpi.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/base/base.h"
#include "opal/include/opal_stdint.h"

static bool ompi_osc_pt2pt_lock_try_acquire (ompi_osc_pt2pt_module_t* module, int source, int lock_type,
                                             uint64_t serial_number);

/* target-side tracking of a lock request */
struct ompi_osc_pt2pt_pending_lock_t {
    opal_list_item_t super;
    int peer;
    int lock_type;
    uint64_t lock_ptr;
};
typedef struct ompi_osc_pt2pt_pending_lock_t ompi_osc_pt2pt_pending_lock_t;
OBJ_CLASS_INSTANCE(ompi_osc_pt2pt_pending_lock_t, opal_list_item_t,
                   NULL, NULL);


/* origin-side tracking of a lock request */
struct ompi_osc_pt2pt_outstanding_lock_t {
    opal_list_item_t super;
    int target;
    int assert;
    bool flushing;
    int32_t lock_acks_received;
    int32_t unlock_acks_received;
    int32_t flush_acks_received;
    uint64_t serial_number;
    int32_t type;
};
typedef struct ompi_osc_pt2pt_outstanding_lock_t ompi_osc_pt2pt_outstanding_lock_t;
OBJ_CLASS_INSTANCE(ompi_osc_pt2pt_outstanding_lock_t, opal_list_item_t,
                   NULL, NULL);

static int ompi_osc_activate_next_lock (ompi_osc_pt2pt_module_t *module);
static inline int queue_lock (ompi_osc_pt2pt_module_t *module, int requestor, int lock_type, uint64_t lock_ptr);
static int ompi_osc_pt2pt_flush_lock (ompi_osc_pt2pt_module_t *module, ompi_osc_pt2pt_outstanding_lock_t *lock,
                                      int target);


/**
 * Find the first outstanding lock to a target.
 *
 * @param[in] module   - OSC PT2PT module
 * @param[in] target   - Target rank
 *
 * @returns an outstanding lock on success
 *
 * This function traverses the outstanding_locks list in the module
 * looking for a lock that matches target. The caller must hold the
 * module lock.
 */
static inline ompi_osc_pt2pt_outstanding_lock_t *find_outstanding_lock_st (ompi_osc_pt2pt_module_t *module, int target)
{
    ompi_osc_pt2pt_outstanding_lock_t *outstanding_lock, *lock = NULL;

    OPAL_LIST_FOREACH(outstanding_lock, &module->outstanding_locks, ompi_osc_pt2pt_outstanding_lock_t) {
        if (outstanding_lock->target == target) {
            lock = outstanding_lock;
            break;
        }
    }

    return lock;
}

static inline ompi_osc_pt2pt_outstanding_lock_t *find_outstanding_lock (ompi_osc_pt2pt_module_t *module, int target)
{
    ompi_osc_pt2pt_outstanding_lock_t *lock;

    OPAL_THREAD_LOCK(&module->lock);
    lock = find_outstanding_lock_st (module, target);
    OPAL_THREAD_UNLOCK(&module->lock);

    return lock;
}

static inline ompi_osc_pt2pt_outstanding_lock_t *find_outstanding_lock_by_serial (ompi_osc_pt2pt_module_t *module, uint64_t serial_number)
{
    ompi_osc_pt2pt_outstanding_lock_t *outstanding_lock, *lock = NULL;

    OPAL_THREAD_LOCK(&module->lock);
    OPAL_LIST_FOREACH(outstanding_lock, &module->outstanding_locks, ompi_osc_pt2pt_outstanding_lock_t) {
        if (outstanding_lock->serial_number == serial_number) {
            lock = outstanding_lock;
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&module->lock);

    return lock;
}

static inline int ompi_osc_pt2pt_lock_self (ompi_osc_pt2pt_module_t *module, ompi_osc_pt2pt_outstanding_lock_t *lock)
{
    const int my_rank = ompi_comm_rank (module->comm);
    bool acquired = false;

    acquired = ompi_osc_pt2pt_lock_try_acquire (module, my_rank, lock->type, (uint64_t) (uintptr_t) lock);
    if (!acquired) {
        /* queue the lock */
        queue_lock (module, my_rank, lock->type, (uint64_t) (uintptr_t) lock);

        /* If locking local, can't be non-blocking according to the
           standard.  We need to wait for the ack here. */
        OPAL_THREAD_LOCK(&module->lock);
        while (0 == lock->lock_acks_received) {
            opal_condition_wait(&module->cond, &module->lock);
        }
        OPAL_THREAD_UNLOCK(&module->lock);
    }

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "local lock aquired"));

    return OMPI_SUCCESS;
}

static inline void ompi_osc_pt2pt_unlock_self (ompi_osc_pt2pt_module_t *module, ompi_osc_pt2pt_outstanding_lock_t *lock)
{
    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_unlock_self: unlocking myself. lock state = %d", module->lock_status));

    if (MPI_LOCK_EXCLUSIVE == lock->type) {
        OPAL_THREAD_ADD32(&module->lock_status, 1);
        ompi_osc_activate_next_lock (module);
    } else if (0 == OPAL_THREAD_ADD32(&module->lock_status, -1)) {
        ompi_osc_activate_next_lock (module);
    }

    /* need to ensure we make progress */
    opal_progress();

    OPAL_THREAD_ADD32(&lock->unlock_acks_received, 1);
}

static inline int ompi_osc_pt2pt_lock_remote (ompi_osc_pt2pt_module_t *module, int target, ompi_osc_pt2pt_outstanding_lock_t *lock)
{
    ompi_osc_pt2pt_header_lock_t lock_req;
    int ret;

    /* generate a lock request */
    lock_req.base.type = OMPI_OSC_PT2PT_HDR_TYPE_LOCK_REQ;
    lock_req.base.flags = OMPI_OSC_PT2PT_HDR_FLAG_VALID | OMPI_OSC_PT2PT_HDR_FLAG_PASSIVE_TARGET;
    lock_req.lock_type = lock->type;
    lock_req.lock_ptr = (uint64_t) (uintptr_t) lock;

    ret = ompi_osc_pt2pt_control_send (module, target, &lock_req, sizeof (lock_req));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    /* make sure the request gets sent, so we can start eager sending... */
    ret = ompi_osc_pt2pt_frag_flush_target (module, target);

    return ret;
}

static inline int ompi_osc_pt2pt_unlock_remote (ompi_osc_pt2pt_module_t *module, int target, ompi_osc_pt2pt_outstanding_lock_t *lock)
{
    ompi_osc_pt2pt_header_unlock_t unlock_req;
    int32_t frag_count = opal_atomic_swap_32 ((int32_t *) module->epoch_outgoing_frag_count + target, -1);

    unlock_req.base.type = OMPI_OSC_PT2PT_HDR_TYPE_UNLOCK_REQ;
    unlock_req.base.flags = OMPI_OSC_PT2PT_HDR_FLAG_VALID | OMPI_OSC_PT2PT_HDR_FLAG_PASSIVE_TARGET;
    unlock_req.frag_count = frag_count;
    unlock_req.lock_type = lock->type;
    unlock_req.lock_ptr = (uint64_t) (uintptr_t) lock;

    /* send control message with unlock request and count */
    return ompi_osc_pt2pt_control_send (module, target, &unlock_req, sizeof (unlock_req));
}

static int ompi_osc_pt2pt_lock_internal_execute (ompi_osc_pt2pt_module_t *module, ompi_osc_pt2pt_outstanding_lock_t *lock)
{
    int my_rank = ompi_comm_rank (module->comm);
    int target = lock->target;
    int assert = lock->assert;
    int ret;

    if (0 == (assert & MPI_MODE_NOCHECK)) {
        if (my_rank != target && target != -1) {
            ret = ompi_osc_pt2pt_lock_remote (module, target, lock);
        } else {
            ret = ompi_osc_pt2pt_lock_self (module, lock);
        }

        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            /* return */
            return ret;
        }

        if (-1 == target) {
            for (int i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
                if (my_rank == i) {
                    continue;
                }

                ret = ompi_osc_pt2pt_lock_remote (module, i, lock);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    return ret;
                }
            }

        }
    } else {
        if (-1 == target) {
            lock->lock_acks_received = ompi_comm_size(module->comm);
        } else {
            lock->lock_acks_received = 1;
        }
    }

    return OMPI_SUCCESS;
}

static int ompi_osc_pt2pt_lock_internal (int lock_type, int target, int assert, ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_osc_pt2pt_outstanding_lock_t *lock;
    ompi_osc_pt2pt_peer_t *peer = NULL;
    int ret = OMPI_SUCCESS;

    if (-1 != target) {
        peer = module->peers + target;
    }

    /* Check if no_locks is set. TODO: we also need to track whether we are in an
     * active target epoch. Fence can make this tricky to track. */
    if (module->sc_group) {
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: lock %d %d", target, lock_type));

    /* create lock item */
    lock = OBJ_NEW(ompi_osc_pt2pt_outstanding_lock_t);
    if (OPAL_UNLIKELY(NULL == lock)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    lock->target = target;
    lock->lock_acks_received = 0;
    lock->unlock_acks_received = 0;
    lock->serial_number = OPAL_THREAD_ADD64((int64_t *) &module->lock_serial_number, 1);
    lock->type = lock_type;
    lock->assert = assert;

    /* delay all eager sends until we've heard back.. */
    OPAL_THREAD_LOCK(&module->lock);

    /* check for conflicting lock */
    if (find_outstanding_lock_st (module, target)) {
        OBJ_RELEASE(lock);
        OPAL_THREAD_UNLOCK(&module->lock);
        return OMPI_ERR_RMA_CONFLICT;
    }

    /* when the lock ack returns we will be in an access epoch with this peer/all peers (target = -1) */
    if (-1 == target) {
        module->all_access_epoch = true;
    } else {
        peer->access_epoch = true;
    }

    module->passive_target_access_epoch = true;

    opal_list_append(&module->outstanding_locks, &lock->super);
    OPAL_THREAD_UNLOCK(&module->lock);

    ret = ompi_osc_pt2pt_lock_internal_execute (module, lock);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OPAL_THREAD_SCOPED_LOCK(&module->lock,
                                opal_list_remove_item(&module->outstanding_locks, &lock->super));
        OBJ_RELEASE(lock);
    }

    return ret;
}

static int ompi_osc_pt2pt_unlock_internal (int target, ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_osc_pt2pt_outstanding_lock_t *lock = NULL;
    int my_rank = ompi_comm_rank (module->comm);
    ompi_osc_pt2pt_peer_t *peer = NULL;
    int lock_acks_expected;
    int ret = OMPI_SUCCESS;

    if (-1 != target) {
        lock_acks_expected = 1;
        peer = module->peers + target;
    } else {
        lock_acks_expected = ompi_comm_size (module->comm);
    }

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_unlock_internal: unlocking target %d", target));

    OPAL_THREAD_LOCK(&module->lock);
    lock = find_outstanding_lock_st (module, target);
    if (OPAL_UNLIKELY(NULL == lock)) {
        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "ompi_osc_pt2pt_unlock: target %d is not locked in window %s",
                             target, win->w_name));
        OPAL_THREAD_UNLOCK(&module->lock);
        return OMPI_ERR_RMA_SYNC;
    }

    opal_list_remove_item (&module->outstanding_locks, &lock->super);

    /* wait until ack has arrived from target */
    while (lock->lock_acks_received != lock_acks_expected) {
        opal_condition_wait(&module->cond, &module->lock);
    }
    OPAL_THREAD_UNLOCK(&module->lock);

    if (lock->assert & MPI_MODE_NOCHECK) {
        /* flush instead */
        ompi_osc_pt2pt_flush_lock (module, lock, target);
    } else if (my_rank != target) {
        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "osc pt2pt: unlock %d, lock_acks_received = %d", target,
                             lock->lock_acks_received));

        if (-1 == target) {
            /* send unlock messages to all of my peers */
            for (int i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
                if (my_rank == i) {
                    continue;
                }

                ret = ompi_osc_pt2pt_unlock_remote (module, i, lock);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    return ret;
                }
            }

            ompi_osc_pt2pt_unlock_self (module, lock);
        } else {
            ret = ompi_osc_pt2pt_unlock_remote (module, target, lock);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return ret;
            }
        }

        /* start all sendreqs to target */
        if (-1 == target) {
            ret = ompi_osc_pt2pt_frag_flush_all (module);
        } else {
            ret = ompi_osc_pt2pt_frag_flush_target(module, target);
        }

        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }

        /* wait for unlock acks. this signals remote completion of fragments */
        OPAL_THREAD_LOCK(&module->lock);
        while (lock->unlock_acks_received != lock_acks_expected) {
            opal_condition_wait(&module->cond, &module->lock);
        }
        OPAL_THREAD_UNLOCK(&module->lock);

        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "ompi_osc_pt2pt_unlock: unlock of %d complete", target));
    } else {
        ompi_osc_pt2pt_unlock_self (module, lock);
    }

    OPAL_THREAD_LOCK(&module->lock);
    if (-1 != target) {
        peer->access_epoch = false;
        module->passive_target_access_epoch = false;
    } else {
        module->passive_target_access_epoch = false;
        module->all_access_epoch = false;
    }
    OPAL_THREAD_UNLOCK(&module->lock);

    OBJ_RELEASE(lock);

    return ret;
}

int ompi_osc_pt2pt_lock(int lock_type, int target, int assert, ompi_win_t *win)
{
    assert(target >= 0);

    return ompi_osc_pt2pt_lock_internal (lock_type, target, assert, win);
}

int ompi_osc_pt2pt_unlock (int target, struct ompi_win_t *win)
{
    return ompi_osc_pt2pt_unlock_internal (target, win);
}

int ompi_osc_pt2pt_lock_all(int assert, struct ompi_win_t *win)
{
    return ompi_osc_pt2pt_lock_internal (MPI_LOCK_SHARED, -1, assert, win);
}


int ompi_osc_pt2pt_unlock_all (struct ompi_win_t *win)
{
    return ompi_osc_pt2pt_unlock_internal (-1, win);
}


int ompi_osc_pt2pt_sync (struct ompi_win_t *win)
{
    opal_progress();
    return OMPI_SUCCESS;
}

static int ompi_osc_pt2pt_flush_lock (ompi_osc_pt2pt_module_t *module, ompi_osc_pt2pt_outstanding_lock_t *lock,
                                      int target)
{
    ompi_osc_pt2pt_header_flush_t flush_req;
    int peer_count, ret, flush_count;
    int my_rank = ompi_comm_rank (module->comm);

    if (-1 == lock->target) {
        peer_count = ompi_comm_size(module->comm);
    } else {
        peer_count = 1;
    }

    /* wait until ack has arrived from target, since we need to be
       able to eager send before we can transfer all the data... */
    OPAL_THREAD_LOCK(&module->lock);
    while (peer_count > lock->lock_acks_received && lock->flushing) {
        opal_condition_wait(&module->cond, &module->lock);
    }

    lock->flush_acks_received = 0;
    lock->flushing = true;
    OPAL_THREAD_UNLOCK(&module->lock);

    flush_req.base.type = OMPI_OSC_PT2PT_HDR_TYPE_FLUSH_REQ;
    flush_req.base.flags = OMPI_OSC_PT2PT_HDR_FLAG_VALID | OMPI_OSC_PT2PT_HDR_FLAG_PASSIVE_TARGET;
    flush_req.serial_number = lock->serial_number;

    if (-1 == target) {
        /* NTH: no local flush */
        flush_count = ompi_comm_size(module->comm) - 1;
        for (int i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
            if (i == my_rank) {
                continue;
            }

            flush_req.frag_count = opal_atomic_swap_32 ((int32_t *) module->epoch_outgoing_frag_count + i, -1);

            /* send control message with flush request and count */
            ret = ompi_osc_pt2pt_control_send (module, i, &flush_req, sizeof (flush_req));
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return ret;
            }

            /* start all sendreqs to target */
            ret = ompi_osc_pt2pt_frag_flush_target (module, i);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return ret;
            }
        }
    } else {
        flush_req.frag_count = opal_atomic_swap_32 ((int32_t *) module->epoch_outgoing_frag_count + target, -1);
        flush_count = 1;
        /* send control message with flush request and count */
        ret = ompi_osc_pt2pt_control_send (module, target, &flush_req, sizeof (flush_req));
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }

        /* start all sendreqs to target */
        ret = ompi_osc_pt2pt_frag_flush_target (module, target);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }
    }

    /* wait for all the requests and the flush ack (meaning remote completion) */
    OPAL_THREAD_LOCK(&module->lock);
    while (flush_count != lock->flush_acks_received) {
        opal_condition_wait(&module->cond, &module->lock);
    }

    lock->flushing = false;
    opal_condition_broadcast(&module->cond);

    OPAL_THREAD_UNLOCK(&module->lock);

    return OMPI_SUCCESS;
}

int ompi_osc_pt2pt_flush (int target, struct ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_osc_pt2pt_outstanding_lock_t *lock;
    int ret;

    assert (0 <= target);

    /* flush is only allowed from within a passive target epoch */
    if (!module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_flush starting..."));

    if (ompi_comm_rank (module->comm) == target) {
        /* nothing to flush */
        opal_progress ();
        return OMPI_SUCCESS;
    }

    lock = find_outstanding_lock (module, target);
    if (NULL == lock) {
        lock = find_outstanding_lock (module, -1);
    }
    if (OPAL_UNLIKELY(NULL == lock)) {
        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "ompi_osc_pt2pt_flush: target %d is not locked in window %s",
                             target, win->w_name));
        ret = OMPI_ERR_RMA_SYNC;
    } else {
        ret = ompi_osc_pt2pt_flush_lock (module, lock, target);
    }

    return ret;
}


int ompi_osc_pt2pt_flush_all (struct ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_osc_pt2pt_outstanding_lock_t *lock;
    int ret = OMPI_SUCCESS;

    /* flush is only allowed from within a passive target epoch */
    if (OPAL_UNLIKELY(!module->passive_target_access_epoch ||
                      0 == opal_list_get_size (&module->outstanding_locks))) {
        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "ompi_osc_pt2pt_flush_all: no targets are locked in window %s",
                             win->w_name));
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_flush_all entering..."));

    /* flush all locks */
    OPAL_LIST_FOREACH(lock, &module->outstanding_locks, ompi_osc_pt2pt_outstanding_lock_t) {
        ret = ompi_osc_pt2pt_flush_lock (module, lock, lock->target);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_flush_all complete"));

    return ret;
}


int ompi_osc_pt2pt_flush_local (int target, struct ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    int ret;

    /* flush is only allowed from within a passive target epoch */
    if (!module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    ret = ompi_osc_pt2pt_frag_flush_target(module, target);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* wait for all the requests */
    OPAL_THREAD_LOCK(&module->lock);
    while (module->outgoing_frag_count != module->outgoing_frag_signal_count) {
        opal_condition_wait(&module->cond, &module->lock);
    }
    OPAL_THREAD_UNLOCK(&module->lock);

    return OMPI_SUCCESS;
}


int ompi_osc_pt2pt_flush_local_all (struct ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    int ret = OMPI_SUCCESS;

    /* flush is only allowed from within a passive target epoch */
    if (!module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    ret = ompi_osc_pt2pt_frag_flush_all(module);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* wait for all the requests */
    OPAL_THREAD_LOCK(&module->lock);
    while (module->outgoing_frag_count != module->outgoing_frag_signal_count) {
        opal_condition_wait(&module->cond, &module->lock);
    }
    OPAL_THREAD_UNLOCK(&module->lock);

    return OMPI_SUCCESS;
}

/* target side operation to acknowledge to initiator side that the
   lock is now held by the initiator */
static inline int activate_lock (ompi_osc_pt2pt_module_t *module, int requestor,
                                 uint64_t lock_ptr)
{
    ompi_osc_pt2pt_outstanding_lock_t *lock;

    if (ompi_comm_rank (module->comm) != requestor) {
        ompi_osc_pt2pt_header_lock_ack_t lock_ack;

        lock_ack.base.type = OMPI_OSC_PT2PT_HDR_TYPE_LOCK_ACK;
        lock_ack.base.flags = OMPI_OSC_PT2PT_HDR_FLAG_VALID;
        lock_ack.source = ompi_comm_rank(module->comm);
        lock_ack.windx = ompi_comm_get_cid(module->comm);
        lock_ack.lock_ptr = lock_ptr;

        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "osc pt2pt: sending lock to %d", requestor));

        /* we don't want to send any data, since we're the exposure
           epoch only, so use an unbuffered send */
        return ompi_osc_pt2pt_control_send_unbuffered (module, requestor, &lock_ack, sizeof (lock_ack));
    }


    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: releasing local lock"));

    lock = (ompi_osc_pt2pt_outstanding_lock_t *) (uintptr_t) lock_ptr;
    if (OPAL_UNLIKELY(NULL == lock)) {
        OPAL_OUTPUT_VERBOSE((5, ompi_osc_base_framework.framework_output,
                             "lock could not be located"));
    }

    OPAL_THREAD_ADD32(&lock->lock_acks_received, 1);
    opal_condition_broadcast (&module->cond);

    return OMPI_SUCCESS;
}


/* target side operation to create a pending lock request for a lock
   request that could not be satisfied */
static inline int queue_lock (ompi_osc_pt2pt_module_t *module, int requestor,
                              int lock_type, uint64_t lock_ptr)
{
    ompi_osc_pt2pt_pending_lock_t *pending =
        OBJ_NEW(ompi_osc_pt2pt_pending_lock_t);
    if (NULL == pending) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    pending->peer = requestor;
    pending->lock_type = lock_type;
    pending->lock_ptr = lock_ptr;

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: queueing lock request from %d", requestor));

    OPAL_THREAD_SCOPED_LOCK(&module->locks_pending_lock, opal_list_append(&module->locks_pending, &pending->super));

    return OMPI_SUCCESS;
}

static bool ompi_osc_pt2pt_lock_try_acquire (ompi_osc_pt2pt_module_t* module, int source, int lock_type, uint64_t lock_ptr)
{
    bool queue = false;

    if (MPI_LOCK_SHARED == lock_type) {
        int32_t lock_status = module->lock_status;

        do {
            if (lock_status < 0) {
                queue = true;
                break;
            }

            if (opal_atomic_cmpset_32 (&module->lock_status, lock_status, lock_status + 1)) {
                break;
            }

            lock_status = module->lock_status;
        } while (1);
    } else {
        queue = !opal_atomic_cmpset_32 (&module->lock_status, 0, -1);
    }

    if (queue) {
        return false;
    }

    activate_lock(module, source, lock_ptr);

    /* activated the lock */
    return true;
}

static int ompi_osc_activate_next_lock (ompi_osc_pt2pt_module_t *module) {
    /* release any other pending locks we can */
    ompi_osc_pt2pt_pending_lock_t *pending_lock, *next;
    int ret = OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&module->locks_pending_lock);
    OPAL_LIST_FOREACH_SAFE(pending_lock, next, &module->locks_pending,
                           ompi_osc_pt2pt_pending_lock_t) {
        bool acquired = ompi_osc_pt2pt_lock_try_acquire (module, pending_lock->peer, pending_lock->lock_type,
                                                         pending_lock->lock_ptr);
        if (!acquired) {
            break;
        }

        opal_list_remove_item (&module->locks_pending, &pending_lock->super);
        OBJ_RELEASE(pending_lock);
    }
    OPAL_THREAD_UNLOCK(&module->locks_pending_lock);

    return ret;
}


/* target side function called when the initiator sends a lock
   request.  Lock will either be activated and acknowledged or
   queued. */
int ompi_osc_pt2pt_process_lock (ompi_osc_pt2pt_module_t* module, int source,
                                ompi_osc_pt2pt_header_lock_t* lock_header)
{
    bool acquired;

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_process_lock: processing lock request from %d. current lock state = %d",
                         source, module->lock_status));

    acquired = ompi_osc_pt2pt_lock_try_acquire (module, source, lock_header->lock_type, lock_header->lock_ptr);

    if (!acquired) {
        queue_lock(module, source, lock_header->lock_type, lock_header->lock_ptr);
    }

    return OMPI_SUCCESS;
}


/* initiator-side function called when the target acks the lock
   request. */
void ompi_osc_pt2pt_process_lock_ack (ompi_osc_pt2pt_module_t *module,
                                     ompi_osc_pt2pt_header_lock_ack_t *lock_ack_header)
{
    ompi_osc_pt2pt_peer_t *peer = module->peers + lock_ack_header->source;
    ompi_osc_pt2pt_outstanding_lock_t *lock;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_process_unlock_ack: processing lock ack from %d for lock %" PRIu64,
                         lock_ack_header->source, lock_ack_header->lock_ptr));

    lock = (ompi_osc_pt2pt_outstanding_lock_t *) (uintptr_t) lock_ack_header->lock_ptr;
    assert (NULL != lock);

    /* no need to hold the lock to set this */
    peer->eager_send_active = true;
    OPAL_THREAD_ADD32(&lock->lock_acks_received, 1);

    opal_condition_broadcast(&module->cond);
}

void ompi_osc_pt2pt_process_flush_ack (ompi_osc_pt2pt_module_t *module, int source,
                                      ompi_osc_pt2pt_header_flush_ack_t *flush_ack_header) {
    ompi_osc_pt2pt_outstanding_lock_t *lock;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_process_unlock_ack: processing flush ack from %d for lock %" PRIu64,
                         source, flush_ack_header->serial_number));

    /* NTH: need to verify that this will work as expected */
    lock = find_outstanding_lock_by_serial (module, flush_ack_header->serial_number);
    assert (NULL != lock);

    OPAL_THREAD_ADD32(&lock->flush_acks_received, 1);

    opal_condition_broadcast(&module->cond);
}

void ompi_osc_pt2pt_process_unlock_ack (ompi_osc_pt2pt_module_t *module, int source,
                                       ompi_osc_pt2pt_header_unlock_ack_t *unlock_ack_header)
{
    ompi_osc_pt2pt_peer_t *peer = module->peers + source;
    ompi_osc_pt2pt_outstanding_lock_t *lock;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_process_unlock_ack: processing unlock ack from %d",
                         source));

    /* NTH: need to verify that this will work as expected */
    lock = (ompi_osc_pt2pt_outstanding_lock_t *) (intptr_t) unlock_ack_header->lock_ptr;
    assert (NULL != lock);

    peer->eager_send_active = false;

    if (0 == OPAL_THREAD_ADD32(&lock->unlock_acks_received, 1)) {
        opal_condition_broadcast(&module->cond);
    }
}

/**
 * Process an unlock request.
 *
 * @param[in] module        - OSC PT2PT module
 * @param[in] source        - Source rank
 * @param[in] unlock_header - Incoming unlock header
 *
 * This functions is the target-side function for handling an unlock
 * request. Once all pending operations from the target are complete
 * this functions sends an unlock acknowledgement then attempts to
 * active a pending lock if the lock becomes free.
 */
int ompi_osc_pt2pt_process_unlock (ompi_osc_pt2pt_module_t *module, int source,
                                  ompi_osc_pt2pt_header_unlock_t *unlock_header)
{
    ompi_osc_pt2pt_header_unlock_ack_t unlock_ack;
    ompi_osc_pt2pt_peer_t *peer = module->peers + source;
    int ret;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_process_unlock entering (passive_incoming_frag_count: %d)...",
                         peer->passive_incoming_frag_count));

    /* we cannot block when processing an incoming request */
    if (0 != peer->passive_incoming_frag_count) {
        return OMPI_ERR_WOULD_BLOCK;
    }

    unlock_ack.base.type = OMPI_OSC_PT2PT_HDR_TYPE_UNLOCK_ACK;
    unlock_ack.base.flags = OMPI_OSC_PT2PT_HDR_FLAG_VALID;
    unlock_ack.lock_ptr = unlock_header->lock_ptr;

    ret = ompi_osc_pt2pt_control_send_unbuffered (module, source, &unlock_ack, sizeof (unlock_ack));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    if (-1 == module->lock_status) {
        OPAL_THREAD_ADD32(&module->lock_status, 1);
        ompi_osc_activate_next_lock (module);
    } else if (0 == OPAL_THREAD_ADD32(&module->lock_status, -1)) {
        ompi_osc_activate_next_lock (module);
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: finished processing unlock fragment"));

    return ret;
}

int ompi_osc_pt2pt_process_flush (ompi_osc_pt2pt_module_t *module, int source,
                                 ompi_osc_pt2pt_header_flush_t *flush_header)
{
    ompi_osc_pt2pt_peer_t *peer = module->peers + source;
    ompi_osc_pt2pt_header_flush_ack_t flush_ack;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_process_flush entering (passive_incoming_frag_count: %d)...",
                         peer->passive_incoming_frag_count));

    /* we cannot block when processing an incoming request */
    if (0 != peer->passive_incoming_frag_count) {
        return OMPI_ERR_WOULD_BLOCK;
    }

    flush_ack.base.type = OMPI_OSC_PT2PT_HDR_TYPE_FLUSH_ACK;
    flush_ack.base.flags = OMPI_OSC_PT2PT_HDR_FLAG_VALID;
    flush_ack.serial_number = flush_header->serial_number;

    return ompi_osc_pt2pt_control_send_unbuffered (module, source, &flush_ack, sizeof (flush_ack));
}
