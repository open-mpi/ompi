/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_OSC_RDMA_LOCK_H
#define OMPI_OSC_RDMA_LOCK_H

#include "osc_rdma_types.h"
#include "osc_rdma_frag.h"
#include "osc_rdma_btl_comm.h"

static inline int ompi_osc_rdma_trylock_local (ompi_osc_rdma_atomic_lock_t *lock)
{
    ompi_osc_rdma_lock_t _tmp_value = 0;
    return !ompi_osc_rdma_lock_compare_exchange (lock, &_tmp_value, OMPI_OSC_RDMA_LOCK_EXCLUSIVE);
}

static inline void ompi_osc_rdma_unlock_local (ompi_osc_rdma_atomic_lock_t *lock)
{
    (void) ompi_osc_rdma_lock_add (lock, -OMPI_OSC_RDMA_LOCK_EXCLUSIVE);
}

__opal_attribute_always_inline__
static inline int ompi_osc_rdma_lock_btl_fop (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer, uint64_t address,
                                              int op, ompi_osc_rdma_lock_t operand, ompi_osc_rdma_lock_t *result,
                                              const bool wait_for_completion)
{
    return ompi_osc_rdma_btl_fop (module, peer->state_btl_index, peer->state_endpoint, address, peer->state_handle, op,
                                  operand, 0, result, wait_for_completion, NULL, NULL, NULL);
}

__opal_attribute_always_inline__
static inline int ompi_osc_rdma_lock_btl_op (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer, uint64_t address,
                                             int op, ompi_osc_rdma_lock_t operand, const bool wait_for_completion)
{
    return ompi_osc_rdma_btl_op (module, peer->state_btl_index, peer->state_endpoint, address, peer->state_handle, op,
                                 operand, 0, wait_for_completion, NULL, NULL, NULL);
}

__opal_attribute_always_inline__
static inline int ompi_osc_rdma_lock_btl_cswap (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer, uint64_t address,
                                                ompi_osc_rdma_lock_t compare, ompi_osc_rdma_lock_t value, ompi_osc_rdma_lock_t *result)
{
    return ompi_osc_rdma_btl_cswap (module, peer->state_btl_index, peer->state_endpoint, address, peer->state_handle, compare, value,
                                    0, result);
}

/**
 * ompi_osc_rdma_lock_acquire_shared:
 *
 * @param[in] module    - osc/rdma module
 * @param[in] peer      - peer object
 * @param[in] value     - increment value
 * @param[in] offset    - offset of lock in remote peer's state segment
 *
 * @returns OMPI_SUCCESS on success and another ompi error code on failure
 *
 * This function increments a remote shared lock. The value provided in
 * {value} should be the negative of the one used for ompi_osc_rdma_lock_acquire_shared.
 * It is erroneous to release a shared lock not held by the calling process.
 */
static inline int ompi_osc_rdma_lock_release_shared (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer,
                                                     ompi_osc_rdma_lock_t value, ptrdiff_t offset)
{
    uint64_t lock = (uint64_t) (intptr_t) peer->state + offset;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "releasing shared lock %" PRIx64 " on peer %d. value 0x%lx", lock,
                     peer->rank, (unsigned long) value);

    if (!ompi_osc_rdma_peer_local_state (peer)) {
        return ompi_osc_rdma_lock_btl_op (module, peer, lock, MCA_BTL_ATOMIC_ADD, value, false);
    }

    (void) ompi_osc_rdma_lock_add ((ompi_osc_rdma_atomic_lock_t *) lock, value);

    return OMPI_SUCCESS;
}

/**
 * ompi_osc_rdma_lock_acquire_shared:
 *
 * @param[in] module    - osc rdma module
 * @param[in] peer      - owner of lock
 * @param[in] value     - increment value
 * @param[in] offset    - offset of lock in remote peer's state segment
 * @param[in] check     - check value for success
 *
 * @returns OMPI_SUCCESS on success and another ompi error code on failure
 *
 * This function increments a remote shared lock and checks it against the
 * check value in {check}. If any of the bits in the prior counter value
 * match those in {check} the function decrements the value and tries again.
 */
static inline int ompi_osc_rdma_lock_acquire_shared (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer,
                                                     ompi_osc_rdma_lock_t value, ptrdiff_t offset,
                                                     ompi_osc_rdma_lock_t check)
{
    uint64_t lock = (uint64_t) peer->state + offset;
    ompi_osc_rdma_lock_t lock_state;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "acquiring shared lock %" PRIx64 " on peer %d. value 0x%lx", lock,
                     peer->rank, (unsigned long) value);

    /* spin until the lock has been acquired */
    if (!ompi_osc_rdma_peer_local_state (peer)) {
        do {
            ret = ompi_osc_rdma_lock_btl_fop (module, peer, lock, MCA_BTL_ATOMIC_ADD, value, &lock_state, true);
            if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
                OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "failed to increment shared lock. opal error code %d", ret);
                return ret;
            }

            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "shared lock incremented. old value 0x%lx", (unsigned long) lock_state);

            if (!(lock_state & check)) {
                break;
            }

            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "another peer has exclusive access to lock");

            /* NTH: i think this is correct. backoff! */
            ompi_osc_rdma_lock_release_shared (module, peer, -value, offset);
            ompi_osc_rdma_progress (module);
        } while (1);
    } else {
        do {
            lock_state = ompi_osc_rdma_lock_add ((ompi_osc_rdma_atomic_lock_t *) lock, value);
            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "local shared lock incremented. old value 0x%lx",
                             (unsigned long) lock_state);
            if (!(lock_state & check)) {
                break;
            }

            (void) ompi_osc_rdma_lock_add ((ompi_osc_rdma_atomic_lock_t *) lock, -value);
            ompi_osc_rdma_progress (module);
        } while (1);
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "shared lock acquired");

    return OMPI_SUCCESS;
}

/**
 * ompi_osc_rdma_lock_try_acquire_exclusive:
 *
 * @param[in] module   - osc/rdma module
 * @param[in] peer     - peer object
 * @param[in] offset   - offset of lock in peer's state structure
 *
 * @returns 0 on success, 1 on failure
 *
 * This function attempts to obtain an exclusive lock at {offset} in a peer's state.
 */
static inline int ompi_osc_rdma_lock_try_acquire_exclusive (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer,
                                                            ptrdiff_t offset)
{
    uint64_t lock = (uint64_t) (uintptr_t) peer->state + offset;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "trying to acquire exclusive lock %" PRIx64 " on peer %d", lock,
                     peer->rank);

    if (!ompi_osc_rdma_peer_local_state (peer)) {
        /* set the temporary value so we can detect success. note that a lock should never be -1 */
        ompi_osc_rdma_lock_t lock_state = -1;

        ret = ompi_osc_rdma_lock_btl_cswap (module, peer, lock, 0, OMPI_OSC_RDMA_LOCK_EXCLUSIVE, &lock_state);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }

#if OPAL_ENABLE_DEBUG
        if (0 == lock_state) {
            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "exclusive lock acquired");
        } else {
            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "could not acquire exclusive lock. lock state 0x%" PRIx64,
                             (uint64_t) lock_state);
        }
#endif

        return lock_state != 0;
    }

    return ompi_osc_rdma_trylock_local ((ompi_osc_rdma_atomic_lock_t *)(intptr_t) lock);
}

/**
 * ompi_osc_rdma_lock_acquire_exclusive:
 *
 * @param[in] module   - osc/rdma module
 * @param[in] peer     - peer object
 * @param[in] offset   - offset into the remote peer's state segment
 *
 * @returns OMPI_SUCCESS on success or another ompi error code on failure
 *
 * This function obtains an exclusive lock at {offset} in a peer's state.
 */
static inline int ompi_osc_rdma_lock_acquire_exclusive (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer,
                                                        ptrdiff_t offset)
{
    int ret;

    while (1 == (ret = ompi_osc_rdma_lock_try_acquire_exclusive (module, peer, offset))) {
        ompi_osc_rdma_progress (module);
    }

    return ret;
}

/**
 * ompi_osc_rdma_lock_release_exclusive:
 *
 * @param[in] peer   - peer to unlock
 * @param[in] offset - offset into the remote peer's state segment
 *
 * @returns OMPI_SUCCESS on success or another ompi error code on failure
 *
 * This function unlocks the lock at {offset} in the remote peer's state
 * structure. It is illegal to call this function unless this process
 * holds the lock.
 */
static inline int ompi_osc_rdma_lock_release_exclusive (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer,
                                                        ptrdiff_t offset)
{
    uint64_t lock = (uint64_t) (intptr_t) peer->state + offset;
    int ret = OMPI_SUCCESS;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "releasing exclusive lock %" PRIx64 " on peer %d\n", lock, peer->rank);

    if (!ompi_osc_rdma_peer_local_state (peer)) {
        ret = ompi_osc_rdma_lock_btl_op (module, peer, lock, MCA_BTL_ATOMIC_ADD, -OMPI_OSC_RDMA_LOCK_EXCLUSIVE,
                                         false);
        if (OMPI_SUCCESS != ret) {
            abort ();
        }
    } else {
        ompi_osc_rdma_unlock_local ((ompi_osc_rdma_atomic_lock_t *)(intptr_t) lock);
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "exclusive lock released");

    return ret;
}

#endif /* OMPI_OSC_RDMA_LOCK_H */
