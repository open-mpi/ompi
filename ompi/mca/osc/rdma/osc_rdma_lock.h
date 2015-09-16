/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OMPI_OSC_RDMA_LOCK_H)
#define OMPI_OSC_RDMA_LOCK_H

#include "osc_rdma_types.h"
#include "osc_rdma_frag.h"

static inline int ompi_osc_rdma_trylock_local (volatile ompi_osc_rdma_lock_t *lock)
{
    return !ompi_osc_rdma_lock_cmpset (lock, 0, OMPI_OSC_RDMA_LOCK_EXCLUSIVE);
}

static inline void ompi_osc_rdma_unlock_local (volatile ompi_osc_rdma_lock_t *lock)
{
    (void) ompi_osc_rdma_lock_add (lock, -OMPI_OSC_RDMA_LOCK_EXCLUSIVE);
}

/**
 * Dummy completion function for atomic operations
 */
void ompi_osc_rdma_atomic_complete (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                    void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                    void *context, void *data, int status);

/**
 * ompi_osc_rdma_lock_acquire_shared:
 *
 * @param[in] peer      - owner of lock
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
    void *temp = &module->state->scratch_lock;
    volatile bool atomic_complete = false;
    int ret;

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output, "decrementing shared lock %" PRIx64 " by %lx\n", lock,
                         (unsigned long) value));

    /* spin until the lock has been acquired */
    if (!ompi_osc_rdma_peer_local_state (peer)) {
        if (module->selected_btl->btl_flags & MCA_BTL_FLAGS_ATOMIC_OPS) {
            ret = module->selected_btl->btl_atomic_op (module->selected_btl, peer->state_endpoint, (intptr_t) lock, peer->state_handle,
                                                       MCA_BTL_ATOMIC_ADD, value, 0, MCA_BTL_NO_ORDER, ompi_osc_rdma_atomic_complete,
                                                       (void *) &atomic_complete, NULL);
        } else {
            ret = module->selected_btl->btl_atomic_fop (module->selected_btl, peer->state_endpoint, temp, (intptr_t) lock, module->state_handle,
                                                        peer->state_handle, MCA_BTL_ATOMIC_ADD, value, 0, MCA_BTL_NO_ORDER,
                                                        ompi_osc_rdma_atomic_complete, (void *) &atomic_complete, NULL);
        }

        if (OPAL_SUCCESS == ret) {
            while (!atomic_complete) {
                ompi_osc_rdma_progress (module);
            }
        } else if (1 == OPAL_SUCCESS) {
            ret = OMPI_SUCCESS;
        }

        return ret;
    } else {
        (void) ompi_osc_rdma_lock_add ((volatile ompi_osc_rdma_lock_t *) lock, value);
    }

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
    intptr_t lock = (intptr_t) peer->state + offset;
    volatile bool atomic_complete;
    ompi_osc_rdma_lock_t *temp;
    int ret;

    /* spin until the lock has been acquired */
    if (!ompi_osc_rdma_peer_local_state (peer)) {
        ompi_osc_rdma_frag_t *frag;

        ret = ompi_osc_rdma_frag_alloc (module, 8, &frag, (char **) &temp);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
            return ret;
        }

        do {
            atomic_complete = false;
            ret = module->selected_btl->btl_atomic_fop (module->selected_btl, peer->state_endpoint, (void *) temp, lock, frag->handle,
                                                        peer->state_handle, MCA_BTL_ATOMIC_ADD, value, 0, MCA_BTL_NO_ORDER,
                                                        ompi_osc_rdma_atomic_complete, (void *) &atomic_complete, NULL);
            if (OPAL_UNLIKELY(OPAL_SUCCESS > ret)) {
                OPAL_OUTPUT_VERBOSE((10, ompi_osc_base_framework.framework_output, "failed to increment shared lock. ret: %d", ret));
                return ret;
            }

            if (1 != ret) {
                /* wait for completion of the atomic operation */
                while (!atomic_complete) {
                    ompi_osc_rdma_progress (module);
                }
            }

            OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output, "incremented shared lock 0x%lx by 0x%lx. Old value 0x%lx",
                                 (unsigned long) lock, (unsigned long) value, (unsigned long) *temp));

            if (!(*temp & check)) {
                break;
            }

            /* NTH: i think this is correct. backoff! */
            ompi_osc_rdma_lock_release_shared (module, peer, -value, offset);
            ompi_osc_rdma_progress (module);
        } while (1);

        ompi_osc_rdma_frag_complete (frag);
    } else {
        ompi_osc_rdma_lock_t lock_state;
        do {
            lock_state = ompi_osc_rdma_lock_add ((volatile ompi_osc_rdma_lock_t *) lock, value);
            OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output, "incremented local shared lock by 0x%lx. Old value 0x%lx",
                                 (unsigned long) value, (unsigned long) lock_state));
            if (!(lock_state & check)) {
                break;
            }

            (void) ompi_osc_rdma_lock_add ((volatile ompi_osc_rdma_lock_t *) lock, -value);
            ompi_osc_rdma_progress (module);
        } while (1);
    }

    return OMPI_SUCCESS;
}

/**
 * ompi_osc_rdma_lock_try_acquire_exclusive:
 *
 * @param[in] peer     - peer to lock
 * @param[in] temp     - temporary registered location for lock result
 * @param[in] temp_seg - registered segment for temp
 * @param[in] offset   - offset into the remote peer's state segment
 *
 * @returns 0 on success, 1 on failure
 *
 * This function attempts to lock the lock at {offset} on the remote
 * peer. The buffer pointer to by {temp} must not be modified until
 * this functions completes.
 */
static inline int ompi_osc_rdma_lock_try_acquire_exclusive (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer,
                                                            ptrdiff_t offset)
{
    uint64_t lock = (uint64_t) (uintptr_t) peer->state + offset;
    ompi_osc_rdma_lock_t *temp = NULL;
    volatile bool atomic_complete;
    int ret;

    if (!ompi_osc_rdma_peer_local_state (peer)) {
        ompi_osc_rdma_frag_t *frag = NULL;
        int result;

        ret = ompi_osc_rdma_frag_alloc (module, 8, &frag, (char **) &temp);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
            return ret;
        }

        /* set the temporary value so we can detect success. note that a lock should never be -1 */
        atomic_complete = false;
        ret = module->selected_btl->btl_atomic_cswap (module->selected_btl, peer->state_endpoint, temp, lock, frag->handle,
                                                      peer->state_handle, 0, OMPI_OSC_RDMA_LOCK_EXCLUSIVE, 0, 0,
                                                      ompi_osc_rdma_atomic_complete, (void *) &atomic_complete, NULL);
        if (OPAL_UNLIKELY(OPAL_SUCCESS > ret)) {
            return ret;
        }

        if (0 == ret) {
            /* wait for the atomic operation to complete */
            while (!atomic_complete) {
                ompi_osc_rdma_progress (module);
            }
        }

        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output, "got %lx when attempting compare and swap %" PRIx64 " complete %d",
                             (unsigned long) *temp, lock, atomic_complete));
        result = (*temp != 0);

        ompi_osc_rdma_frag_complete (frag);

        return result;
    }

    return ompi_osc_rdma_trylock_local ((int64_t *)(intptr_t) lock);
}

/**
 * ompi_osc_rdma_lock_acquire_exclusive:
 *
 * @param[in] peer     - peer to lock
 * @param[in] temp     - temporary registered location for lock result
 * @param[in] temp_seg - registered segment for temp
 * @param[in] offset   - offset into the remote peer's state segment
 *
 * @returns OMPI_SUCCESS on success or another ompi error code on failure
 *
 * This function locks the lock at {offset} on the remote peer. The
 * buffer pointed to by {temp} must not be modified until this
 * function completes.
 */
static inline int ompi_osc_rdma_lock_acquire_exclusive (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer,
                                                        ptrdiff_t offset)
{
    while (ompi_osc_rdma_lock_try_acquire_exclusive (module, peer, offset)) {
        ompi_osc_rdma_progress (module);
    }

    return OMPI_SUCCESS;
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
    void *temp = &module->state->scratch_lock;
    volatile bool atomic_complete = false;
    int ret;

    if (!ompi_osc_rdma_peer_local_state (peer)) {
        if (module->selected_btl->btl_flags & MCA_BTL_FLAGS_ATOMIC_OPS) {
            ret = module->selected_btl->btl_atomic_op (module->selected_btl, peer->state_endpoint, lock, peer->state_handle, MCA_BTL_ATOMIC_ADD,
                                                       -OMPI_OSC_RDMA_LOCK_EXCLUSIVE, 0, MCA_BTL_NO_ORDER, ompi_osc_rdma_atomic_complete,
                                                       (void *) &atomic_complete, NULL);
        } else {
            ret = module->selected_btl->btl_atomic_fop (module->selected_btl, peer->state_endpoint, temp, lock, module->state_handle,
                                                        peer->state_handle, MCA_BTL_ATOMIC_ADD, -OMPI_OSC_RDMA_LOCK_EXCLUSIVE, 0,
                                                        MCA_BTL_NO_ORDER, ompi_osc_rdma_atomic_complete, (void *) &atomic_complete, NULL);
        }

        if (OPAL_UNLIKELY(OMPI_SUCCESS > ret)) {
            return ret;
        }

        if (OPAL_SUCCESS == ret) {
            while (!atomic_complete) {
                ompi_osc_rdma_progress (module);
            }
        }

        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output, "unlocked target lock %" PRIx64 " with value 0x%lx. old value 0x%"
                             PRIx64, lock, (unsigned long) -OMPI_OSC_RDMA_LOCK_EXCLUSIVE, ((uint64_t *) temp)[0]));
    } else {
        ompi_osc_rdma_unlock_local ((volatile ompi_osc_rdma_lock_t *)(intptr_t) lock);
    }


    return OMPI_SUCCESS;
}

#endif /* OMPI_OSC_RDMA_LOCK_H */
