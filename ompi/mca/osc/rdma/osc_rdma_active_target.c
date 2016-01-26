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
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_rdma.h"
#include "osc_rdma_frag.h"
#include "osc_rdma_active_target.h"

#include "mpi.h"
#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/base/base.h"

/**
 * ompi_osc_rdma_pending_post_t:
 *
 * Describes a post operation that was encountered outside it's
 * matching start operation.
 */
struct ompi_osc_rdma_pending_post_t {
    opal_list_item_t super;
    int rank;
};
typedef struct ompi_osc_rdma_pending_post_t ompi_osc_rdma_pending_post_t;

static OBJ_CLASS_INSTANCE(ompi_osc_rdma_pending_post_t, opal_list_item_t, NULL, NULL);

/**
 * Dummy completion function for atomic operations
 */
void ompi_osc_rdma_atomic_complete (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                    void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                    void *context, void *data, int status)
{
    volatile bool *atomic_complete = (volatile bool *) context;

    if (atomic_complete) {
        *atomic_complete = true;
    }
}

/**
 * compare_ranks:
 *
 * @param[in] ptra    Pointer to integer item
 * @param[in] ptrb    Pointer to integer item
 *
 * @returns 0 if *ptra == *ptrb
 * @returns -1 if *ptra < *ptrb
 * @returns 1 otherwise
 *
 * This function is used to sort the rank list. It can be removed if
 * groups are always in order.
 */
static int compare_ranks (const void *ptra, const void *ptrb)
{
    int a = *((int *) ptra);
    int b = *((int *) ptrb);

    if (a < b) {
        return -1;
    } else if (a > b) {
        return 1;
    }

    return 0;
}

/**
 * ompi_osc_rdma_get_comm_ranks:
 *
 * @param[in] module    - OSC RDMA module
 * @param[in] sub_group - Group with ranks to translate
 *
 * @returns an array of translated ranks on success or NULL on failure
 *
 * Translate the ranks given in {sub_group} into ranks in the
 * communicator used to create {module}.
 */
static ompi_osc_rdma_peer_t **ompi_osc_rdma_get_peers (ompi_osc_rdma_module_t *module, ompi_group_t *sub_group)
{
    int size = ompi_group_size(sub_group);
    ompi_osc_rdma_peer_t **peers;
    int *ranks1, *ranks2;
    int ret;

    ranks1 = calloc (size, sizeof(int));
    ranks2 = calloc (size, sizeof(int));
    peers = calloc (size, sizeof (ompi_osc_rdma_peer_t *));
    if (NULL == ranks1 || NULL == ranks2 || NULL == peers) {
        free (ranks1);
        free (ranks2);
        free (peers);
        return NULL;
    }

    for (int i = 0 ; i < size ; ++i) {
        ranks1[i] = i;
    }

    ret = ompi_group_translate_ranks (sub_group, size, ranks1, module->comm->c_local_group,
                                      ranks2);
    free (ranks1);
    if (OMPI_SUCCESS != ret) {
        free (ranks2);
        free (peers);
        return NULL;
    }

    qsort (ranks2, size, sizeof (int), compare_ranks);
    for (int i = 0 ; i < size ; ++i) {
        peers[i] = ompi_osc_rdma_module_peer (module, ranks2[i]);
        if (NULL == peers[i]) {
            free (peers);
            peers = NULL;
            break;
        }

        OBJ_RETAIN(peers[i]);
    }
    free (ranks2);

    return peers;
}

static void ompi_osc_rdma_release_peers (ompi_osc_rdma_peer_t **peers, int npeers)
{
    for (int i = 0 ; i < npeers ; ++i) {
        OBJ_RELEASE(peers[i]);
    }

    free (peers);
}

static void ompi_osc_rdma_handle_post (ompi_osc_rdma_module_t *module, int rank, ompi_osc_rdma_peer_t **peers, int npeers) {
    ompi_osc_rdma_state_t *state = module->state;
    ompi_osc_rdma_pending_post_t *pending_post;

    /* look for the posting peer in the group */
    for (int j = 0 ; j < npeers ; ++j) {
        if (rank == peers[j]->rank) {
            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "got expected post from %d. still expecting posts from %d processes",
                             rank, (int) (npeers - state->num_post_msgs - 1));
            ++state->num_post_msgs;
            return;
        }
    }

    /* post does not belong to this start epoch. save it for later */
    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "got unexpected post from %d . queueing for later", rank);
    pending_post = OBJ_NEW(ompi_osc_rdma_pending_post_t);
    pending_post->rank = rank;
    OPAL_THREAD_SCOPED_LOCK(&module->lock, opal_list_append (&module->pending_posts, &pending_post->super));
}

int ompi_osc_rdma_post_atomic (ompi_group_t *group, int assert, ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t **peers;
    int my_rank = ompi_comm_rank (module->comm);
    ompi_osc_rdma_state_t *state = module->state;
    volatile bool atomic_complete;
    ompi_osc_rdma_frag_t *frag = NULL;
    osc_rdma_counter_t *temp = NULL;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "post: %p, %d, %s", (void*) group, assert, win->w_name);

    /* check if we are already in a post epoch */
    if (module->pw_group) {
        return OMPI_ERR_RMA_SYNC;
    }

    /* save the group */
    OBJ_RETAIN(group);

    OPAL_THREAD_LOCK(&module->lock);

    /* ensure we're not already in a post */
    if (NULL != module->pw_group) {
        OPAL_THREAD_UNLOCK(&(module->lock));
        return OMPI_ERR_RMA_SYNC;
    }
    module->pw_group = group;

    /* Update completion counter.  Can't have received any completion
       messages yet; complete won't send a completion header until
       we've sent a post header. */
    state->num_complete_msgs = 0;
    OPAL_THREAD_UNLOCK(&module->lock);

    /* allocate a temporary buffer for atomic response */
    ret = ompi_osc_rdma_frag_alloc (module, 8, &frag, (char **) &temp);

    if ((assert & MPI_MODE_NOCHECK) || 0 == ompi_group_size (group)) {
        return OMPI_SUCCESS;
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* translate group ranks into the communicator */
    peers = ompi_osc_rdma_get_peers (module, module->pw_group);
    if (OPAL_UNLIKELY(NULL == peers)) {
        ompi_osc_rdma_frag_complete (frag);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "sending post messages");

    /* send a hello counter to everyone in group */
    for (int i = 0 ; i < ompi_group_size(module->pw_group) ; ++i) {
        ompi_osc_rdma_peer_t *peer = peers[i];
        uint64_t target = (uint64_t) (intptr_t) peer->state + offsetof (ompi_osc_rdma_state_t, post_index);
        int post_index;

        if (peer->rank == my_rank) {
            ompi_osc_rdma_handle_post (module, my_rank, NULL, 0);
            continue;
        }

        /* get a post index */
        atomic_complete = false;
        if (!ompi_osc_rdma_peer_local_state (peer)) {
            do {
                ret = module->selected_btl->btl_atomic_fop (module->selected_btl, peer->state_endpoint, temp, target, frag->handle,
                                                            peer->state_handle, MCA_BTL_ATOMIC_ADD, 1, 0, MCA_BTL_NO_ORDER,
                                                            ompi_osc_rdma_atomic_complete, (void *) &atomic_complete, NULL);
                assert (OPAL_SUCCESS >= ret);

                if (OMPI_SUCCESS == ret) {
                    while (!atomic_complete) {
                        ompi_osc_rdma_progress (module);
                    }

                    break;
                }

                ompi_osc_rdma_progress (module);
            } while (1);
        } else {
            *temp = ompi_osc_rdma_counter_add ((osc_rdma_counter_t *) (intptr_t) target, 1) - 1;
        }
        post_index = (*temp) & (OMPI_OSC_RDMA_POST_PEER_MAX - 1);

        target = (uint64_t) (intptr_t) peer->state + offsetof (ompi_osc_rdma_state_t, post_peers) +
            sizeof (osc_rdma_counter_t) * post_index;

        do {
            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "attempting to post to index %d @ rank %d", post_index, peer->rank);

            /* try to post. if the value isn't 0 then another rank is occupying this index */
            if (!ompi_osc_rdma_peer_local_state (peer)) {
                atomic_complete = false;
                ret = module->selected_btl->btl_atomic_cswap (module->selected_btl, peer->state_endpoint, temp, target, frag->handle, peer->state_handle,
                                                              0, 1 + (int64_t) my_rank, 0, MCA_BTL_NO_ORDER, ompi_osc_rdma_atomic_complete,
                                                              (void *) &atomic_complete, NULL);
                assert (OPAL_SUCCESS >= ret);

                if (OMPI_SUCCESS == ret) {
                    while (!atomic_complete) {
                        ompi_osc_rdma_progress (module);
                    }
                } else {
                    ompi_osc_rdma_progress (module);
                    continue;
                }

            } else {
                *temp = !ompi_osc_rdma_lock_cmpset ((osc_rdma_counter_t *) target, 0, 1 + (osc_rdma_counter_t) my_rank);
            }

            if (OPAL_LIKELY(0 == *temp)) {
                break;
            }

            /* prevent circular wait by checking for post messages received */
            for (int j = 0 ; j < OMPI_OSC_RDMA_POST_PEER_MAX ; ++j) {
                /* no post at this index (yet) */
                if (0 == state->post_peers[j]) {
                    continue;
                }

                ompi_osc_rdma_handle_post (module, state->post_peers[j] - 1, NULL, 0);
                state->post_peers[j] = 0;
            }

            usleep (100);
        } while (1);
    }

    ompi_osc_rdma_frag_complete (frag);

    ompi_osc_rdma_release_peers (peers, ompi_group_size(module->pw_group));

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "post complete");

    return OMPI_SUCCESS;
}

int ompi_osc_rdma_start_atomic (ompi_group_t *group, int assert, ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_pending_post_t *pending_post, *next;
    ompi_osc_rdma_state_t *state = module->state;
    ompi_osc_rdma_sync_t *sync = &module->all_sync;
    int group_size = ompi_group_size (group);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "start: %p, %d, %s", (void*) group, assert,
                     win->w_name);

    OPAL_THREAD_LOCK(&module->lock);

    /* check if we are already in an access epoch */
    if (ompi_osc_rdma_access_epoch_active (module)) {
        OPAL_THREAD_UNLOCK(&module->lock);
        return OMPI_ERR_RMA_SYNC;
    }

    /* mark all procs in this group as being in an access epoch */
    sync->num_peers = ompi_group_size (group);
    sync->sync.pscw.group = group;

    /* haven't processed any post messaes yet */
    state->num_post_msgs = 0;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "start group size %d", sync->num_peers);

    if (0 == ompi_group_size (group)) {
        /* nothing more to do. this is an empty start epoch */
        OPAL_THREAD_UNLOCK(&module->lock);
        return OMPI_SUCCESS;
    }

    opal_atomic_wmb ();

    sync->type = OMPI_OSC_RDMA_SYNC_TYPE_PSCW;

    /* prevent us from entering a passive-target, fence, or another pscw access epoch until
     * the matching complete is called */
    sync->epoch_active = true;

    /* translate the group ranks into the communicator */
    sync->peer_list.peers = ompi_osc_rdma_get_peers (module, group);
    if (NULL == sync->peer_list.peers) {
        OPAL_THREAD_UNLOCK(&module->lock);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* save the group */
    OBJ_RETAIN(group);

    if (!(assert & MPI_MODE_NOCHECK)) {
        /* look through list of pending posts */
        OPAL_LIST_FOREACH_SAFE(pending_post, next, &module->pending_posts, ompi_osc_rdma_pending_post_t) {
            for (int i = 0 ; i < group_size ; ++i) {
                ompi_osc_rdma_peer_t *peer = sync->peer_list.peers[i];

                if (pending_post->rank == peer->rank) {
                    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "found queued post from %d. still expecting posts "
                                     "from %d processes", peer->rank, (int) (group_size - state->num_post_msgs - 1));
                    opal_list_remove_item (&module->pending_posts, &pending_post->super);
                    OBJ_RELEASE(pending_post);
                    /* only one thread can process post messages so there is no need of atomics here */
                    ++state->num_post_msgs;
                    break;
                }
            }
        }

        /* wait for all post messages to arrive */
        while (state->num_post_msgs != group_size) {
            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "waiting for post messages. have %d of %d",
                             (int) state->num_post_msgs, group_size);
            for (int i = 0 ; i < OMPI_OSC_RDMA_POST_PEER_MAX ; ++i) {
                /* no post at this index (yet) */
                if (0 == state->post_peers[i]) {
                    continue;
                }

                ompi_osc_rdma_handle_post (module, state->post_peers[i] - 1, sync->peer_list.peers, group_size);
                state->post_peers[i] = 0;
            }

            ompi_osc_rdma_progress (module);
        }
    } else {
        state->num_post_msgs = group_size;
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "start complete");

    OPAL_THREAD_UNLOCK(&module->lock);
    return OMPI_SUCCESS;
}

int ompi_osc_rdma_complete_atomic (ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_sync_t *sync = &module->all_sync;
    ompi_osc_rdma_frag_t *frag = NULL;
    ompi_osc_rdma_peer_t **peers;
    void *scratch_lock = NULL;
    ompi_group_t *group;
    int group_size, ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "complete: %s", win->w_name);

    OPAL_THREAD_LOCK(&module->lock);
    if (OMPI_OSC_RDMA_SYNC_TYPE_PSCW != sync->type) {
        OPAL_THREAD_UNLOCK(&module->lock);
        return OMPI_ERR_RMA_SYNC;
    }

    /* phase 1 cleanup sync object */
    group = sync->sync.pscw.group;
    group_size = sync->num_peers;
    sync->type = OMPI_OSC_RDMA_SYNC_TYPE_NONE;
    sync->epoch_active = false;

    /* phase 2 cleanup group */
    OBJ_RELEASE(group);

    peers = sync->peer_list.peers;
    if (NULL == peers) {
        /* empty peer list */
        OPAL_THREAD_UNLOCK(&(module->lock));
        OBJ_RELEASE(group);
        return OMPI_SUCCESS;
    }

    sync->peer_list.peers = NULL;

    OPAL_THREAD_UNLOCK(&(module->lock));

    ompi_osc_rdma_sync_rdma_complete (sync);

    if (!(MCA_BTL_FLAGS_ATOMIC_OPS & module->selected_btl->btl_flags)) {
        /* need a temporary buffer for performing fetching atomics */
        ret = ompi_osc_rdma_frag_alloc (module, 8, &frag, (char **) &scratch_lock);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
            return ret;
        }
    }

    /* for each process in the group increment their number of complete messages */
    for (int i = 0 ; i < group_size ; ++i) {
        ompi_osc_rdma_peer_t *peer = peers[i];
        intptr_t target = (intptr_t) peer->state + offsetof (ompi_osc_rdma_state_t, num_complete_msgs);

        if (!ompi_osc_rdma_peer_local_state (peer)) {
            do {
                if (MCA_BTL_FLAGS_ATOMIC_OPS & module->selected_btl->btl_flags) {
                    ret = module->selected_btl->btl_atomic_op (module->selected_btl, peer->state_endpoint, target, peer->state_handle,
                                                               MCA_BTL_ATOMIC_ADD, 1, 0, MCA_BTL_NO_ORDER,
                                                               ompi_osc_rdma_atomic_complete, NULL, NULL);
                } else {
                    /* don't care about the read value so use the scratch lock */
                    ret = module->selected_btl->btl_atomic_fop (module->selected_btl, peer->state_endpoint, scratch_lock,
                                                                target, frag->handle, peer->state_handle, MCA_BTL_ATOMIC_ADD, 1,
                                                                0, MCA_BTL_NO_ORDER, ompi_osc_rdma_atomic_complete, NULL, NULL);
                }

                if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
                    break;
                }
            } while (1);
        } else {
            (void) ompi_osc_rdma_counter_add ((osc_rdma_counter_t *) target, 1);
        }
    }

    if (frag) {
        ompi_osc_rdma_frag_complete (frag);
    }

    /* release our reference to peers in this group */
    ompi_osc_rdma_release_peers (peers, group_size);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "complete complete");

    return OMPI_SUCCESS;
}

int ompi_osc_rdma_wait_atomic (ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_state_t *state = module->state;
    ompi_group_t *group;
    int group_size;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "wait: %s", win->w_name);

    OPAL_THREAD_LOCK(&module->lock);
    if (NULL == module->pw_group) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "no matching post");
        OPAL_THREAD_UNLOCK(&module->lock);
        return OMPI_ERR_RMA_SYNC;
    }

    group_size = ompi_group_size (module->pw_group);
    OPAL_THREAD_UNLOCK(&module->lock);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "waiting on complete message. have %d of %d",
                     (int) state->num_complete_msgs, group_size);

    while (group_size != state->num_complete_msgs) {
        ompi_osc_rdma_progress (module);
        opal_atomic_mb ();
    }

    OPAL_THREAD_LOCK(&module->lock);
    state->num_complete_msgs = 0;
    group = module->pw_group;
    module->pw_group = NULL;
    OPAL_THREAD_UNLOCK(&module->lock);

    OBJ_RELEASE(group);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "wait complete");

    return OMPI_SUCCESS;
}


int ompi_osc_rdma_test_atomic (ompi_win_t *win, int *flag)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_state_t *state = module->state;
    ompi_group_t *group;
    int group_size;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "test: %s", win->w_name);

    OPAL_THREAD_LOCK(&module->lock);
    if (NULL == module->pw_group) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "no matching post");
        OPAL_THREAD_UNLOCK(&module->lock);
        return OMPI_ERR_RMA_SYNC;
    }

    group_size = ompi_group_size (module->pw_group);

    *flag = (group_size == state->num_complete_msgs);
    OPAL_THREAD_UNLOCK(&module->lock);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "checking on complete message. have %d of %d",
                     (int) state->num_complete_msgs, group_size);

    if (!*flag) {
        ompi_osc_rdma_progress (module);
        return OMPI_SUCCESS;
    }

    state->num_complete_msgs = 0;

    OPAL_THREAD_LOCK(&(module->lock));
    group = module->pw_group;
    module->pw_group = NULL;
    OPAL_THREAD_UNLOCK(&(module->lock));

    OBJ_RELEASE(group);

    return OMPI_SUCCESS;
}

int ompi_osc_rdma_fence_atomic (int assert, ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    int ret = OMPI_SUCCESS;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "fence: %d, %s", assert, win->w_name);

    /* can't enter an active target epoch while a lock is active */
    if (ompi_osc_rdma_in_passive_epoch (module) || module->pw_group) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "can not start fence epoch due to conflicting epoch");
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_THREAD_LOCK(&module->lock);

    /* active sends are now active (we will close the epoch if NOSUCCEED is specified) */
    if (0 == (assert & MPI_MODE_NOSUCCEED)) {
        module->all_sync.type = OMPI_OSC_RDMA_SYNC_TYPE_FENCE;
        module->all_sync.num_peers = ompi_comm_size (module->comm);
        /* NTH: should add a fast access array for peers here later. for now just use the
         * hash table. */
    }

    /* technically it is possible to enter a lock epoch (which will close the fence epoch) if
     * no communication has occurred. this flag will be set on the next put, get, accumulate, etc. */
    module->all_sync.epoch_active = false;

    /* short-circuit the noprecede case */
    if (0 != (assert & MPI_MODE_NOPRECEDE)) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "fence complete (short circuit)");
        /* no communication can occur until a peer has entered the same fence epoch. for now
         * a barrier is used to ensure this is the case. */
        ret = module->comm->c_coll.coll_barrier(module->comm, module->comm->c_coll.coll_barrier_module);
        OPAL_THREAD_UNLOCK(&module->lock);
        return ret;
    }

    ompi_osc_rdma_sync_rdma_complete (&module->all_sync);

    /* ensure all writes to my memory are complete */
    ret = module->comm->c_coll.coll_barrier(module->comm, module->comm->c_coll.coll_barrier_module);

    if (assert & MPI_MODE_NOSUCCEED) {
        /* as specified in MPI-3 p 438 3-5 the fence can end an epoch. it isn't explicitly
         * stated that MPI_MODE_NOSUCCEED ends the epoch but it is a safe assumption. */
        module->all_sync.type = OMPI_OSC_RDMA_SYNC_TYPE_NONE;
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "fence complete");

    OPAL_THREAD_UNLOCK(&module->lock);

    return ret;
}
