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

#include "osc_pt2pt.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_data_move.h"
#include "osc_pt2pt_frag.h"

#include "mpi.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/base/base.h"

/**
 * ompi_osc_pt2pt_pending_post_t:
 *
 * Describes a post operation that was encountered outside its
 * matching start operation.
 */
struct ompi_osc_pt2pt_pending_post_t {
    opal_list_item_t super;
    int rank;
};
typedef struct ompi_osc_pt2pt_pending_post_t ompi_osc_pt2pt_pending_post_t;
OBJ_CLASS_DECLARATION(ompi_osc_pt2pt_pending_post_t);

OBJ_CLASS_INSTANCE(ompi_osc_pt2pt_pending_post_t, opal_list_item_t, NULL, NULL);

static bool group_contains_proc (ompi_group_t *group, ompi_proc_t *proc)
{
    int group_size = ompi_group_size (group);

    for (int i = 0 ; i < group_size ; ++i) {
        ompi_proc_t *group_proc = ompi_group_peer_lookup (group, i);

        /* it is safe to compare procs by pointer */
        if (group_proc == proc) {
            return true;
        }
    }

    return false;
}

static int*
get_comm_ranks(ompi_osc_pt2pt_module_t *module,
               ompi_group_t *sub_group)
{
    int *ranks1 = NULL, *ranks2 = NULL;
    bool success = false;
    int i, ret;

    ranks1 = malloc(sizeof(int) * ompi_group_size(sub_group));
    if (NULL == ranks1) goto cleanup;
    ranks2 = malloc(sizeof(int) * ompi_group_size(sub_group));
    if (NULL == ranks2) goto cleanup;

    for (i = 0 ; i < ompi_group_size(sub_group) ; ++i) {
        ranks1[i] = i;
    }

    ret = ompi_group_translate_ranks(sub_group,
                                     ompi_group_size(sub_group),
                                     ranks1,
                                     module->comm->c_local_group,
                                     ranks2);
    if (OMPI_SUCCESS != ret) goto cleanup;

    success = true;

 cleanup:
    if (NULL != ranks1) free(ranks1);
    if (!success) {
        if (NULL != ranks2) free(ranks2);
        ranks2 = NULL;
    }

    return ranks2;
}

int
ompi_osc_pt2pt_fence(int assert, ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    uint32_t incoming_reqs;
    int ret = OMPI_SUCCESS;

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: fence start"));

    /* can't enter an active target epoch when in a passive target epoch */
    if (module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    /* active sends are now active (we will close the epoch if NOSUCCEED is specified) */
    if (0 == (assert & MPI_MODE_NOSUCCEED)) {
        module->active_eager_send_active = true;
        module->all_access_epoch = true;
    }

    /* short-circuit the noprecede case */
    if (0 != (assert & MPI_MODE_NOPRECEDE)) {
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                             "osc pt2pt: fence end (short circuit)"));
        return ret;
    }

    /* try to start all requests.  */
    ret = ompi_osc_pt2pt_frag_flush_all(module);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: fence done sending"));

    /* find out how much data everyone is going to send us.  */
    ret = module->comm->c_coll.coll_reduce_scatter_block (module->epoch_outgoing_frag_count,
                                                          &incoming_reqs, 1, MPI_UINT32_T,
                                                          MPI_SUM, module->comm,
                                                          module->comm->c_coll.coll_reduce_scatter_block_module);
    if (OMPI_SUCCESS != ret) {
        OPAL_THREAD_UNLOCK(&module->lock);
        return ret;
    }

    OPAL_THREAD_LOCK(&module->lock);
    bzero(module->epoch_outgoing_frag_count,
          sizeof(uint32_t) * ompi_comm_size(module->comm));

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: fence expects %d requests",
                         incoming_reqs));

    /* set our complete condition for incoming requests */
    module->active_incoming_frag_signal_count += incoming_reqs;

    /* wait for completion */
    while (module->outgoing_frag_count != module->outgoing_frag_signal_count ||
           module->active_incoming_frag_count < module->active_incoming_frag_signal_count) {
        opal_condition_wait(&module->cond, &module->lock);
    }

    if (assert & MPI_MODE_NOSUCCEED) {
        /* as specified in MPI-3 p 438 3-5 the fence can end an epoch. it isn't explicitly
         * stated that MPI_MODE_NOSUCCEED ends the epoch but it is a safe assumption. */
        module->active_eager_send_active = false;
        module->all_access_epoch = false;
    }
    opal_condition_broadcast (&module->cond);
    OPAL_THREAD_UNLOCK(&module->lock);

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: fence end: %d", ret));

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_start(ompi_group_t *group,
                    int assert,
                    ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_osc_pt2pt_pending_post_t *pending_post, *next;
    int group_size;
    int *ranks;

    OPAL_THREAD_LOCK(&module->lock);

    /* ensure we're not already in a start or passive target. we can no check for all
     * access here due to fence */
    if (NULL != module->sc_group || module->passive_target_access_epoch) {
        OPAL_THREAD_UNLOCK(&module->lock);
        return OMPI_ERR_RMA_SYNC;
    }

    /* save the group */
    OBJ_RETAIN(group);
    ompi_group_increment_proc_count(group);

    module->sc_group = group;

    /* mark all procs in this group as being in an access epoch */
    group_size = ompi_group_size (module->sc_group);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_start entering with group size %d...",
                         group_size));

    ranks = get_comm_ranks(module, module->sc_group);
    if (NULL == ranks) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    for (int i = 0 ; i < group_size ; ++i) {
        /* when the post comes in we will be in an access epoch with this proc */
        module->peers[ranks[i]].access_epoch = true;
    }

    free (ranks);

    OPAL_LIST_FOREACH_SAFE(pending_post, next, &module->pending_posts, ompi_osc_pt2pt_pending_post_t) {
        ompi_proc_t *pending_proc = ompi_comm_peer_lookup (module->comm, pending_post->rank);

        if (group_contains_proc (module->sc_group, pending_proc)) {
            ompi_osc_pt2pt_peer_t *peer = module->peers + pending_post->rank;

            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output, "Consumed unexpected post message from %d",
                                 pending_post->rank));
            ++module->num_post_msgs;
            peer->eager_send_active = true;

            opal_list_remove_item (&module->pending_posts, &pending_post->super);
            OBJ_RELEASE(pending_post);
        }
    }

    /* disable eager sends until we've receved the proper number of
       post messages, at which time we know all our peers are ready to
       receive messages. */
    module->active_eager_send_active = false;

    /* possible we've already received a couple in messages, so
       add however many we're going to wait for */
    module->num_post_msgs -= ompi_group_size(module->sc_group);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "num_post_msgs = %d", module->num_post_msgs));

    /* if we've already received all the post messages, we can eager
       send.  Otherwise, eager send will be enabled when
       numb_post_messages reaches 0 */
    if (0 == module->num_post_msgs) {
        module->active_eager_send_active = true;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_start complete"));

    OPAL_THREAD_UNLOCK(&module->lock);
    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_complete(ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_osc_pt2pt_header_complete_t complete_req;
    ompi_osc_pt2pt_peer_t *peer;
    int ret = OMPI_SUCCESS;
    int i;
    int *ranks = NULL;
    ompi_group_t *group;
    int my_rank = ompi_comm_rank (module->comm);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_complete entering..."));

    if (NULL == module->sc_group) {
        return OMPI_ERR_RMA_SYNC;
    }

    ranks = get_comm_ranks(module, module->sc_group);
    if (NULL == ranks) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    OPAL_THREAD_LOCK(&module->lock);

    /* wait for all the post messages */
    while (0 != module->num_post_msgs) {
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                             "waiting for post messages. num_post_msgs = %d", module->num_post_msgs));
        opal_condition_wait(&module->cond, &module->lock);
    }
    OPAL_THREAD_UNLOCK(&module->lock);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_complete sending complete messages"));

    /* for each process in group, send a control message with number
       of updates coming, then start all the requests.  Note that the
       control send is processed as another message in a fragment, so
       this might get queued until the flush_all (which is fine).

       At the same time, clean out the outgoing count for the next
       round. */
    for (i = 0 ; i < ompi_group_size(module->sc_group) ; ++i) {
        if (my_rank == ranks[i]) {
            /* shortcut for self */
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output, "ompi_osc_pt2pt_complete self complete"));
            module->num_complete_msgs++;
            continue;
        }

        complete_req.base.type = OMPI_OSC_PT2PT_HDR_TYPE_COMPLETE;
        complete_req.base.flags = OMPI_OSC_PT2PT_HDR_FLAG_VALID;
        complete_req.frag_count = module->epoch_outgoing_frag_count[ranks[i]];

        peer = module->peers + ranks[i];

        /* XXX -- TODO -- since fragment are always delivered in order we do not need to count anything but long
         * requests. once that is done this can be removed. */
        if (peer->active_frag && (peer->active_frag->remain_len < sizeof (complete_req))) {
            ++complete_req.frag_count;
        }

        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                             "ompi_osc_pt2pt_complete sending complete message to %d. frag_count: %u",
                             ranks[i], complete_req.frag_count));


        peer->access_epoch = false;

        ret = ompi_osc_pt2pt_control_send (module, ranks[i], &complete_req,
                                           sizeof(ompi_osc_pt2pt_header_complete_t));
        if (OMPI_SUCCESS != ret) goto cleanup;

        ret = ompi_osc_pt2pt_frag_flush_target (module, ranks[i]);
        if (OMPI_SUCCESS != ret) goto cleanup;
    }

    OPAL_THREAD_LOCK(&module->lock);
    /* zero the fragment counts here to ensure they are zerod */
    for (i = 0 ; i < ompi_group_size(module->sc_group) ; ++i) {
        peer = module->peers + ranks[i];
        module->epoch_outgoing_frag_count[ranks[i]] = 0;
        peer->eager_send_active = false;
    }

    /* wait for outgoing requests to complete.  Don't wait for incoming, as
       we're only completing the access epoch, not the exposure epoch */
    while (module->outgoing_frag_count != module->outgoing_frag_signal_count) {
        opal_condition_wait(&module->cond, &module->lock);
    }

    /* phase 1 cleanup group */
    group = module->sc_group;
    module->sc_group = NULL;

    /* unlock here, as group cleanup can take a while... */
    OPAL_THREAD_UNLOCK(&module->lock);

    /* phase 2 cleanup group */
    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_complete complete"));
    free (ranks);

    return OMPI_SUCCESS;

 cleanup:
    if (NULL != ranks) free(ranks);

    return ret;
}


int
ompi_osc_pt2pt_post(ompi_group_t *group,
                   int assert,
                   ompi_win_t *win)
{
    int *ranks;
    int ret = OMPI_SUCCESS;
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_osc_pt2pt_header_post_t post_req;
    int my_rank = ompi_comm_rank(module->comm);

    /* can't check for all access epoch here due to fence */
    if (module->pw_group) {
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_post entering with group size %d...",
                         ompi_group_size (group)));

    /* save the group */
    OBJ_RETAIN(group);
    ompi_group_increment_proc_count(group);

    OPAL_THREAD_LOCK(&(module->lock));

    /* ensure we're not already in a post */
    if (NULL != module->pw_group) {
        OPAL_THREAD_UNLOCK(&(module->lock));
        return OMPI_ERR_RMA_SYNC;
    }
    module->pw_group = group;

    /* Update completion counter.  Can't have received any completion
       messages yet; complete won't send a completion header until
       we've sent a post header. */
    module->num_complete_msgs = -ompi_group_size(module->pw_group);

    OPAL_THREAD_UNLOCK(&(module->lock));

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "sending post messages"));

    ranks = get_comm_ranks(module, module->pw_group);
    if (NULL == ranks) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* send a hello counter to everyone in group */
    for (int i = 0 ; i < ompi_group_size(module->pw_group) ; ++i) {
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output, "Sending post message to rank %d", ranks[i]));

        /* shortcut for self */
        if (my_rank == ranks[i]) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output, "ompi_osc_pt2pt_complete self post"));
            osc_pt2pt_incoming_post (module, my_rank);
            continue;
        }

        post_req.base.type = OMPI_OSC_PT2PT_HDR_TYPE_POST;
        post_req.base.flags = OMPI_OSC_PT2PT_HDR_FLAG_VALID;
        post_req.windx = ompi_comm_get_cid(module->comm);

        /* we don't want to send any data, since we're the exposure
           epoch only, so use an unbuffered send */
        ret = ompi_osc_pt2pt_control_send_unbuffered(module, ranks[i], &post_req,
                                                    sizeof(ompi_osc_pt2pt_header_post_t));
        if (OMPI_SUCCESS != ret) {
            break;
        }
    }

    free (ranks);

    return ret;
}


int
ompi_osc_pt2pt_wait(ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_group_t *group;

    if (NULL == module->pw_group) {
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_wait entering..."));

    OPAL_THREAD_LOCK(&module->lock);
    while (0 != module->num_complete_msgs ||
             module->active_incoming_frag_count != module->active_incoming_frag_signal_count) {
        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "num_complete_msgs = %d, active_incoming_frag_count = %d, active_incoming_frag_signal_count = %d",
                             module->num_complete_msgs, module->active_incoming_frag_count, module->active_incoming_frag_signal_count));
        opal_condition_wait(&module->cond, &module->lock);
    }

    group = module->pw_group;
    module->pw_group = NULL;
    OPAL_THREAD_UNLOCK(&module->lock);

    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_wait complete"));

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_test(ompi_win_t *win,
                   int *flag)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_group_t *group;
    int ret = OMPI_SUCCESS;

#if !OPAL_ENABLE_PROGRESS_THREADS
    opal_progress();
#endif

    if (NULL == module->pw_group) {
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_THREAD_LOCK(&(module->lock));

    if (0 != module->num_complete_msgs ||
           module->active_incoming_frag_count != module->active_incoming_frag_signal_count) {
        *flag = 0;
        ret = OMPI_SUCCESS;
    } else {
        *flag = 1;

        group = module->pw_group;
        module->pw_group = NULL;

        OPAL_THREAD_UNLOCK(&(module->lock));

        ompi_group_decrement_proc_count(group);
        OBJ_RELEASE(group);

        return OMPI_SUCCESS;
    }

    OPAL_THREAD_UNLOCK(&(module->lock));

    return ret;
}

int osc_pt2pt_incoming_post (ompi_osc_pt2pt_module_t *module, int source)
{
    ompi_proc_t *source_proc = ompi_comm_peer_lookup (module->comm, source);
    ompi_osc_pt2pt_peer_t *peer = module->peers + source;

    OPAL_THREAD_LOCK(&module->lock);

    /* verify that this proc is part of the current start group */
    if (!module->sc_group || !group_contains_proc (module->sc_group, source_proc)) {
        ompi_osc_pt2pt_pending_post_t *pending_post = OBJ_NEW(ompi_osc_pt2pt_pending_post_t);

        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                             "received unexpected post message from %d. module->sc_group = %p, size = %d",
                             source, (void*)module->sc_group, module->sc_group ? ompi_group_size (module->sc_group) : 0));

        pending_post->rank = source;

        opal_list_append (&module->pending_posts, &pending_post->super);

        OPAL_THREAD_UNLOCK(&module->lock);
        return OMPI_SUCCESS;
    }

    assert (!peer->eager_send_active);
    peer->eager_send_active = true;

    module->num_post_msgs++;
    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "received post message. num_post_msgs = %d", module->num_post_msgs));

    if (0 == module->num_post_msgs) {
        module->active_eager_send_active = true;
    }
    opal_condition_broadcast (&module->cond);
    OPAL_THREAD_UNLOCK(&module->lock);

    return OMPI_SUCCESS;
}
