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

static int*
get_comm_ranks(ompi_osc_rdma_module_t *module,
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
ompi_osc_rdma_fence(int assert, ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    uint32_t incoming_reqs;
    int ret = OMPI_SUCCESS;

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "osc rdma: fence start"));

    /* can't enter an active target epoch when in a passive target epoch */
    if (module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    /* active sends are now active (we will close the epoch if NOSUCCEED is specified) */
    if (0 == (assert & MPI_MODE_NOSUCCEED)) {
        module->active_eager_send_active = true;
    }

    /* short-circuit the noprecede case */
    if (0 != (assert & MPI_MODE_NOPRECEDE)) {
        ret = module->comm->c_coll.coll_barrier(module->comm,
                                                module->comm->c_coll.coll_barrier_module);
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                             "osc rdma: fence end (short circuit)"));
        return ret;
    }

    /* try to start all the requests.  */
    ret = ompi_osc_rdma_frag_flush_all(module);
    if (OMPI_SUCCESS != ret) goto cleanup;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc rdma: fence done sending"));

    /* find out how much data everyone is going to send us.  */
    ret = module->comm->c_coll.coll_reduce_scatter_block (module->epoch_outgoing_frag_count,
                                                          &incoming_reqs, 1, MPI_UINT32_T,
                                                          MPI_SUM, module->comm,
                                                          module->comm->c_coll.coll_reduce_scatter_block_module);
    if (OMPI_SUCCESS != ret) goto cleanup;

    OPAL_THREAD_LOCK(&module->lock);

    bzero(module->epoch_outgoing_frag_count,
          sizeof(uint32_t) * ompi_comm_size(module->comm));

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc rdma: fence expects %d requests",
                         incoming_reqs));

    /* set our complete condition for incoming requests */
    module->active_incoming_frag_signal_count += incoming_reqs;

    /* wait for completion */
    while (module->outgoing_frag_count != module->outgoing_frag_signal_count ||
           module->active_incoming_frag_count < module->active_incoming_frag_signal_count) {
        opal_condition_wait(&module->cond, &module->lock);
    }

    ret = OMPI_SUCCESS;

    if (assert & MPI_MODE_NOSUCCEED) {
        /* as specified in MPI-3 p 438 3-5 the fence can end an epoch. it isn't explicitly
         * stated that MPI_MODE_NOSUCCEED ends the epoch but it is a safe assumption. */
        module->active_eager_send_active = false;
    }

 cleanup:
    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "osc rdma: fence end: %d", ret));

    OPAL_THREAD_UNLOCK(&module->lock);

    return ret;
}


int
ompi_osc_rdma_start(ompi_group_t *group,
                    int assert,
                    ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_start entering..."));

    OPAL_THREAD_LOCK(&module->lock);

    /* ensure we're not already in a start */
    if (NULL != module->sc_group || module->passive_target_access_epoch) {
        OPAL_THREAD_UNLOCK(&module->lock);
        return OMPI_ERR_RMA_SYNC;
    }

    /* save the group */
    OBJ_RETAIN(group);
    ompi_group_increment_proc_count(group);

    module->sc_group = group;    

    /* disable eager sends until we've receved the proper number of
       post messages, at which time we know all our peers are ready to
       receive messages. */
    module->active_eager_send_active = false;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "num_post_msgs = %d", module->num_post_msgs));

    /* possible we've already received a couple in messages, so
       add however many we're going to wait for */
    module->num_post_msgs -= ompi_group_size(module->sc_group);

    /* if we've already received all the post messages, we can eager
       send.  Otherwise, eager send will be enabled when
       numb_post_messages reaches 0 */
    if (0 == module->num_post_msgs) {
        module->active_eager_send_active = true;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_start complete"));

    OPAL_THREAD_UNLOCK(&module->lock);
    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_complete(ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_header_complete_t complete_req;
    int ret = OMPI_SUCCESS;
    int i;
    int *ranks = NULL;
    ompi_group_t *group;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_complete entering..."));

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

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_complete sending complete message"));

    /* for each process in group, send a control message with number
       of updates coming, then start all the requests.  Note that the
       control send is processed as another message in a fragment, so
       this might get queued until the flush_all (which is fine).

       At the same time, clean out the outgoing count for the next
       round. */
    OPAL_THREAD_UNLOCK(&module->lock);
    for (i = 0 ; i < ompi_group_size(module->sc_group) ; ++i) {
        complete_req.base.type = OMPI_OSC_RDMA_HDR_TYPE_COMPLETE;
        complete_req.base.flags = OMPI_OSC_RDMA_HDR_FLAG_VALID;
        complete_req.frag_count = module->epoch_outgoing_frag_count[ranks[i]];

        ret = ompi_osc_rdma_control_send(module, 
                                         ranks[i],
                                         &complete_req,
                                         sizeof(ompi_osc_rdma_header_complete_t));
        if (OMPI_SUCCESS != ret) goto cleanup;
    }
    OPAL_THREAD_LOCK(&module->lock);

    /* start all requests */
    ret = ompi_osc_rdma_frag_flush_all(module);
    if (OMPI_SUCCESS != ret) goto cleanup;

    /* zero the fragment counts here to ensure they are zerod */
    for (i = 0 ; i < ompi_group_size(module->sc_group) ; ++i) {
        module->epoch_outgoing_frag_count[ranks[i]] = 0;
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
    OPAL_THREAD_UNLOCK(&(module->lock));

    /* phase 2 cleanup group */
    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_complete complete"));
    free (ranks);

    return OMPI_SUCCESS;

 cleanup:
    if (NULL != ranks) free(ranks);

    OPAL_THREAD_UNLOCK(&(module->lock));

    return ret;
}


int
ompi_osc_rdma_post(ompi_group_t *group,
                   int assert,
                   ompi_win_t *win)
{
    int *ranks;
    int ret = OMPI_SUCCESS;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_header_post_t post_req;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_post entering..."));

    if (module->pw_group) {
        return OMPI_ERR_RMA_SYNC;
    }

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
        post_req.base.type = OMPI_OSC_RDMA_HDR_TYPE_POST;
        post_req.base.flags = OMPI_OSC_RDMA_HDR_FLAG_VALID;
        post_req.windx = ompi_comm_get_cid(module->comm);

        /* we don't want to send any data, since we're the exposure
           epoch only, so use an unbuffered send */
        ret = ompi_osc_rdma_control_send_unbuffered(module, ranks[i], &post_req,
                                                    sizeof(ompi_osc_rdma_header_post_t));
        if (OMPI_SUCCESS != ret) {
            break;
        }
    }

    free (ranks);

    return ret;
}


int
ompi_osc_rdma_wait(ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_group_t *group;

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_wait entering..."));

    if (NULL == module->pw_group) {
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_THREAD_LOCK(&module->lock);
    while (0 != module->num_complete_msgs ||
             module->active_incoming_frag_count < module->active_incoming_frag_signal_count) {
        opal_condition_wait(&module->cond, &module->lock);
    }

    group = module->pw_group;
    module->pw_group = NULL;
    OPAL_THREAD_UNLOCK(&module->lock);

    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                         "ompi_osc_rdma_wait complete"));

    return OMPI_SUCCESS;
}


int 
ompi_osc_rdma_test(ompi_win_t *win,
                   int *flag)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_group_t *group;
    int ret = OMPI_SUCCESS;

#if !OMPI_ENABLE_PROGRESS_THREADS
    opal_progress();
#endif

    if (NULL == module->pw_group) {
        return OMPI_ERR_RMA_SYNC;
    }

    OPAL_THREAD_LOCK(&(module->lock));

    if (0 != module->num_complete_msgs || 
           module->active_incoming_frag_count < module->active_incoming_frag_signal_count) {
        *flag = 0;
        ret = OMPI_SUCCESS;
        goto cleanup;
    } else {
        *flag = 1;

        group = module->pw_group;
        module->pw_group = NULL;

        OPAL_THREAD_UNLOCK(&(module->lock));

        ompi_group_decrement_proc_count(group);
        OBJ_RELEASE(group);

        return OMPI_SUCCESS;
    }

 cleanup:
    OPAL_THREAD_UNLOCK(&(module->lock));

    return ret;
}

