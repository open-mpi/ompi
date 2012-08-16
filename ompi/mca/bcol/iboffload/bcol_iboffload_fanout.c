/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>

#include "bcol_iboffload.h"
#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_task.h"
#include "bcol_iboffload_collfrag.h"
#include "bcol_iboffload_endpoint.h"

static int mca_bcol_iboffload_fanout_leader_progress(
                mca_bcol_iboffload_module_t *iboffload,
                struct mca_bcol_iboffload_collreq_t *coll_request)
{
    int rc = OMPI_SUCCESS, leader_rank = 0, rank,
        sbgp_size = iboffload->ibnet->super.group_size;

    struct mqe_task *last_send = NULL;
    mca_bcol_iboffload_task_t *send_task = NULL;
    mca_bcol_iboffload_frag_t *send_fragment = NULL;

    struct mqe_task **mqe_ptr_to_set;
    mca_bcol_iboffload_collfrag_t *coll_fragment;

    coll_fragment = (mca_bcol_iboffload_collfrag_t *)
                         opal_list_get_last(&coll_request->work_requests);

    mqe_ptr_to_set = &coll_fragment->to_post;

    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
               iboffload, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));
        goto out_of_resources;
    }

    for (rank = leader_rank + 1; rank < sbgp_size; ++rank) {
        /* post send */
        send_fragment = mca_bcol_iboffload_get_send_frag(coll_request,
                                      rank, coll_request->qp_index, 0,
                                      0, SBUF, MCA_BCOL_IBOFFLOAD_SEND_FRAG_DUMMY);
        if(NULL == send_fragment) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting and packing send frag.\n"));
            goto out_of_resources;
        }

        send_task = mca_bcol_iboffload_get_send_task(iboffload, rank, MCA_BCOL_IBOFFLOAD_QP_BARRIER,
                                                     send_fragment, coll_fragment, INLINE);
        if(NULL == send_task) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting send task.\n"));
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST(mqe_ptr_to_set, send_task, last_send);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, send_task);
    }

   /* end of list */
    *mqe_ptr_to_set = NULL;
    assert(NULL != last_send);

    last_send->flags |= MQE_WR_FLAG_SIGNAL;

    coll_fragment->signal_task_wr_id = last_send->wr_id;
    last_send->wr_id = (uint64_t) (uintptr_t) coll_fragment;

    /* post the mwr */
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload, coll_fragment->to_post);
    if(OMPI_SUCCESS != rc) {
        IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload->super, coll_request->order_info);

    return OMPI_SUCCESS;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Fan-in, adding collfrag to collfrag_pending"));
    return mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload);
}

static int mca_bcol_iboffload_fanout_proxy_progress(
                mca_bcol_iboffload_module_t *iboffload,
                struct mca_bcol_iboffload_collreq_t *coll_request)
{
    int rc = OMPI_SUCCESS, leader_rank = 0;

    struct mqe_task *last_wait = NULL;
    mca_bcol_iboffload_task_t *wait_task = NULL;
    mca_bcol_iboffload_frag_t *preposted_recv_frag = NULL;

    struct mqe_task **mqe_ptr_to_set;
    mca_bcol_iboffload_collfrag_t *coll_fragment;

    coll_fragment = (mca_bcol_iboffload_collfrag_t *)
                         opal_list_get_last(&coll_request->work_requests);

    mqe_ptr_to_set = &coll_fragment->to_post;

    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
               iboffload, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));
        goto out_of_resources;
    }

     /* post wait */
    preposted_recv_frag = mca_bcol_iboffload_get_preposted_recv_frag(
                                        iboffload, leader_rank, coll_request->qp_index);
    if(NULL == preposted_recv_frag) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting prepost recv frag.\n"));
        goto out_of_resources;
    }

    wait_task = mca_bcol_iboffload_get_wait_task(iboffload, leader_rank, 1,
                             preposted_recv_frag, coll_request->qp_index, NULL);
    if(NULL == wait_task) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
        goto out_of_resources;
    }

    APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

   /* end of list */
    *mqe_ptr_to_set = NULL;

    last_wait->flags |= MQE_WR_FLAG_SIGNAL;

    coll_fragment->signal_task_wr_id = last_wait->wr_id;
    last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;

    /* post the mwr */
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload, coll_fragment->to_post);
    if(OMPI_SUCCESS != rc) {
        IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload->super, coll_request->order_info);

    return OMPI_SUCCESS;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Fan-in, adding collfrag to collfrag_pending"));
    return mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload);
}

static int mca_bcol_iboffload_fanout_init(
                bcol_function_args_t *input_args,
                mca_bcol_iboffload_module_t *iboffload,
                struct mca_bcol_iboffload_collreq_t **coll_request)
{
    int rc;

    ompi_free_list_item_t *item = NULL;
    mca_bcol_iboffload_collfrag_t *coll_fragment = NULL;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    IBOFFLOAD_VERBOSE(10, ("Calling for mca_bcol_iboffload_barrier_init"));

    OMPI_FREE_LIST_WAIT(&cm->collreqs_free, item, rc);
    if(OMPI_SUCCESS != rc) {
        IBOFFLOAD_VERBOSE(10, ("Failing for coll request free list waiting.\n"));
        return rc;
    }

    (*coll_request) = (mca_bcol_iboffload_collreq_t *) item;
    (*coll_request)->progress_fn = iboffload->fanout_algth;

    (*coll_request)->completion_cb_fn = NULL;
    (*coll_request)->order_info = &input_args->order_info;

    (*coll_request)->module = iboffload;
    (*coll_request)->ml_buffer_index = input_args->buffer_index;
    (*coll_request)->buffer_info[SBUF].offset = 0;
    (*coll_request)->buffer_info[RBUF].offset = 0;
    (*coll_request)->qp_index = MCA_BCOL_IBOFFLOAD_QP_BARRIER;

    /* finish initializing full message descriptor */
    (*coll_request)->n_fragments  = 1;
    (*coll_request)->n_frags_sent = 1;

    (*coll_request)->n_frag_mpi_complete = 0;
    (*coll_request)->n_frag_net_complete = 0;

    (*coll_request)->user_handle_freed = false;

    input_args->bcol_opaque_data = (void *) (*coll_request);

    /*
     * setup collective work request
     */

    /* get collective frag */
    coll_fragment = &(*coll_request)->first_collfrag;
    mca_bcol_iboffload_collfrag_init(coll_fragment);

    coll_fragment->alg = FANOUT_ALG;
    coll_fragment->mq_index = COLL_MQ;

    /* Set mq credits */
    coll_fragment->mq_credits = iboffload->alg_task_consump[FANOUT_ALG];

    /* set pointers for (coll frag) <-> (coll full request) */
    MCA_BCOL_IBOFFLOAD_SET_COLL_REQ_LINKS(*coll_request, coll_fragment);

    return OMPI_SUCCESS;
}

/************************************************************************
 ************************ New style Fan-In ******************************
 ***********************************************************************/
static int mca_bcol_iboffload_new_style_fanout_progress(
                        bcol_function_args_t *input_args,
                        struct coll_ml_function_t *const_args)
{
    mca_bcol_iboffload_collreq_t *coll_request =
                 (mca_bcol_iboffload_collreq_t *)
                                   input_args->bcol_opaque_data;

    if (BCOL_IS_COMPLETED(coll_request)) {
        coll_request->user_handle_freed = true;
        if (COLLREQ_IS_DONE(coll_request)) {
            IBOFFLOAD_VERBOSE(10, ("Coll request already done.\n"));
            RELEASE_COLLREQ(coll_request);
        }

        IBOFFLOAD_VERBOSE(10, ("Fan-Out already done.\n"));
        return BCOL_FN_COMPLETE;
    }

    return BCOL_FN_STARTED;
}

int mca_bcol_iboffload_new_style_fanout_first_call(
                mca_bcol_iboffload_module_t *iboffload,
                struct mca_bcol_iboffload_collreq_t *coll_request)
{
    int i = 0, leader_rank = 0, /* We always suppose - the lowest index is a leader */
        my_rank = iboffload->ibnet->super.my_index,
        sbgp_size = iboffload->ibnet->super.group_size;

    mca_bcol_iboffload_endpoint_t *ep = NULL;
    mca_sbgp_ibnet_proc_t *my_ibnet_proc = iboffload->endpoints[my_rank]->ibnet_proc;

    assert(NULL != my_ibnet_proc);

    if (MCA_SBGP_IBNET_NODE_LEADER == my_ibnet_proc->duty) {
        iboffload->fanout_algth = mca_bcol_iboffload_fanout_leader_progress;
        iboffload->alg_task_consump[FANOUT_ALG] += sbgp_size;

        for (i = leader_rank + 1; i < sbgp_size; ++i) {
            ep = iboffload->endpoints[i];
            while (OMPI_SUCCESS !=
                    check_endpoint_state(ep, NULL, NULL)) {
                opal_progress();
            }
        }
    } else {
        iboffload->fanout_algth = mca_bcol_iboffload_fanout_proxy_progress;
        iboffload->alg_task_consump[FANOUT_ALG] += 1;

        ep = iboffload->endpoints[leader_rank];
        while(OMPI_SUCCESS !=
                check_endpoint_state(ep, NULL, NULL)) {
            opal_progress();
        }
    }

    return iboffload->fanout_algth(iboffload, coll_request);
}

static int mca_bcol_iboffload_new_style_fanout_intra(
                                bcol_function_args_t *input_args,
                                struct coll_ml_function_t *const_args)
{
    int rc = OMPI_SUCCESS;

    struct mca_bcol_iboffload_collreq_t *coll_request = NULL;
    mca_bcol_iboffload_module_t *iboffload =
                    (mca_bcol_iboffload_module_t *) const_args->bcol_module;

    assert(NULL != iboffload);

    MCA_BCOL_CHECK_ORDER(const_args->bcol_module, input_args);

    /* Init Fan-In collective reqeust */
    rc = mca_bcol_iboffload_fanout_init(input_args, iboffload, &coll_request);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Error from mca_bcol_iboffload_fanin_init.\n"));
        return BCOL_FN_NOT_STARTED;
    }

    rc = iboffload->fanout_algth(iboffload, coll_request);
    if (OPAL_UNLIKELY(OMPI_ERROR == rc)) {
        return BCOL_FN_NOT_STARTED;
    }

    return BCOL_FN_STARTED;
}

int mca_bcol_iboffload_fanout_register(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    IBOFFLOAD_VERBOSE(10, ("Register iboffload Fan-In.\n"));

    comm_attribs.bcoll_type = BCOL_FANOUT;

    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1024 * 1024;
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000; /* range 1 */

    inv_attribs.datatype_bitmap = 0xffffffff;
    inv_attribs.op_types_bitmap = 0xffffffff;

    comm_attribs.data_src = DATA_SRC_KNOWN;

    mca_bcol_base_set_attributes(super,
        &comm_attribs, &inv_attribs,
        mca_bcol_iboffload_new_style_fanout_intra,
        mca_bcol_iboffload_new_style_fanout_progress);

    return OMPI_SUCCESS;
}
