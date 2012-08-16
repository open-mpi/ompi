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
#include "bcol_iboffload_collreq.h"
#include "bcol_iboffload_collfrag.h"
#include "bcol_iboffload_endpoint.h"
#include "ompi/mca/coll/ml/coll_ml_allocation.h"

static int mca_bcol_iboffload_barrier_init(
        bcol_function_args_t *input_args,
        mca_bcol_iboffload_module_t *iboffload,
        collective_message_completion_callback_function cb_fn,
        struct mca_bcol_iboffload_collreq_t **coll_request);

/**
 * Start barrier
 */

int mca_bcol_iboffload_barrier_intra_recursive_doubling(
        mca_bcol_iboffload_module_t *iboffload,
        struct mca_bcol_iboffload_collreq_t *coll_request)
{
    /* local variables */
    mca_bcol_iboffload_task_t *send_task = NULL,
                              *wait_task = NULL;

    struct mqe_task **mqe_ptr_to_set = NULL;
    mca_bcol_iboffload_collfrag_t *coll_fragment = NULL;

    struct mqe_task *last_wait = NULL, /* we need ask from completion on last wait */
                    *last_send = NULL; /* If it no wait, we need ask for completion on last send */

    int rc, exchange, extra_rank, pair_rank;


    mca_bcol_iboffload_frag_t *send_fragment = NULL,
                              *preposted_recv_frag = NULL;

    mca_common_netpatterns_pair_exchange_node_t *my_exchange_node =
                                          &iboffload->recursive_doubling_tree;

    IBOFFLOAD_VERBOSE(10, ("Calling for mca_bcol_iboffload_barrier_intra_recursive_doubling.\n"));

    coll_fragment = (mca_bcol_iboffload_collfrag_t *)
                         opal_list_get_last(&coll_request->work_requests);
    /* Set mq credits */
    coll_fragment->mq_credits = iboffload->alg_task_consump[RECURSIVE_DOUBLING_BARRIER_ALG];

    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                 iboffload, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));

        goto out_of_resources;
    }

    coll_fragment->alg = RECURSIVE_DOUBLING_BARRIER_ALG;

    /*
     * NOTE: need to generate template, if this will be a multiple fragment
     * message.  This way we can progress the collective w/o knowing it's
     * type - actually, this is not the case for barrier, but just a note
     * to remind us that we need to generalize this.
     */

    mqe_ptr_to_set = &coll_fragment->to_post;

    /*
     * Fill in the communication pattern
     */

    /*
     * If non power of 2, may need to wait for message from "extra" proc.
     */

    if (0 < my_exchange_node->n_extra_sources) {
        if (EXCHANGE_NODE == my_exchange_node->node_type) {
            /* I will participate in the exchange (of the algorithm) -
             * wait for signal from extra process */
            extra_rank = my_exchange_node->rank_extra_source;
            preposted_recv_frag = mca_bcol_iboffload_get_preposted_recv_frag(
                    iboffload, extra_rank, coll_request->qp_index);

            if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
                IBOFFLOAD_VERBOSE(10, ("Non power of 2 case: "
                                   "Failing for getting prepost recv frag.\n"));
                goto out_of_resources;
            }

            wait_task = mca_bcol_iboffload_get_wait_task(iboffload,
                    extra_rank, 1, preposted_recv_frag, coll_request->qp_index, NULL);
            if (OPAL_UNLIKELY(NULL == wait_task)) {
                IBOFFLOAD_VERBOSE(10, ("Non power of 2 case: "
                                   "Failing for getting wait task.\n"));
                goto out_of_resources;
            }

            APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
            MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);
        }  else {
            /* I will not participate in the exchange - so just "register" as here */
            extra_rank = my_exchange_node->rank_extra_source;
            /* send - no need to send any data, in-order delivery */
            send_fragment = mca_bcol_iboffload_get_send_frag(coll_request,
                                    extra_rank, coll_request->qp_index, 0,
                                    0, SBUF,MCA_BCOL_IBOFFLOAD_SEND_FRAG_DUMMY);

            send_task = mca_bcol_iboffload_get_send_task(iboffload, extra_rank,
                    coll_request->qp_index, send_fragment, coll_fragment, INLINE);
            if (OPAL_UNLIKELY(NULL == send_task)) {
                IBOFFLOAD_VERBOSE(10, ("Non power of 2 case: "
                                   "Failing for getting send task.\n"));
                goto out_of_resources;
            }

            APPEND_TO_TASKLIST(mqe_ptr_to_set, send_task, last_send);
            MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, send_task);
        }
    }

    /* loop over exchange send/recv pairs */
    for (exchange = 0; exchange < my_exchange_node->n_exchanges; ++exchange) {
        /* rank of exchange partner */
        pair_rank = my_exchange_node->rank_exchanges[exchange];
        /* post send */
        send_fragment = mca_bcol_iboffload_get_send_frag(coll_request,
                                 pair_rank, coll_request->qp_index, 0,
                                 0, SBUF, MCA_BCOL_IBOFFLOAD_SEND_FRAG_DUMMY);

        assert(NULL != send_fragment);

        send_task = mca_bcol_iboffload_get_send_task(iboffload, pair_rank,
                                                     coll_request->qp_index,
                                                     send_fragment, coll_fragment, INLINE);
        if (OPAL_UNLIKELY(NULL == send_task)) {
            IBOFFLOAD_VERBOSE(10, ("Exchaging: "
                               "Failing for getting send task.\n"));
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST(mqe_ptr_to_set, send_task, last_send);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, send_task);

        /* post wait */
        preposted_recv_frag = mca_bcol_iboffload_get_preposted_recv_frag(
                iboffload, pair_rank, coll_request->qp_index);
        if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
            IBOFFLOAD_VERBOSE(10, ("Exchaging: "
                               "Failing for getting prepost recv frag.\n"));
            goto out_of_resources;
        }

        wait_task = mca_bcol_iboffload_get_wait_task(iboffload, pair_rank, 1,
                                                     preposted_recv_frag,
                                                     coll_request->qp_index, NULL);
        if (OPAL_UNLIKELY(NULL == wait_task)) {
            IBOFFLOAD_VERBOSE(10, ("Exchaging: "
                               "Failing for getting wait task.\n"));
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);
    }

    /* if non power of 2, may need to send message to "extra" proc */
    if (0 < my_exchange_node->n_extra_sources)  {
        if (EXTRA_NODE == my_exchange_node->node_type) {
            /* I will not participate in the exchange -
             * wait for signal from exchange process */
            extra_rank = my_exchange_node->rank_extra_source;
            /* post wait */
            preposted_recv_frag =
                mca_bcol_iboffload_get_preposted_recv_frag(iboffload, extra_rank,
                                                           coll_request->qp_index);
            if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
                IBOFFLOAD_VERBOSE(10, ("Sending to 'extra' node: "
                                   "Failing for getting prepost recv frag.\n"));
                goto out_of_resources;
            }

            wait_task = mca_bcol_iboffload_get_wait_task(iboffload, extra_rank, 1,
                                                         preposted_recv_frag,
                                                         coll_request->qp_index, NULL);
            if (OPAL_UNLIKELY(NULL == wait_task)) {
                IBOFFLOAD_VERBOSE(10, ("Sending to 'extra' node: "
                                   "Failing for getting wait task.\n"));
                goto out_of_resources;
            }

            APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
            MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

        }  else {
            /* I will participate in the exchange -
             * send signal to extra process */
            extra_rank = my_exchange_node->rank_extra_source;
            send_fragment = mca_bcol_iboffload_get_send_frag(coll_request,
                                    extra_rank, coll_request->qp_index, 0,
                                    0, SBUF, MCA_BCOL_IBOFFLOAD_SEND_FRAG_DUMMY);

            send_task = mca_bcol_iboffload_get_send_task(
                                                iboffload, extra_rank,
                                                coll_request->qp_index,
                                                send_fragment, coll_fragment, INLINE);
            if (OPAL_UNLIKELY(NULL == send_task)) {
                IBOFFLOAD_VERBOSE(10, ("Sending to 'extra' node: "
                                   "Failing for getting send task.\n"));
                goto out_of_resources;
            }

            APPEND_TO_TASKLIST(mqe_ptr_to_set, send_task, last_send);
            MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, send_task);
        }
    }

    /* Fill in the the rest of the coll_fragment */
    IBOFFLOAD_VERBOSE(10, ("Fill in the the rest of the coll_fragment.\n"));
    /* end of list */
    *mqe_ptr_to_set = NULL;

    /* finish initializing full message descriptor */
    coll_request->n_fragments  = 1;
    coll_request->n_frags_sent = 1;

    coll_request->n_frag_mpi_complete = 0;
    coll_request->n_frag_net_complete = 0;

    coll_request->user_handle_freed = false;

    last_wait->flags |= MQE_WR_FLAG_SIGNAL;
    coll_fragment->signal_task_wr_id = last_wait->wr_id;
    last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;

    /* post the mwr */
    if (MCA_BCOL_IBOFFLOAD_QP_SYNC != coll_request->qp_index) {
        rc = mca_bcol_iboffload_post_mqe_tasks(iboffload, coll_fragment->to_post);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
            /* Note: need to clean up */
            return rc;
        }

        MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload->super, coll_request->order_info);
    } else {
        /* Special flow for ML service barrier , only this function supposed to
         post service requests */
        struct mqe_task *bad_mqe = NULL;
        assert (MCA_BCOL_IBOFFLOAD_QP_SYNC == coll_request->qp_index );
        /* Post to special service MQ - 1 */
        rc = mqe_post_task(iboffload->mq[1], coll_fragment->to_post, &bad_mqe);
        if (OPAL_UNLIKELY(0 != rc)) {
            IBOFFLOAD_ERROR(("ibv_post_mqe failed on device (%s), errno says: %s,"
                        " the return code is [%d]\n",
                        ibv_get_device_name(iboffload->device->dev.ib_dev),
                        strerror(errno), rc));
            return OMPI_ERROR;
        }
    }

    IBOFFLOAD_VERBOSE(10, ("Return success.\n"));
    return OMPI_SUCCESS;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Barrier, adding collfrag to collfrag_pending.\n"));
    return mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload);
}

int mca_bcol_iboffload_barrier_intra_recursive_doubling_start(
        struct mca_bcol_iboffload_module_t *iboffload,
        struct mca_bcol_iboffload_collreq_t *coll_request)
{
    int rc;

    rc = mca_bcol_iboffload_rec_doubling_start_connections(iboffload);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    iboffload->barrier_algth =
                       mca_bcol_iboffload_barrier_intra_recursive_doubling;
    return
       mca_bcol_iboffload_barrier_intra_recursive_doubling(iboffload, coll_request);
}

int mca_bcol_iboffload_nb_memory_service_barrier_start(
        struct mca_bcol_iboffload_module_t *iboffload,
        struct mca_bcol_iboffload_collreq_t *coll_request)
{
    int rc;

    rc = mca_bcol_iboffload_rec_doubling_start_connections(iboffload);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    iboffload->memsync_algth =
        mca_bcol_iboffload_barrier_intra_recursive_doubling;

    return
        mca_bcol_iboffload_barrier_intra_recursive_doubling
        (iboffload, coll_request);
}

int mca_bcol_iboffload_nb_memory_service_barrier_intra(bcol_function_args_t *input_args,
        struct coll_ml_function_t *const_args)
{

    /* local variables */
    int rc;
    mca_bcol_iboffload_collreq_t *coll_request;
    mca_bcol_iboffload_module_t *iboffload =
                    (mca_bcol_iboffload_module_t *) const_args->bcol_module;
    /*
     * recursive doubling
     */


    IBOFFLOAD_VERBOSE(10, ("Memory syncranization barrier was started\n"));

    /* init barrier collective request */
    rc = mca_bcol_iboffload_barrier_init(input_args, iboffload, NULL, &coll_request);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("Get error from mca_bcol_iboffload_barrier_init"));
        return rc;
    }

    /* set the qp index to special qp that is used only for synchronization */
    coll_request->qp_index = MCA_BCOL_IBOFFLOAD_QP_SYNC;
    /* overwrite mq index to run over service setup */
    coll_request->first_collfrag.mq_index = SERVICE_MQ;

    /* start the barrier */
    rc = iboffload->memsync_algth(iboffload, coll_request);
    if (OPAL_UNLIKELY(OMPI_ERROR == rc)) {
      return rc;
    }

    /* complete the barrier - progress releases full request descriptors */
    IBOFFLOAD_VERBOSE(10, ("Memory syncranization barrier was started\n"));

    /* done */
    return BCOL_FN_STARTED;
}

/* Recursive K - ing*/
static int recursive_knomial_start_connections(struct mca_bcol_iboffload_module_t *iboffload)
{
    mca_common_netpatterns_k_exchange_node_t *my_exchange_node =
        &iboffload->knomial_exchange_tree;
    int k, i, n_exchanges = my_exchange_node->n_exchanges,
        **exchanges = my_exchange_node->rank_exchanges,
        n_extra_src = my_exchange_node->n_extra_sources,
        tree_order = my_exchange_node->tree_order - 1,
        rank_extra_src;

    mca_bcol_iboffload_endpoint_t *ep;

    iboffload->alg_task_consump[RECURSIVE_KNOMIAL_BARRIER_ALG] += 0;

    IBOFFLOAD_VERBOSE(10, ("\nMy sbgp rank (index) - %d, "
                "num of endpoints = %d, iboffload module - %p"
                " extra n %d, n_exchanges %d",
                iboffload->ibnet->super.my_index, iboffload->num_endpoints, iboffload,
                n_extra_src, n_exchanges));
    if (0 < n_extra_src) {
        for (k = 0; k < n_extra_src; k++) {
            iboffload->alg_task_consump[RECURSIVE_KNOMIAL_BARRIER_ALG] += 2; /* One send task one wait */
            rank_extra_src = my_exchange_node->rank_extra_sources_array[k];
            ep = iboffload->endpoints[rank_extra_src];
            if (iboffload->ibnet->super.my_index < ep->index) {
                while(0 == (ep)->remote_zero_rdma_addr.addr) {
                    opal_progress();
                }
            } else {
                IBOFFLOAD_VERBOSE(10, ("Trying to connect - %d", ep->index));
                while (OMPI_SUCCESS !=
                        check_endpoint_state(ep, NULL, NULL)) {
                    opal_progress();
                }
            }
        }
    }

    for (i = 0; i < n_exchanges; ++i) {
        for (k = 0; k < tree_order; k++) {
            iboffload->alg_task_consump[RECURSIVE_KNOMIAL_BARRIER_ALG] += 2; /* One send task one wait */
            ep = iboffload->endpoints[exchanges[i][k]];

            IBOFFLOAD_VERBOSE(10, ("Trying to connect - %d", ep->index));
            if (iboffload->ibnet->super.my_index < ep->index) {
                while(0 == (ep)->remote_zero_rdma_addr.addr) {
                    opal_progress();
                }
            } else {
                while (OMPI_SUCCESS !=
                        check_endpoint_state(ep, NULL, NULL)) {
                    opal_progress();
                }
            }
        }
    }

    return OMPI_SUCCESS;
}

static int mca_bcol_iboffload_barrier_intra_recursive_knomial(
        mca_bcol_iboffload_module_t *iboffload,
        struct mca_bcol_iboffload_collreq_t *coll_request)
{
    /* local variables */
    mca_bcol_iboffload_task_t *send_task = NULL,
                              *wait_task = NULL;

    struct mqe_task **mqe_ptr_to_set = NULL;
    mca_bcol_iboffload_collfrag_t *coll_fragment = NULL;

    struct mqe_task *last_wait = NULL, /* we need ask from completion on last wait */
                    *last_send = NULL; /* If it no wait, we need ask for completion on last send */

    int rc, exchange, extra_rank, pair_rank, k;


    mca_bcol_iboffload_frag_t *send_fragment = NULL,
                              *preposted_recv_frag = NULL;

    mca_common_netpatterns_k_exchange_node_t *my_exchange_node =
        &iboffload->knomial_exchange_tree;
    IBOFFLOAD_VERBOSE(10, ("Calling for mca_bcol_iboffload_barrier_intra_recursive_knomial. Node type %d\n", my_exchange_node->node_type));

    coll_fragment = (mca_bcol_iboffload_collfrag_t *)
                         opal_list_get_last(&coll_request->work_requests);

    /* Set mq credits */
    coll_fragment->mq_credits = iboffload->alg_task_consump[RECURSIVE_KNOMIAL_BARRIER_ALG];

    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                 iboffload, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));

        goto out_of_resources;
    }

    coll_fragment->alg = RECURSIVE_KNOMIAL_BARRIER_ALG;

    /*
     * NOTE: need to generate template, if this will be a multiple fragment
     * message.  This way we can progress the collective w/o knowing it's
     * type - actually, this is not the case for barrier, but just a note
     * to remind us that we need to generalize this.
     */

    mqe_ptr_to_set = &coll_fragment->to_post;

    /*
     * Fill in the communication pattern
     */

    /*
     * If non power of 2, may need to wait for message from "extra" proc.
     */

    if (0 < my_exchange_node->n_extra_sources) {
        if (EXCHANGE_NODE == my_exchange_node->node_type) {
            /* I will participate in the exchange (of the algorithm) -
             * wait for signal from extra process */
            for (k = 0; k < my_exchange_node->n_extra_sources; k++) {
                extra_rank = my_exchange_node->rank_extra_sources_array[k];
                IBOFFLOAD_VERBOSE(10,("Exchange [ %d ] extra get %d", k, extra_rank));

                preposted_recv_frag = mca_bcol_iboffload_get_preposted_recv_frag(
                        iboffload, extra_rank, coll_request->qp_index);

                if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
                    IBOFFLOAD_VERBOSE(10, ("Non power of 2 case: "
                                "Failing for getting prepost recv frag.\n"));
                    goto out_of_resources;
                }

                wait_task = mca_bcol_iboffload_get_wait_task(iboffload,
                        extra_rank, 1, preposted_recv_frag, coll_request->qp_index, NULL);
                if (OPAL_UNLIKELY(NULL == wait_task)) {
                    IBOFFLOAD_VERBOSE(10, ("Non power of 2 case: "
                                "Failing for getting wait task.\n"));
                    goto out_of_resources;
                }

                APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
                MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);
            }
        }  else {
            /* I will not participate in the exchange - so just "register" as here */
            extra_rank = my_exchange_node->rank_extra_sources_array[0];
            IBOFFLOAD_VERBOSE(10,("Send to proxy %d", extra_rank));
            /* send - no need to send any data, in-order delivery */
            send_fragment = mca_bcol_iboffload_get_send_frag(coll_request,
                                    extra_rank, coll_request->qp_index, 0,
                                    0, SBUF, MCA_BCOL_IBOFFLOAD_SEND_FRAG_DUMMY);

            send_task = mca_bcol_iboffload_get_send_task(iboffload, extra_rank,
                    coll_request->qp_index, send_fragment, coll_fragment, INLINE);
            if (OPAL_UNLIKELY(NULL == send_task)) {
                IBOFFLOAD_VERBOSE(10, ("Non power of 2 case: "
                                   "Failing for getting send task.\n"));
                goto out_of_resources;
            }

            APPEND_TO_TASKLIST(mqe_ptr_to_set, send_task, last_send);
            MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, send_task);
        }
    }

    /* loop over exchange send/recv pairs */
    for (exchange = 0; exchange < my_exchange_node->n_exchanges; ++exchange) {
        for (k = 0; k < my_exchange_node->tree_order - 1; k++) {
            /* rank of exchange partner */
            pair_rank = my_exchange_node->rank_exchanges[exchange][k];
            IBOFFLOAD_VERBOSE(10,("Exchange [ %d ,%d ] send to %d", exchange, k, pair_rank));
            /* post send */
            send_fragment = mca_bcol_iboffload_get_send_frag(coll_request,
                                        pair_rank, coll_request->qp_index, 0,
                                        0, SBUF, MCA_BCOL_IBOFFLOAD_SEND_FRAG_DUMMY);

            send_task = mca_bcol_iboffload_get_send_task(iboffload, pair_rank,
                    coll_request->qp_index,
                    send_fragment, coll_fragment, INLINE);
            if (OPAL_UNLIKELY(NULL == send_task)) {
                IBOFFLOAD_VERBOSE(10, ("Exchaging: "
                            "Failing for getting send task.\n"));
                goto out_of_resources;
            }

            APPEND_TO_TASKLIST(mqe_ptr_to_set, send_task, last_send);
            MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, send_task);
        }

        for (k = 0; k < my_exchange_node->tree_order - 1; k++) {

            pair_rank = my_exchange_node->rank_exchanges[exchange][k];
            IBOFFLOAD_VERBOSE(10,("Exchange [ %d ,%d ] recv %d", exchange, k, pair_rank));
            /* post wait */
            preposted_recv_frag = mca_bcol_iboffload_get_preposted_recv_frag(
                    iboffload, pair_rank, coll_request->qp_index);
            if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
                IBOFFLOAD_VERBOSE(10, ("Exchaging: "
                            "Failing for getting prepost recv frag.\n"));
                goto out_of_resources;
            }

            wait_task = mca_bcol_iboffload_get_wait_task(iboffload, pair_rank, 1,
                    preposted_recv_frag, coll_request->qp_index, NULL);
            if (OPAL_UNLIKELY(NULL == wait_task)) {
                IBOFFLOAD_VERBOSE(10, ("Exchaging: "
                            "Failing for getting wait task.\n"));
                goto out_of_resources;
            }

            APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
            MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);
        }
    }

    /* if non power of 2, may need to send message to "extra" proc */
    if (0 < my_exchange_node->n_extra_sources)  {
        if (EXTRA_NODE == my_exchange_node->node_type) {
            /* I will not participate in the exchange -
             * wait for signal from exchange process */
            extra_rank = my_exchange_node->rank_extra_sources_array[0];
            IBOFFLOAD_VERBOSE(10,("Wait from proxy %d", extra_rank));
            /* post wait */
            preposted_recv_frag =
                mca_bcol_iboffload_get_preposted_recv_frag(iboffload, extra_rank,
                                                           coll_request->qp_index);
            if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
                IBOFFLOAD_VERBOSE(10, ("Sending to 'extra' node: "
                                   "Failing for getting prepost recv frag.\n"));
                goto out_of_resources;
            }

            wait_task = mca_bcol_iboffload_get_wait_task(iboffload, extra_rank, 1,
                                                         preposted_recv_frag,
                                                         coll_request->qp_index, NULL);
            if (OPAL_UNLIKELY(NULL == wait_task)) {
                IBOFFLOAD_VERBOSE(10, ("Sending to 'extra' node: "
                                   "Failing for getting wait task.\n"));
                goto out_of_resources;
            }

            APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
            MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

        }  else {
            /* I will participate in the exchange -
             * send signal to extra process */
            for (k = 0; k < my_exchange_node->n_extra_sources; k++) {
                extra_rank = my_exchange_node->rank_extra_sources_array[k];
                IBOFFLOAD_VERBOSE(10,("Exchange [ %d ] extra release %d", k, extra_rank));

                send_fragment = mca_bcol_iboffload_get_send_frag(coll_request,
                                        extra_rank, coll_request->qp_index, 0,
                                        0, SBUF, MCA_BCOL_IBOFFLOAD_SEND_FRAG_DUMMY);

                send_task = mca_bcol_iboffload_get_send_task(
                        iboffload, extra_rank,
                        coll_request->qp_index,
                        send_fragment, coll_fragment, INLINE);
                if (OPAL_UNLIKELY(NULL == send_task)) {
                    IBOFFLOAD_VERBOSE(10, ("Sending to 'extra' node: "
                                "Failing for getting send task.\n"));
                    goto out_of_resources;
                }

                APPEND_TO_TASKLIST(mqe_ptr_to_set, send_task, last_send);
                MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, send_task);
            }
        }
    }

    /* Fill in the the rest of the coll_fragment */
    IBOFFLOAD_VERBOSE(10, ("Fill in the the rest of the coll_fragment.\n"));
    /* end of list */
    *mqe_ptr_to_set = NULL;

    /* finish initializing full message descriptor */
    coll_request->n_fragments  = 1;
    coll_request->n_frags_sent = 1;

    coll_request->n_frag_mpi_complete = 0;
    coll_request->n_frag_net_complete = 0;

    coll_request->user_handle_freed = false;

    last_wait->flags |= MQE_WR_FLAG_SIGNAL;
    coll_fragment->signal_task_wr_id = last_wait->wr_id;
    last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;

    /* post the mwr */
    if (MCA_BCOL_IBOFFLOAD_QP_SYNC != coll_request->qp_index) {
        rc = mca_bcol_iboffload_post_mqe_tasks(iboffload, coll_fragment->to_post);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
            /* Note: need to clean up */
            return rc;
        }

        MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload->super, coll_request->order_info);
    } else {
        /* Special flow for ML service barrier , only this function supposed to
         post service requests */
        struct mqe_task *bad_mqe = NULL;
        assert (MCA_BCOL_IBOFFLOAD_QP_SYNC == coll_request->qp_index );
        /* Post to special service MQ - 1 */
        rc = mqe_post_task(iboffload->mq[1], coll_fragment->to_post, &bad_mqe);
        if (OPAL_UNLIKELY(0 != rc)) {
            IBOFFLOAD_ERROR(("ibv_post_mqe failed on device (%s), errno says: %s,"
                        " the return code is [%d]\n",
                        ibv_get_device_name(iboffload->device->dev.ib_dev),
                        strerror(errno), rc));
            return OMPI_ERROR;
        }
    }

    IBOFFLOAD_VERBOSE(10, ("Return success.\n"));
    return OMPI_SUCCESS;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Barrier, adding collfrag to collfrag_pending.\n"));
    return mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload);
}

int mca_bcol_iboffload_barrier_intra_recursive_knomial_start(
        struct mca_bcol_iboffload_module_t *iboffload,
        struct mca_bcol_iboffload_collreq_t *coll_request)
{
    int rc;

    rc = recursive_knomial_start_connections(iboffload);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    iboffload->barrier_algth =
        mca_bcol_iboffload_barrier_intra_recursive_knomial;
    return
       mca_bcol_iboffload_barrier_intra_recursive_knomial(iboffload, coll_request);
}

int mca_bcol_iboffload_rec_doubling_start_connections(mca_bcol_iboffload_module_t *iboffload)
{
    mca_common_netpatterns_pair_exchange_node_t *my_exchange_node =
                                          &iboffload->recursive_doubling_tree;

    int i, n_exchanges = my_exchange_node->n_exchanges,
        *exchanges = my_exchange_node->rank_exchanges,
        n_extra_src = my_exchange_node->n_extra_sources,
        rank_extra_src = my_exchange_node->rank_extra_source;

    mca_bcol_iboffload_endpoint_t *ep;

    IBOFFLOAD_VERBOSE(10, ("\nMy sbgp rank (index) - %d, "
                          "num of endpoints = %d, iboffload module - %p\n",
                           iboffload->ibnet->super.my_index, iboffload->num_endpoints, iboffload));
    if (0 < n_extra_src) {
        iboffload->alg_task_consump[RECURSIVE_DOUBLING_BARRIER_ALG] += 2; /* One send task one wait */
        ep = iboffload->endpoints[rank_extra_src];

        if (iboffload->ibnet->super.my_index < ep->index) {
            while(0 == (ep)->remote_zero_rdma_addr.addr) {
                opal_progress();
            }
        } else {
            IBOFFLOAD_VERBOSE(10, ("Trying to connect - %d", ep->index));
            while (OMPI_SUCCESS !=
                    check_endpoint_state(ep, NULL, NULL)) {
                opal_progress();
            }
        }
    }

    for (i = 0; i < n_exchanges; ++i) {
        iboffload->alg_task_consump[RECURSIVE_DOUBLING_BARRIER_ALG] += 2; /* One send task one wait */
        ep = iboffload->endpoints[exchanges[i]];

        if (iboffload->ibnet->super.my_index < ep->index) {
            while(0 == (ep)->remote_zero_rdma_addr.addr) {
                opal_progress();
            }
        } else {
            IBOFFLOAD_VERBOSE(10, ("Trying to connect - %d", ep->index));
            while (OMPI_SUCCESS !=
                    check_endpoint_state(ep, NULL, NULL)) {
                opal_progress();
            }
        }
    }

    return OMPI_SUCCESS;
}

static int mca_bcol_iboffload_barrier_init(
        bcol_function_args_t *input_args,
        mca_bcol_iboffload_module_t *iboffload,
        collective_message_completion_callback_function cb_fn,
        struct mca_bcol_iboffload_collreq_t **coll_request)
{
    int rc;

    ompi_free_list_item_t *item;
    mca_bcol_iboffload_collfrag_t *coll_fragment;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    IBOFFLOAD_VERBOSE(10, ("Calling for mca_bcol_iboffload_barrier_init"));

    OMPI_FREE_LIST_WAIT(&cm->collreqs_free, item, rc);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for coll request free list waiting.\n"));
        return rc;
    }

    (*coll_request) = (mca_bcol_iboffload_collreq_t *) item;
    (*coll_request)->progress_fn = iboffload->barrier_algth;

    /*
     * For usual barrier it is null. For memory
     * service barrier we need some work to do
     */
    (*coll_request)->completion_cb_fn = cb_fn;
    (*coll_request)->order_info = &input_args->order_info;

    (*coll_request)->module = iboffload;
    (*coll_request)->ml_buffer_index = input_args->buffer_index;
    (*coll_request)->buffer_info[SBUF].offset = 0;
    (*coll_request)->buffer_info[RBUF].offset = 0;
    (*coll_request)->qp_index = MCA_BCOL_IBOFFLOAD_QP_BARRIER;

    input_args->bcol_opaque_data = (void *) (*coll_request);

    /*
     * setup collective work request
     */

    /* get collective frag */
    coll_fragment = &(*coll_request)->first_collfrag;
    mca_bcol_iboffload_collfrag_init(coll_fragment);

    coll_fragment->mq_index = COLL_MQ;

    /* set pointers for (coll frag) <-> (coll full request) */
    MCA_BCOL_IBOFFLOAD_SET_COLL_REQ_LINKS(*coll_request, coll_fragment);

    return OMPI_SUCCESS;
}

/************************************************************************
 ************************ New style Barrier *****************************
 ***********************************************************************/

static int mca_bcol_iboffload_new_style_barrier_progress(
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

        IBOFFLOAD_VERBOSE(10, ("Barrier already done.\n"));
        return BCOL_FN_COMPLETE;
    }

    return BCOL_FN_STARTED;
}

static int mca_bcol_iboffload_new_style_barrier_intra(
                                bcol_function_args_t *input_args,
                                struct coll_ml_function_t *const_args)
{
    /* local variables */
    int rc;
    mca_bcol_iboffload_collreq_t *coll_request;
    mca_bcol_iboffload_module_t *iboffload =
                    (mca_bcol_iboffload_module_t *) const_args->bcol_module;

    /* check for ordering */
    MCA_BCOL_CHECK_ORDER(const_args->bcol_module, input_args);

    /*
     * recursive doubling
     */


    IBOFFLOAD_VERBOSE(10, ("Barrier starts.\n"));

    /* init barrier collective reqeust */
    rc = mca_bcol_iboffload_barrier_init(input_args, iboffload, NULL, &coll_request);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("Get error from mca_bcol_iboffload_barrier_init"));
        return rc;
    }

    /* start the barrier */
    rc = iboffload->barrier_algth(iboffload, coll_request);
    if (OPAL_UNLIKELY(OMPI_ERROR == rc)) {
        return BCOL_FN_NOT_STARTED;
    }

    /* done */
    return BCOL_FN_STARTED;
}

int mca_bcol_iboffload_barrier_register(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    IBOFFLOAD_VERBOSE(10, ("Register iboffload Barrier.\n"));

    comm_attribs.bcoll_type = BCOL_BARRIER;

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
        mca_bcol_iboffload_new_style_barrier_intra,
        mca_bcol_iboffload_new_style_barrier_progress);

    return OMPI_SUCCESS;
}

int mca_bcol_iboffload_memsync_register(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    IBOFFLOAD_VERBOSE(10, ("Register sync function\n"));

    comm_attribs.bcoll_type = BCOL_SYNC;

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
        mca_bcol_iboffload_nb_memory_service_barrier_intra,
        mca_bcol_iboffload_new_style_barrier_progress);

    return OMPI_SUCCESS;
}
