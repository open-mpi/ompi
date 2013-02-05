/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_IBOFFLOAD_BCAST_H
#define MCA_BCOL_IBOFFLOAD_BCAST_H

#include "ompi_config.h"

#include "bcol_iboffload.h"
#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_task.h"
#include "bcol_iboffload_collreq.h"
#include "bcol_iboffload_collfrag.h"
#include "bcol_iboffload_endpoint.h"

#include "opal/include/opal/types.h"

BEGIN_C_DECLS

int mca_bcol_iboffload_small_msg_bcast_progress(
                        bcol_function_args_t *input_args,
                        struct coll_ml_function_t *const_args);
int mca_bcol_iboffload_small_msg_bcast_extra_intra(bcol_function_args_t *fn_arguments,
        struct coll_ml_function_t *const_args);
int mca_bcol_iboffload_small_msg_bcast_intra(bcol_function_args_t *fn_arguments,
        struct coll_ml_function_t *const_args);
int mca_bcol_iboffload_bcast_scatter_allgather_intra(bcol_function_args_t *fn_arguments,
        struct coll_ml_function_t *const_args);
int mca_bcol_iboffload_zero_copy_progress(bcol_function_args_t *fn_arguments,
        struct coll_ml_function_t *const_args);
int mca_bcol_iboffload_bcast_scatter_allgather_extra_intra(bcol_function_args_t *fn_arguments,
        struct coll_ml_function_t *const_args);
int mca_bcol_iboffload_bcast_register(mca_bcol_base_module_t *super);

static inline __opal_attribute_always_inline__ int
mca_bcol_iboffload_recv_rtr_setup(
        struct mqe_task **last_wait,
        uint32_t dest_rank,
        mca_bcol_iboffload_module_t *iboffload,
        mca_bcol_iboffload_collfrag_t *coll_fragment)
{
    mca_bcol_iboffload_task_t *task;
    mca_bcol_iboffload_frag_t *fragment;

    /* Wait for RTR message over credit QP */
    fragment = mca_bcol_iboffload_get_preposted_recv_frag(
            iboffload, dest_rank,
            MCA_BCOL_IBOFFLOAD_QP_CREDIT);
    if (OPAL_UNLIKELY(NULL == fragment)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get recv frag.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    task = mca_bcol_iboffload_get_wait_task(
            iboffload, dest_rank, 1, fragment, MCA_BCOL_IBOFFLOAD_QP_CREDIT,
            iboffload->endpoints[dest_rank]->qps[MCA_BCOL_IBOFFLOAD_QP_LARGE_BUFF].qp->lcl_qp);
    if (OPAL_UNLIKELY(NULL == task)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get wait task.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, task);
    MCA_BCOL_IBOFFLOAD_APPEND_MQ_TASK_TO_LIST(coll_fragment->tail_next, task);

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__ int
mca_bcol_iboffload_send_small_buff_setup(
        struct mqe_task **last_send,
        size_t len, uint32_t dest_rank,
        mca_bcol_iboffload_module_t *iboffload,
        mca_bcol_iboffload_collfrag_t *coll_fragment)
{
    mca_bcol_iboffload_task_t *task;
    mca_bcol_iboffload_frag_t *fragment;

    mca_bcol_iboffload_collreq_t *coll_request =
        coll_fragment->coll_full_req;

    IBOFFLOAD_VERBOSE(10,("Get ml frag that I will send dest rank %d, len %d, lkey %d",
                            dest_rank, len, iboffload->rdma_block.ib_info.lkey));

    fragment = mca_bcol_iboffload_get_send_frag(coll_request, dest_rank,
                                 coll_request->qp_index, len, 0,
                                 SBUF, /* this could be problematic */
                                 MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML);
    if (OPAL_UNLIKELY(NULL == fragment)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get frag.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    IBOFFLOAD_VERBOSE(10,("Get an rdma task for dest %d for packet size %d",
                            dest_rank,len));
    task = mca_bcol_iboffload_get_rdma_task(
                            dest_rank, 0,
                            fragment, iboffload, coll_fragment);

    if (OPAL_UNLIKELY(NULL == task)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get send task.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    *last_send = &task->element;

    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, task);
    MCA_BCOL_IBOFFLOAD_APPEND_MQ_TASK_TO_LIST(coll_fragment->tail_next, task);

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__ int
mca_bcol_iboffload_send_large_buff_setup(
        struct mqe_task **last_send,
        int buf_index, int offset,
        size_t len, uint32_t dest_rank,
        mca_bcol_iboffload_module_t *iboffload,
        mca_bcol_iboffload_collfrag_t *coll_fragment)
{
    mca_bcol_iboffload_task_t *task;
    mca_bcol_iboffload_frag_t *fragment;

    mca_bcol_iboffload_collreq_t *coll_request =
        coll_fragment->coll_full_req;

    fragment = mca_bcol_iboffload_get_send_frag(coll_request, dest_rank,
                                 MCA_BCOL_IBOFFLOAD_QP_LARGE_BUFF,
                                 len,
                                 offset, buf_index, MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML);
    if (OPAL_UNLIKELY(NULL == fragment)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get frag.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    task = mca_bcol_iboffload_get_send_task(
                            iboffload, dest_rank,
                            MCA_BCOL_IBOFFLOAD_QP_LARGE_BUFF,
                            fragment, coll_fragment, NO_INLINE);

    if (OPAL_UNLIKELY(NULL == task)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get send task.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    *last_send = &task->element;

    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, task);
    MCA_BCOL_IBOFFLOAD_APPEND_MQ_TASK_TO_LIST(coll_fragment->tail_next, task);

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__ int
mca_bcol_iboffload_send_rtr_setup(
                            struct mqe_task **last_send,
                            uint32_t dest_rank,
                            mca_bcol_iboffload_module_t *iboffload,
                            mca_bcol_iboffload_collfrag_t *coll_fragment)
{
    mca_bcol_iboffload_task_t *task;
    mca_bcol_iboffload_frag_t *fragment;

    /* Recv is ready , Send RTR message */
    fragment = mca_bcol_iboffload_get_send_frag(coll_fragment->coll_full_req,
                                 dest_rank, MCA_BCOL_IBOFFLOAD_QP_CREDIT, 0,
                                 0, RBUF, MCA_BCOL_IBOFFLOAD_SEND_FRAG_DUMMY);
    if (OPAL_UNLIKELY(NULL == fragment)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get frag.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    task = mca_bcol_iboffload_get_send_task(iboffload, dest_rank,
            MCA_BCOL_IBOFFLOAD_QP_CREDIT,
            fragment, coll_fragment, INLINE);
    if (OPAL_UNLIKELY(NULL == task)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get send task.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    IBOFFLOAD_VERBOSE(10, ("dest_rank - %d. qp index - %d.\n",
                dest_rank, MCA_BCOL_IBOFFLOAD_QP_CREDIT));

    *last_send = &task->element;

    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, task);
    MCA_BCOL_IBOFFLOAD_APPEND_MQ_TASK_TO_LIST(coll_fragment->tail_next, task);

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__ int
mca_bcol_iboffload_recv_small_preposted_buff_setup(
                            struct mqe_task **last_wait,
                            size_t len, uint32_t dest_rank,
                            int qp_index,
                            int nwaits,
                            mca_bcol_iboffload_module_t *iboffload,
                            mca_bcol_iboffload_collfrag_t *coll_fragment)
{
    mca_bcol_iboffload_task_t *task;
    mca_bcol_iboffload_frag_t *fragment;

    IBOFFLOAD_VERBOSE(10,("Get preposted recv from rank %d", dest_rank));

    fragment = mca_bcol_iboffload_get_preposted_recv_frag(
                               iboffload, dest_rank,
                               qp_index);
    if (OPAL_UNLIKELY(NULL == fragment)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get recv frag.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    task = mca_bcol_iboffload_get_wait_task(iboffload, dest_rank, nwaits,
            fragment, qp_index, NULL);
    if (OPAL_UNLIKELY(NULL == task)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get wait task.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    *last_wait = &task->element;

    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, task);
    MCA_BCOL_IBOFFLOAD_APPEND_MQ_TASK_TO_LIST(coll_fragment->tail_next, task);

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__ int
mca_bcol_iboffload_recv_small_buff_setup(
                            struct mqe_task **last_wait,
                            size_t len, uint32_t dest_rank,
                            mca_bcol_iboffload_module_t *iboffload,
                            mca_bcol_iboffload_collfrag_t *coll_fragment)
{
    mca_bcol_iboffload_task_t *task;
    mca_bcol_iboffload_frag_t *fragment;

    mca_bcol_iboffload_collreq_t *coll_request =
        coll_fragment->coll_full_req;

    IBOFFLOAD_VERBOSE(10, ("Get preposted recv from rank %d", dest_rank));

    fragment = mca_bcol_iboffload_get_preposted_recv_frag(
                               iboffload, dest_rank,
                               coll_request->qp_index);
    if (OPAL_UNLIKELY(NULL == fragment)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get recv frag.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    task = mca_bcol_iboffload_get_wait_task(iboffload, dest_rank, 1,
            fragment, coll_request->qp_index, NULL);
    if (OPAL_UNLIKELY(NULL == task)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get wait task.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    *last_wait = &task->element;

    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, task);
    MCA_BCOL_IBOFFLOAD_APPEND_MQ_TASK_TO_LIST(coll_fragment->tail_next, task);

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__ int
mca_bcol_iboffload_recv_large_buff_setup(
                            struct mqe_task **last_wait,
                            int buf_index, int offset,
                            size_t len, uint32_t dest_rank,
                            mca_bcol_iboffload_module_t *iboffload,
                            mca_bcol_iboffload_collfrag_t *coll_fragment)
{
    int num_preposted;

    mca_bcol_iboffload_task_t *task;
    mca_bcol_iboffload_frag_t *fragment;

    mca_bcol_iboffload_collreq_t *coll_request = coll_fragment->coll_full_req;

    /* Post message to recv queue for large messages */
    fragment = mca_bcol_iboffload_get_ml_frag(
            iboffload, MCA_BCOL_IBOFFLOAD_QP_LARGE_BUFF, len,
            coll_request->buffer_info[buf_index].iboffload_reg->mr->lkey,
            (uint64_t)((unsigned char *)coll_request->buffer_info[buf_index].buf + offset));
    if (OPAL_UNLIKELY(NULL == fragment)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get recv frag.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    num_preposted = mca_bcol_iboffload_prepost_ml_recv_frag(
            MCA_BCOL_IBOFFLOAD_QP_LARGE_BUFF,
            dest_rank, fragment, iboffload);
    if (0 >= num_preposted) {
        IBOFFLOAD_ERROR(("Failed to prepost recv fragments "
                    "return code - %d; dest_rank - %d",
                    num_preposted, dest_rank));

        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    task = mca_bcol_iboffload_get_wait_task(iboffload, dest_rank, 1,
            fragment, MCA_BCOL_IBOFFLOAD_QP_LARGE_BUFF, NULL);
    if (OPAL_UNLIKELY(NULL == task)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to get wait task.\n"));
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    *last_wait = &task->element;

    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, task);
    MCA_BCOL_IBOFFLOAD_APPEND_MQ_TASK_TO_LIST(coll_fragment->tail_next, task);

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__
int bcol_iboffload_binomial_root_to_src(int group_root, int my_rank,
        int pow2_size, int group_size, int *distance)
{
    int root, relative_rank, src,
        pow2_distance = 0, i;

    if (group_root < pow2_size) {
        root = group_root;
    } else {
        /* the source of the data is extra node,
           the real root it represented by some rank from
           pow2 group */
        root = group_root - pow2_size;
        /* shortcut for the case when my rank is root for the group */
        if (my_rank == root) {
            *distance = -1;
            return group_root;
        }
    }

    relative_rank = (my_rank - root) < 0 ? my_rank - root + pow2_size :
        my_rank - root;

    for (i = 1; i < pow2_size; i<<=1, pow2_distance++) {
        if (relative_rank & i) {
            src = my_rank ^ i;
            if (src >= pow2_size)
                src -= pow2_size;

            *distance = pow2_distance;
            IBOFFLOAD_VERBOSE(10, ("AAAAA d %d rel %d it %d root %d my %d", *distance, relative_rank, i, root, my_rank));
            return src;
        }
    }

    /* error case */
    *distance = -1;
    return -1;
}

static inline void bcol_iboffload_setup_binomial_connection(mca_bcol_iboffload_module_t *iboffload)
{
    netpatterns_pair_exchange_node_t *my_exchange_node =
                                          &iboffload->recursive_doubling_tree;

    int i, n_exchanges = my_exchange_node->n_exchanges,
        *exchanges = my_exchange_node->rank_exchanges,
        n_extra_src = my_exchange_node->n_extra_sources,
        my_rank = iboffload->ibnet->super.my_index,
        rank_extra_src = my_exchange_node->rank_extra_source;

    mca_bcol_iboffload_endpoint_t *ep;

    IBOFFLOAD_VERBOSE(10, ("Open connections.\n"));

    if (0 < n_extra_src) {
        ep = iboffload->endpoints[rank_extra_src];
        while (OMPI_SUCCESS !=
                check_endpoint_state(ep, NULL, NULL)) {
            opal_progress();
        }

#if OPAL_ENABLE_DEBUG
        {
            int qp_index, num_qps = mca_bcol_iboffload_component.num_qps;
            for (qp_index = 0; qp_index < num_qps; ++qp_index) {
                assert(NULL != ep->qps[qp_index].qp->lcl_qp);
                IBOFFLOAD_VERBOSE(10, ("Endpoint - %p, QP index - %d: qp num - %x.",
                                       ep, qp_index, ep->qps[qp_index].qp->lcl_qp->qp_num));
            }
        }
#endif

        /* Connect to all extra nodes */
        if (EXTRA_NODE == my_exchange_node->node_type) {
            for (i = iboffload->power_of_2_ranks;
                    i < iboffload->num_endpoints; ++i) {
                if (i != my_rank) {
                    ep = iboffload->endpoints[i];

                    IBOFFLOAD_VERBOSE(10, ("subgroup rank %d: Connect to rank %d.\n", my_rank, i));

                    while (OMPI_SUCCESS !=
                            check_endpoint_state(ep, NULL, NULL)) {
                        opal_progress();
                    }

#if OPAL_ENABLE_DEBUG
        {
            int qp_index, num_qps = mca_bcol_iboffload_component.num_qps;
            for (qp_index = 0; qp_index < num_qps; ++qp_index) {
                assert(NULL != ep->qps[qp_index].qp->lcl_qp);
                IBOFFLOAD_VERBOSE(10, ("Endpoint - %p, QP index - %d: qp num - %x.",
                                       ep, qp_index, ep->qps[qp_index].qp->lcl_qp->qp_num));
            }
        }
#endif
                }
            }
        }
    }

    for (i = 0; i < n_exchanges; ++i) {
        ep = iboffload->endpoints[exchanges[i]];

        while (OMPI_SUCCESS !=
                check_endpoint_state(ep, NULL, NULL)) {
            opal_progress();
        }

#if OPAL_ENABLE_DEBUG
        {
            int qp_index, num_qps = mca_bcol_iboffload_component.num_qps;
            for (qp_index = 0; qp_index < num_qps; ++qp_index) {
                assert(NULL != ep->qps[qp_index].qp->lcl_qp);
                IBOFFLOAD_VERBOSE(10, ("Endpoint - %p, QP index - %d: qp num - %x.",
                                       ep, qp_index, ep->qps[qp_index].qp->lcl_qp->qp_num));
            }
        }
#endif
    }
    /* set the connection status to connected */
    iboffload->connection_status[RECURSIVE_DOUBLING_TREE_BCAST] = true;
}

static inline __opal_attribute_always_inline__
int bcol_iboffload_bcast_binomial_gather(mca_bcol_iboffload_module_t *iboffload_module,
        struct mqe_task **last_send, struct mqe_task **last_wait,
        mca_bcol_iboffload_collfrag_t *coll_fragment,
        int count, int base_block_size, int radix_mask_pow)
{
    int rc;
    int i;
    int my_group_index = iboffload_module->ibnet->super.my_index;
    int delta, rdelta;

    IBOFFLOAD_VERBOSE(10, ("bcol_iboffload_bcast_binomial_gather %d %d",
                radix_mask_pow, my_group_index));

    /* we assume the iteration #iteration already was completed with probe */
    for (i = 0; i < iboffload_module->power_of_2; i++) {
        int pow2 = 1 << i;
        int peer_index = my_group_index ^ pow2;
        int slen, rlen,
            send_offset,
            recv_offset;

        if (i > radix_mask_pow) {
            slen = rlen = pow2 * base_block_size;
            send_offset = base_block_size * ((my_group_index) & ((~(int)0) << i));
            recv_offset = base_block_size * ((peer_index)     & ((~(int)0) << i));

            rdelta = count - recv_offset;
            if (rdelta > 0) {
                IBOFFLOAD_VERBOSE(10, ("Recv1 [ pow2 %d, radix %d ] offset %d , len %d , dest %d",
                            pow2, 1 << iboffload_module->power_of_2,
                            recv_offset, rlen, peer_index));

                rc = mca_bcol_iboffload_send_rtr_setup(last_send,
                        peer_index, iboffload_module,
                        coll_fragment);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                    IBOFFLOAD_VERBOSE(10, ("Failed to setup send rtr"));
                    return OMPI_ERROR;
                }
            }

            delta = count - send_offset;
            if (delta > 0) {
                if (delta < slen) {
                    /* recv the tail */
                    slen = delta;
                }

                IBOFFLOAD_VERBOSE(10, ("Send1 [ pow2 %d, radix %d ] offset %d , len %d , dest %d",
                            pow2, 1 << iboffload_module->power_of_2,
                            send_offset, slen, peer_index));
                rc = mca_bcol_iboffload_recv_rtr_setup(last_wait, peer_index, iboffload_module, coll_fragment);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                    IBOFFLOAD_VERBOSE(10, ("Failed to isend data"));
                    return OMPI_ERROR;
                }

                rc = mca_bcol_iboffload_send_large_buff_setup(last_send, SBUF, send_offset, slen, peer_index,
                        iboffload_module, coll_fragment);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                    IBOFFLOAD_VERBOSE(10, ("Failed to isend data"));
                    return OMPI_ERROR;
                }
            }

            if (rdelta > 0) {
                if (rdelta < rlen) {
                    /* recv the tail */
                    rlen = rdelta;
                }

                rc = mca_bcol_iboffload_recv_large_buff_setup(last_wait,
                        SBUF, recv_offset, rlen, peer_index,
                        iboffload_module, coll_fragment);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                    IBOFFLOAD_VERBOSE(10, ("Failed to setup data receive"));
                    return OMPI_ERROR;
                }
            }

        } else if (i == radix_mask_pow) {
            /* only receive data */
            rlen = pow2 * base_block_size;
            recv_offset = base_block_size * ((peer_index) & ((~(int)0) << i));
            delta = count - recv_offset;
            if (0 >= delta) {
                /* we have nothing to send, skip the iteration */
                continue;
            }
            if (delta < rlen) {
                /* recv the tail */
                rlen = delta;
            }
            /* receive data from the peer */
            IBOFFLOAD_VERBOSE(10, ("Recv2 [ pow2 %d, radix %d ] offset %d , len %d , dest %d",
                        pow2,
                        1 << iboffload_module->power_of_2,
                        recv_offset,
                        rlen, peer_index));
            rc = mca_bcol_iboffload_send_rtr_setup(last_send,
                    peer_index, iboffload_module,
                    coll_fragment);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to setup send rtr"));
                return OMPI_ERROR;
            }

            rc = mca_bcol_iboffload_recv_large_buff_setup(last_wait,
                    SBUF, recv_offset, rlen, peer_index,
                    iboffload_module, coll_fragment);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to setup data receive"));
                return OMPI_ERROR;
            }
        } else if (i < radix_mask_pow) {
            /* Only send data */
            slen = pow2 * base_block_size;
            send_offset = base_block_size * ((my_group_index) & ((~(int)0) << i));
            delta = count - send_offset;
            if (0 >= delta) {
                /* we have nothing to send, skip the iteration */
                continue;
            }

            if (delta < slen) {
                slen = delta;
            }

            IBOFFLOAD_VERBOSE(10, ("Send2 [ pow2 %d, radix %d ] offset %d , len %d , dest %d",
                        pow2,
                        1 << iboffload_module->power_of_2,
                        send_offset,
                        slen,
                        peer_index));

            rc = mca_bcol_iboffload_recv_rtr_setup(last_wait, peer_index, iboffload_module, coll_fragment);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to isend data"));
                return OMPI_ERROR;
            }

            rc = mca_bcol_iboffload_send_large_buff_setup(last_send, SBUF, send_offset, slen, peer_index,
                    iboffload_module, coll_fragment);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to isend data"));
                return OMPI_ERROR;
            }
        }
    }

    return OMPI_SUCCESS;
}

END_C_DECLS

#endif
