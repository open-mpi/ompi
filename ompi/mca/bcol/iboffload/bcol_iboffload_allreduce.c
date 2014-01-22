/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file */

#include "ompi_config.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <inttypes.h>

#include "bcol_iboffload.h"
#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_task.h"
#include "bcol_iboffload_collfrag.h"
#include "bcol_iboffload_endpoint.h"

#include "opal/include/opal/types.h"

static int mca_bcol_iboffload_calc_res_to_user(void *callback_data)
{
    int rc;
    uint64_t result = 0;

    uint64_t l_operand = 0;
    uint64_t r_operand = 0;

    mca_bcol_iboffload_collfrag_t *coll_frag =
                        (mca_bcol_iboffload_collfrag_t *) callback_data;

    mca_bcol_iboffload_collreq_t *coll_request = coll_frag->coll_full_req;

    ompi_op_t *op = coll_request->op;
    ompi_datatype_t *dtype = coll_request->dtype;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    struct ibv_context *ib_dev_context = coll_request->module->device->dev.ib_dev_context;

    IBOFFLOAD_VERBOSE(10, ("Start calculating.\n"));

    rc = unpack_data_from_calc(ib_dev_context,
                               cm->map_ompi_to_ib_calcs[op->op_type],
                               cm->map_ompi_to_ib_dt[dtype->id], false,
                               (void *) (uintptr_t) coll_request->l_operand,
                               NULL, (void *) &l_operand);
    if (0 != rc) {
        IBOFFLOAD_VERBOSE(10, ("unpack_data_from_calc for l_operand failed: op %s, type %s\n",
                                op->o_name, dtype->name));
        return OMPI_ERROR;
    }

    rc = unpack_data_from_calc(ib_dev_context,
                               cm->map_ompi_to_ib_calcs[op->op_type],
                               cm->map_ompi_to_ib_dt[dtype->id], false,
                               (void *) (uintptr_t) coll_request->r_operand,
                               NULL, (void *) &r_operand);
    if (0 != rc) {
        IBOFFLOAD_VERBOSE(10, ("unpack_data_from_calc for r_operand failed: op %s, type %s\n",
                                op->o_name, dtype->name));
        return OMPI_ERROR;
    }

    switch (op->op_type) {
        case OMPI_OP_PROD:
                break; /* ronni todo - ????? */
        case OMPI_OP_LAND:
            result = l_operand && r_operand;
            break;
        case OMPI_OP_BAND:
            result = l_operand & r_operand;
            break;
        case OMPI_OP_LOR:
            result = l_operand || r_operand;
            break;
        case OMPI_OP_BOR:
            result = l_operand | r_operand;
            break;
        case OMPI_OP_LXOR:
            result = ((l_operand && !r_operand) || (!l_operand && r_operand));
            break;
        case OMPI_OP_BXOR:
            result = l_operand ^ r_operand;
            break;
        case OMPI_OP_MAXLOC:
        case OMPI_OP_MINLOC:
            break;
        case OMPI_OP_MAX:
        case OMPI_OP_MIN:
        case OMPI_OP_SUM:
            switch (cm->map_ompi_to_ib_dt[dtype->id]) {
                case IBV_M_DATA_TYPE_INT8:
                    MCA_BCOL_IBOFFLOAD_ALLREDUCE_DO_CALC(coll_request->op->op_type, char, l_operand, r_operand, result);
                    break;
                case IBV_M_DATA_TYPE_INT16:
                    MCA_BCOL_IBOFFLOAD_ALLREDUCE_DO_CALC(coll_request->op->op_type, int16_t, l_operand, r_operand, result);
                    break;
                case IBV_M_DATA_TYPE_INT32:
                    MCA_BCOL_IBOFFLOAD_ALLREDUCE_DO_CALC(coll_request->op->op_type, int32_t, l_operand, r_operand, result);
                    break;
                case IBV_M_DATA_TYPE_INT64:
                    MCA_BCOL_IBOFFLOAD_ALLREDUCE_DO_CALC(coll_request->op->op_type, int64_t, l_operand, r_operand, result);
                    break;
                case IBV_M_DATA_TYPE_FLOAT32:
                    MCA_BCOL_IBOFFLOAD_ALLREDUCE_DO_CALC(coll_request->op->op_type, float, l_operand, r_operand, result);
                    break;
                case IBV_M_DATA_TYPE_FLOAT64:
                    MCA_BCOL_IBOFFLOAD_ALLREDUCE_DO_CALC(coll_request->op->op_type, double, l_operand, r_operand, result);
                    break;
                default:
                    IBOFFLOAD_VERBOSE(10, ("Unsupported data type: %s.\n", dtype->name));
                    return OMPI_ERROR;
            }

            break;

        default:
            IBOFFLOAD_VERBOSE(10, ("Unsupported op: %s.\n", coll_request->op->o_name));
            return OMPI_ERROR;
    }

    memcpy(coll_request->buffer_info[RBUF].buf, &result, coll_frag->unpack_size);
    IBOFFLOAD_VERBOSE(10, ("The output data after calc is %lf, result %lf, l_operand %lf, r_operand %lf: "
                           "sbuf addr %p, rbuf addr %p.\n",
                           *(double *) coll_request->buffer_info[RBUF].buf, *(double *) &result,
                           *(double *) &l_operand, *(double *) &r_operand,
                           coll_request->buffer_info[SBUF].buf,
                           coll_request->buffer_info[RBUF].buf));

    return OMPI_SUCCESS;
}

static int mca_bcol_iboffload_unpack_res_to_user(void *callback_data)
{
    int rc;

    mca_bcol_iboffload_collfrag_t *coll_frag =
                        (mca_bcol_iboffload_collfrag_t *) callback_data;

    mca_bcol_iboffload_collreq_t *coll_request = coll_frag->coll_full_req;
    mca_bcol_iboffload_task_t *task = (mca_bcol_iboffload_task_t *) coll_frag->signal_task_wr_id;

    mca_bcol_iboffload_frag_t *recv_frag = task->frag;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    struct ibv_context *ib_dev_context = coll_request->module->device->dev.ib_dev_context;

    rc = unpack_data_from_calc(ib_dev_context,
                               cm->map_ompi_to_ib_calcs[coll_request->op->op_type],
                               cm->map_ompi_to_ib_dt[coll_request->dtype->id],
                               false, (void*) (uintptr_t) recv_frag->sg_entry.addr,
                               NULL, coll_request->buffer_info[RBUF].buf);
    if (0 != rc) {
        IBOFFLOAD_VERBOSE(10, ("unpack_data_from_calc is failed: op %s, type %s\n",
                                coll_request->op->o_name, coll_request->dtype->name));
        return OMPI_ERROR;
    }

    IBOFFLOAD_VERBOSE(10, ("The naitive output data is %" PRId64 ".\n"
                           "The output data is %" PRId64 ".\n",
                            *(uint64_t *) recv_frag->sg_entry.addr,
                            *(uint64_t *) coll_request->buffer_info[RBUF].buf));

    return OMPI_SUCCESS;
}

static int
allreduce_extra_node(mca_bcol_iboffload_module_t *iboffload,
                     mca_bcol_iboffload_collreq_t *coll_request)
/* (EXTRA_NODE == my_exchange_node->node_type) */
{
    /* local variables */
    int rc, extra_rank;

    mca_bcol_iboffload_frag_t *send_fragment,
                              *preposted_recv_frag;

    mca_bcol_iboffload_task_t *send_task,
                              *wait_task;

    struct mqe_task *last_wait, /* we need ask from completion on last wait */
                    *last_send;

    netpatterns_pair_exchange_node_t *my_exchange_node =
                                          &iboffload->recursive_doubling_tree;

    struct mqe_task **mqe_ptr_to_set;
    mca_bcol_iboffload_collfrag_t *coll_fragment = (mca_bcol_iboffload_collfrag_t *)
                                                   opal_list_get_last(&coll_request->work_requests);

    mqe_ptr_to_set = &coll_fragment->to_post;

    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                 iboffload, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));

        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    /* I will NOT participate in the exchange - so just "register" as here */
    extra_rank = my_exchange_node->rank_extra_source;

    send_fragment = mca_bcol_iboffload_get_send_frag(coll_request,
                            extra_rank, coll_request->qp_index,
                            MCA_IBOFFLOAD_IB_DRIVER_OPERAND_SIZE, 0,
                            SBUF,
                            MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML_CALC);

    if (OPAL_UNLIKELY(NULL == send_fragment)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting and packing send frag.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    /* send my operand to EXCHANGE NODE */
    send_task = mca_bcol_iboffload_get_send_task(iboffload, extra_rank,
            coll_request->qp_index, send_fragment, coll_fragment, INLINE);
    if (OPAL_UNLIKELY(NULL == send_task)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting send task.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    APPEND_TO_TASKLIST(mqe_ptr_to_set, send_task, last_send);
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, send_task);

    preposted_recv_frag =
        mca_bcol_iboffload_get_preposted_recv_frag(
                iboffload, extra_rank, coll_request->qp_index);
    if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
        /* RLG need cleanup */
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    /* Wait for final result from EXCHANGE NODE */
    wait_task = mca_bcol_iboffload_get_wait_task(iboffload, extra_rank, 1,
                            preposted_recv_frag, coll_request->qp_index, NULL);
    if (OPAL_UNLIKELY(NULL == wait_task)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

    *mqe_ptr_to_set = NULL;

    /* finish initializing full message descriptor */
    coll_request->n_fragments  = 1;
    coll_request->n_frags_sent = 1;

    /* Pasha: need to set to true in upper layer */
    coll_request->user_handle_freed = false;

    last_wait->flags |= MQE_WR_FLAG_SIGNAL;

    coll_fragment->signal_task_wr_id = last_wait->wr_id;
    last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;

    /* post the mwr */
    IBOFFLOAD_VERBOSE(10, ("Post tasks.\n"));
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload, coll_fragment->to_post);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload->super, coll_request->order_info);

    return OMPI_SUCCESS;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Allreduce: adding collfrag to collfrag_pending.\n"));
    return mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload);
}

/**
 * Start allreduce
 */
static int do_exchange(mca_bcol_iboffload_module_t *iboffload,
                    mca_bcol_iboffload_collreq_t *coll_request,
                    struct mqe_task ***mqe_ptr_to_set,
                    struct mqe_task **last_wait,
                    struct ibv_sge **l_operand,
                    struct ibv_sge **r_operand)
{
    int rc = OMPI_SUCCESS, exchange, pair_rank,
        my_rank = ((mca_sbgp_base_module_t *) iboffload->ibnet)->my_index;

    mca_bcol_iboffload_frag_t *preposted_recv_frag;

    mca_bcol_iboffload_task_t *wait_task,
                              *calc_task;

    struct mqe_task *last_send;
    netpatterns_pair_exchange_node_t *my_exchange_node =
                                          &iboffload->recursive_doubling_tree;

    mca_bcol_iboffload_collfrag_t *coll_fragment = (mca_bcol_iboffload_collfrag_t *)
                                                   opal_list_get_last(&coll_request->work_requests);

    size_t calc_size = MCA_IBOFFLOAD_IB_DRIVER_OPERAND_SIZE + MCA_IBOFFLOAD_CALC_SIZE_EXT;

    pair_rank = my_exchange_node->rank_exchanges[0];
    preposted_recv_frag =
        mca_bcol_iboffload_get_preposted_recv_frag(
                iboffload, pair_rank, coll_request->qp_index);
    if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
        /* RLG need cleanup */
        IBOFFLOAD_VERBOSE(10, ("Get prepost recv fag fail.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    /* Wait for send from first algorithm partner */
    wait_task = mca_bcol_iboffload_get_wait_task(iboffload, pair_rank, 1,
                            preposted_recv_frag, coll_request->qp_index, NULL);
    if (OPAL_UNLIKELY(NULL == wait_task)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    APPEND_TO_TASKLIST((*mqe_ptr_to_set), wait_task, (*last_wait));
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

    (*l_operand)->length = calc_size;
    for (exchange = 1; exchange < my_exchange_node->n_exchanges; ++exchange) {
        pair_rank = my_exchange_node->rank_exchanges[exchange];

        (*r_operand) = &preposted_recv_frag->sg_entry;
        (*r_operand)->length = calc_size;

        /* Calc and send the result to the partner */
        calc_task = mca_bcol_iboffload_get_calc_task(iboffload,
                        pair_rank, coll_request->qp_index, NULL,
                        *l_operand, *r_operand,
                        coll_request, NO_INLINE);
        if (OPAL_UNLIKELY(NULL == calc_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting calc task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST((*mqe_ptr_to_set), calc_task, last_send);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, calc_task);

        /* Calc and send the result to myself */
        calc_task = mca_bcol_iboffload_get_calc_task(iboffload,
                        my_rank, coll_request->qp_index, NULL,
                        *l_operand, *r_operand, coll_request, NO_INLINE);
        if (OPAL_UNLIKELY(NULL == calc_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting calc task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST((*mqe_ptr_to_set), calc_task, last_send);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, calc_task);

        preposted_recv_frag =
            mca_bcol_iboffload_get_preposted_recv_frag(
                    iboffload, my_rank, coll_request->qp_index);
        if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
            /* RLG need cleanup */
            IBOFFLOAD_VERBOSE(10, ("Get prepost recv fag fail.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        /* Wait for calc from myself */
        wait_task = mca_bcol_iboffload_get_wait_task(iboffload, my_rank, 1,
                                preposted_recv_frag, coll_request->qp_index, NULL);
        if (NULL == wait_task) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST((*mqe_ptr_to_set), wait_task, (*last_wait));
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

        (*l_operand) = &preposted_recv_frag->sg_entry;
        (*l_operand)->length = calc_size;

        preposted_recv_frag =
            mca_bcol_iboffload_get_preposted_recv_frag(
                    iboffload, pair_rank, coll_request->qp_index);
        if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
            /* RLG need cleanup */
            IBOFFLOAD_VERBOSE(10, ("Get prepost recv fag fail.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        /* Wait for calc from the current algorithm partner */
        wait_task = mca_bcol_iboffload_get_wait_task(iboffload, pair_rank, 1,
                                preposted_recv_frag, coll_request->qp_index, NULL);
        if (OPAL_UNLIKELY(NULL == wait_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST((*mqe_ptr_to_set), wait_task, (*last_wait));
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

    }

    (*r_operand) = &preposted_recv_frag->sg_entry;
    (*r_operand)->length = calc_size;

    return OMPI_SUCCESS;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Adding collfrag to collfrag_pending"));
    return mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload);
}

/* Power of 2 case */
static int
pure_recursive_doubling(mca_bcol_iboffload_module_t *iboffload,
                        mca_bcol_iboffload_collreq_t *coll_request)
{
    /* local variables */
    int rc = OMPI_SUCCESS, pair_rank,
        my_rank = ((mca_sbgp_base_module_t *) iboffload->ibnet)->my_index;

    struct mqe_task *last_send,
                    *last_wait;

    mca_bcol_iboffload_task_t *send_task,
                              *wait_task,
                              *calc_task;

    mca_bcol_iboffload_frag_t *send_fragment,
                              *preposted_recv_frag;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    netpatterns_pair_exchange_node_t *my_exchange_node =
                                          &iboffload->recursive_doubling_tree;

    struct ibv_sge *r_operand = NULL,
                   *l_operand = NULL;

    struct mqe_task **mqe_ptr_to_set;
    mca_bcol_iboffload_collfrag_t *coll_fragment = (mca_bcol_iboffload_collfrag_t *)
                                                   opal_list_get_last(&coll_request->work_requests);

    mqe_ptr_to_set = &coll_fragment->to_post;

    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                 iboffload, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));

        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    IBOFFLOAD_VERBOSE(10, ("Allreduce starting: type %d op %d, "
                       "n_extra_sources - %d.\n", cm->map_ompi_to_ib_dt[coll_request->dtype->id],
                        cm->map_ompi_to_ib_calcs[coll_request->op->op_type],
                        my_exchange_node->n_extra_sources));

    pair_rank = my_exchange_node->rank_exchanges[0];

    send_fragment = mca_bcol_iboffload_get_send_frag(coll_request,
                            pair_rank, coll_request->qp_index,
                            (MCA_IBOFFLOAD_IB_DRIVER_OPERAND_SIZE + MCA_IBOFFLOAD_CALC_SIZE_EXT), 0,
                            SBUF,
                            MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML_CALC);
    if (OPAL_UNLIKELY(NULL == send_fragment)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting and packing send frag.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }
/* Vasily: NO_INLINE ????? */
    /* send my operand to the first algorithm partner */
    send_task = mca_bcol_iboffload_get_send_task(iboffload, pair_rank,
            coll_request->qp_index, send_fragment, coll_fragment, NO_INLINE);
    if (OPAL_UNLIKELY(NULL == send_task)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting send task.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    APPEND_TO_TASKLIST(mqe_ptr_to_set, send_task, last_send);
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, send_task);

    l_operand = &send_fragment->sg_entry;
    /* Recursive-doubling exchange */
    rc = do_exchange(iboffload, coll_request, &mqe_ptr_to_set,
                          &last_wait, &l_operand, &r_operand);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }
    if (false == coll_request->do_calc_in_cpu) {
        /* Calc and send the result to myself */
        calc_task = mca_bcol_iboffload_get_calc_task(iboffload,
                        my_rank, coll_request->qp_index, NULL,
                        l_operand,
                        r_operand, coll_request, NO_INLINE);
        if (OPAL_UNLIKELY(NULL == calc_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting calc task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST(mqe_ptr_to_set, calc_task, last_send);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, calc_task);

        preposted_recv_frag =
            mca_bcol_iboffload_get_preposted_recv_frag(
                    iboffload, my_rank, coll_request->qp_index);
        if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
            /* RLG need cleanup */
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        /* Wait for calc from myself */
        wait_task = mca_bcol_iboffload_get_wait_task(iboffload, my_rank, 1,
                        preposted_recv_frag, coll_request->qp_index, NULL);
        if (OPAL_UNLIKELY(NULL == wait_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);
    } else {
        coll_request->l_operand = l_operand->addr;
        coll_request->r_operand = r_operand->addr;
    }

    *mqe_ptr_to_set = NULL;
/* Vasily: TODO with MACRO */
    /* finish initializing full message descriptor */
    coll_request->n_fragments  = 1;
    coll_request->n_frags_sent = 1;

    /* Pasha: need to set to true in upper layer */
    coll_request->user_handle_freed = false;

    last_wait->flags |= MQE_WR_FLAG_SIGNAL;

    coll_fragment->signal_task_wr_id = last_wait->wr_id;
    last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;

    /* post the mwr */
    IBOFFLOAD_VERBOSE(10, ("Post tasks.\n"));
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload, coll_fragment->to_post);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload->super, coll_request->order_info);

    return OMPI_SUCCESS;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Adding collfrag to collfrag_pending"));
    return mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload);
}

static int rdma_do_exchange(mca_bcol_iboffload_module_t *iboffload,
                    mca_bcol_iboffload_collreq_t *coll_request,
                    struct mqe_task ***mqe_ptr_to_set,
                    struct mqe_task **last_wait,
                    struct ibv_sge **l_operand,
                    struct ibv_sge **r_operand)
{
    int rc = OMPI_SUCCESS, exchange, pair_rank,
        my_rank = ((mca_sbgp_base_module_t *) iboffload->ibnet)->my_index;

    mca_bcol_iboffload_frag_t *preposted_recv_frag;

    mca_bcol_iboffload_task_t *wait_task,
                              *calc_task;

    struct mqe_task *last_send;
    netpatterns_pair_exchange_node_t *my_exchange_node =
                                          &iboffload->recursive_doubling_tree;

    mca_bcol_iboffload_collfrag_t *coll_fragment = (mca_bcol_iboffload_collfrag_t *)
                                                   opal_list_get_last(&coll_request->work_requests);

    const size_t calc_size = MCA_IBOFFLOAD_IB_DRIVER_OPERAND_SIZE + MCA_IBOFFLOAD_CALC_SIZE_EXT;
    size_t remote_offset = calc_size;
    size_t self_offset = 0;

    pair_rank = my_exchange_node->rank_exchanges[0];
    preposted_recv_frag =
        mca_bcol_iboffload_get_preposted_recv_frag(
                iboffload, pair_rank, coll_request->qp_index);
    if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
        /* RLG need cleanup */
        IBOFFLOAD_VERBOSE(10, ("Get prepost recv fag fail.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    /* Wait for send from first algorithm partner */
    wait_task = mca_bcol_iboffload_get_wait_task(iboffload, pair_rank, 1,
                            preposted_recv_frag, coll_request->qp_index, NULL);
    if (OPAL_UNLIKELY(NULL == wait_task)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    APPEND_TO_TASKLIST((*mqe_ptr_to_set), wait_task, (*last_wait));
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

    (*l_operand)->length = 2 * calc_size ;
    for (exchange = 1; exchange < my_exchange_node->n_exchanges; ++exchange) {
        pair_rank = my_exchange_node->rank_exchanges[exchange];
        /* Pasha: Not used
        (*r_operand) = &preposted_recv_frag->sg_entry;
        (*r_operand)->length = calc_size;
        */

        remote_offset +=  2 * calc_size;
        self_offset +=  2 * calc_size;

        /* Calc and send the result to the partner */
        /*
        calc_task = mca_bcol_iboffload_get_calc_task(iboffload,
                        pair_rank, coll_request->qp_index, NULL,
                        *l_operand, *r_operand,
                        coll_request, NO_INLINE);
                        */
        calc_task = mca_bcol_iboffload_get_rdma_calc_task(iboffload,
                        pair_rank, coll_request->qp_index, NULL,
                        *l_operand, NULL,
                        coll_request, remote_offset);
        if (OPAL_UNLIKELY(NULL == calc_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting calc task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST((*mqe_ptr_to_set), calc_task, last_send);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, calc_task);

        /* Calc and send the result to myself */
        /*
        calc_task = mca_bcol_iboffload_get_calc_task(iboffload,
                        my_rank, coll_request->qp_index, NULL,
                        *l_operand, NULL,
                        coll_request, NO_INLINE);
                        */
        calc_task = mca_bcol_iboffload_get_rdma_calc_task(iboffload,
                        my_rank, coll_request->qp_index, NULL,
                        *l_operand, NULL,
                        coll_request, self_offset);
        if (OPAL_UNLIKELY(NULL == calc_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting calc task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST((*mqe_ptr_to_set), calc_task, last_send);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, calc_task);

        preposted_recv_frag =
            mca_bcol_iboffload_get_preposted_recv_frag(
                    iboffload, my_rank, coll_request->qp_index);
        if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
            /* RLG need cleanup */
            IBOFFLOAD_VERBOSE(10, ("Get prepost recv fag fail.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        /* Wait for calc from myself */
        wait_task = mca_bcol_iboffload_get_wait_task(iboffload, my_rank, 1,
                                preposted_recv_frag, coll_request->qp_index, NULL);
        if (NULL == wait_task) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST((*mqe_ptr_to_set), wait_task, (*last_wait));
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

        /*
        (*l_operand) = &preposted_recv_frag->sg_entry;
        */

        /* (*l_operand)->length = 2 * calc_size; */
        (*l_operand)->addr = (uint64_t) (uintptr_t) ((unsigned char *) (*l_operand)->addr + 2 * calc_size);

        preposted_recv_frag =
            mca_bcol_iboffload_get_preposted_recv_frag(
                    iboffload, pair_rank, coll_request->qp_index);
        if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
            /* RLG need cleanup */
            IBOFFLOAD_VERBOSE(10, ("Get prepost recv fag fail.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        /* Wait for calc from the current algorithm partner */
        wait_task = mca_bcol_iboffload_get_wait_task(iboffload, pair_rank, 1,
                                preposted_recv_frag, coll_request->qp_index, NULL);
        if (OPAL_UNLIKELY(NULL == wait_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST((*mqe_ptr_to_set), wait_task, (*last_wait));
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

    }
    /* Pasha: not used
    (*r_operand) = &preposted_recv_frag->sg_entry;
    (*r_operand)->length = calc_size;
    */

    return OMPI_SUCCESS;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Adding collfrag to collfrag_pending"));
    return mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload);
}

#define ALLREDUCE_BASE_OFFSET (MCA_IBOFFLOAD_IB_DRIVER_OPERAND_SIZE + MCA_IBOFFLOAD_CALC_SIZE_EXT)

/* RDMA Recursive doubling + cache friendly version */
static int
rdma_pure_recursive_doubling(mca_bcol_iboffload_module_t *iboffload,
                        mca_bcol_iboffload_collreq_t *coll_request)
{
    /* local variables */
    int rc = OMPI_SUCCESS, pair_rank,
        my_rank = ((mca_sbgp_base_module_t *) iboffload->ibnet)->my_index;

    struct mqe_task *last_send,
                    *last_wait;

    mca_bcol_iboffload_task_t *send_task,
                              *wait_task,
                              *calc_task;

    mca_bcol_iboffload_frag_t *send_fragment,
                              *preposted_recv_frag;
    struct ibv_sge operand;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    netpatterns_pair_exchange_node_t *my_exchange_node =
                                          &iboffload->recursive_doubling_tree;

    struct ibv_sge *r_operand = NULL,
                   *l_operand = NULL;

    struct mqe_task **mqe_ptr_to_set;
    mca_bcol_iboffload_collfrag_t *coll_fragment = (mca_bcol_iboffload_collfrag_t *)
                                                   opal_list_get_last(&coll_request->work_requests);

    mqe_ptr_to_set = &coll_fragment->to_post;

    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                 iboffload, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));

        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    IBOFFLOAD_VERBOSE(10, ("Allreduce starting: type %d op %d, "
                       "n_extra_sources - %d.\n", cm->map_ompi_to_ib_dt[coll_request->dtype->id],
                        cm->map_ompi_to_ib_calcs[coll_request->op->op_type],
                        my_exchange_node->n_extra_sources));

    pair_rank = my_exchange_node->rank_exchanges[0];

    send_fragment = mca_bcol_iboffload_get_send_frag(coll_request,
                            pair_rank, coll_request->qp_index,
                            (MCA_IBOFFLOAD_IB_DRIVER_OPERAND_SIZE + MCA_IBOFFLOAD_CALC_SIZE_EXT),
                            0,
                            SBUF,
                            MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML_CALC);
    if (OPAL_UNLIKELY(NULL == send_fragment)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting and packing send frag.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }
    /* Vasily: NO_INLINE ????? */
    /* send my operand to the first algorithm partner */
    /* send_task = mca_bcol_iboffload_get_send_task(iboffload, pair_rank,
            coll_request->qp_index, send_fragment, coll_fragment, NO_INLINE); */

    send_task = mca_bcol_iboffload_get_rdma_task(
            pair_rank, ALLREDUCE_BASE_OFFSET,
            send_fragment, iboffload, coll_fragment);
    if (OPAL_UNLIKELY(NULL == send_task)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting send task.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    /* Pasha: ugly but faster, set inline on first send */
    SENDWR(send_task)->send_flags |= IBV_SEND_INLINE;


    APPEND_TO_TASKLIST(mqe_ptr_to_set, send_task, last_send);
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, send_task);

    /* l_operand = &send_fragment->sg_entry; */
    operand = send_fragment->sg_entry;
    l_operand = &operand;

    /* Recursive-doubling exchange */
    rc = rdma_do_exchange(iboffload, coll_request, &mqe_ptr_to_set,
                          &last_wait, &l_operand, &r_operand);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    /* Pasha: This flow is broken, print error */
    if (false == coll_request->do_calc_in_cpu) {
        ML_ERROR(("Calc in CPU must be enabled !!!"));
        /* Calc and send the result to myself */
        calc_task = mca_bcol_iboffload_get_calc_task(iboffload,
                        my_rank, coll_request->qp_index, NULL,
                        l_operand,
                        r_operand, coll_request, NO_INLINE);
        if (OPAL_UNLIKELY(NULL == calc_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting calc task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST(mqe_ptr_to_set, calc_task, last_send);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, calc_task);

        preposted_recv_frag =
            mca_bcol_iboffload_get_preposted_recv_frag(
                    iboffload, my_rank, coll_request->qp_index);
        if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
            /* RLG need cleanup */
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        /* Wait for calc from myself */
        wait_task = mca_bcol_iboffload_get_wait_task(iboffload, my_rank, 1,
                        preposted_recv_frag, coll_request->qp_index, NULL);
        if (OPAL_UNLIKELY(NULL == wait_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);
    } else {
        coll_request->l_operand = (uint64_t) (uintptr_t)
            ((unsigned char *)l_operand->addr);
        coll_request->r_operand = (uint64_t) (uintptr_t)
            ((unsigned char *) (coll_request->l_operand) + ALLREDUCE_BASE_OFFSET);
    }

    *mqe_ptr_to_set = NULL;
/* Vasily: TODO with MACRO */
    /* finish initializing full message descriptor */
    coll_request->n_fragments  = 1;
    coll_request->n_frags_sent = 1;

    /* Pasha: need to set to true in upper layer */
    coll_request->user_handle_freed = false;

    last_wait->flags |= MQE_WR_FLAG_SIGNAL;

    coll_fragment->signal_task_wr_id = last_wait->wr_id;
    last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;

    /* post the mwr */
    IBOFFLOAD_VERBOSE(10, ("Post tasks.\n"));
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload, coll_fragment->to_post);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload->super, coll_request->order_info);

    return OMPI_SUCCESS;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Adding collfrag to collfrag_pending"));
    return mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload);
}
/*
 * non power of 2 & EXCHANGE_NODE case,
 * need to wait for message from "extra" proc.
 */
static int
non_pure_recursive_doubling(mca_bcol_iboffload_module_t *iboffload,
                            mca_bcol_iboffload_collreq_t *coll_request)
{
    /* local variables */
    int rc = OMPI_SUCCESS, extra_rank, pair_rank,
        my_rank = ((mca_sbgp_base_module_t *) iboffload->ibnet)->my_index;

    mca_bcol_iboffload_frag_t *calc_fragment,
                              *preposted_recv_frag;

    mca_bcol_iboffload_task_t *wait_task,
                              *calc_task;

    struct ibv_sge *r_operand = NULL,
                   *l_operand = NULL;

    struct mqe_task *last_wait, /* we need ask from completion on last wait */
                    *last_send;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    netpatterns_pair_exchange_node_t *my_exchange_node =
                                          &iboffload->recursive_doubling_tree;

    struct mqe_task **mqe_ptr_to_set;
    mca_bcol_iboffload_collfrag_t *coll_fragment = (mca_bcol_iboffload_collfrag_t *)
                                                   opal_list_get_last(&coll_request->work_requests);

    mqe_ptr_to_set = &coll_fragment->to_post;

    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                 iboffload, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));

        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    IBOFFLOAD_VERBOSE(10, ("Allreduce starting: type %d op %d, "
                       "n_extra_sources - %d.\n", cm->map_ompi_to_ib_dt[coll_request->dtype->id],
                        cm->map_ompi_to_ib_calcs[coll_request->op->op_type],
                        my_exchange_node->n_extra_sources));

    extra_rank = my_exchange_node->rank_extra_source;

    preposted_recv_frag =
        mca_bcol_iboffload_get_preposted_recv_frag(
                iboffload, extra_rank, coll_request->qp_index);
    if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
        /* RLG need cleanup */
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    /* Wait for data from extra node */
    wait_task = mca_bcol_iboffload_get_wait_task(iboffload, extra_rank, 1,
                            preposted_recv_frag, coll_request->qp_index, NULL);
    if (OPAL_UNLIKELY(NULL == wait_task)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

    pair_rank = my_exchange_node->rank_exchanges[0];

    calc_fragment = mca_bcol_iboffload_get_send_frag(coll_request,
                            pair_rank, coll_request->qp_index,
                            MCA_IBOFFLOAD_IB_DRIVER_OPERAND_SIZE +
                            MCA_IBOFFLOAD_CALC_SIZE_EXT, 0,
                            SBUF,
                            MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML_CALC);
    if (OPAL_UNLIKELY(NULL == calc_fragment)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting and packing send frag.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    /* Calc extra node operand with mine and send the result
       to the first algorithm partner */
    preposted_recv_frag->sg_entry.length = MCA_IBOFFLOAD_IB_DRIVER_OPERAND_SIZE +
                                           MCA_IBOFFLOAD_CALC_SIZE_EXT;
    calc_task = mca_bcol_iboffload_get_calc_task(iboffload,
                        pair_rank, coll_request->qp_index, calc_fragment,
                        &preposted_recv_frag->sg_entry,
                        &calc_fragment->sg_entry, coll_request, NO_INLINE);
    if (OPAL_UNLIKELY(NULL == calc_task)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting calc task.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    APPEND_TO_TASKLIST(mqe_ptr_to_set, calc_task, last_send);
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, calc_task);

    /* Calc extra node operand with mine and store the result on my buff */
    calc_task = mca_bcol_iboffload_get_calc_task(iboffload,
                        my_rank, coll_request->qp_index, NULL,
                        &preposted_recv_frag->sg_entry,
                        &calc_fragment->sg_entry, coll_request, NO_INLINE);
    if (OPAL_UNLIKELY(NULL == calc_task)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting calc task.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    APPEND_TO_TASKLIST(mqe_ptr_to_set, calc_task, last_send);
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, calc_task);

    preposted_recv_frag =
        mca_bcol_iboffload_get_preposted_recv_frag(
                iboffload, my_rank, coll_request->qp_index);
    if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
        /* RLG need cleanup */
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    /* Wait for calc from myself */
    wait_task = mca_bcol_iboffload_get_wait_task(iboffload, my_rank, 1,
                            preposted_recv_frag, coll_request->qp_index, NULL);
    if (OPAL_UNLIKELY(NULL == wait_task)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);

    l_operand = &preposted_recv_frag->sg_entry;
    l_operand->length = MCA_IBOFFLOAD_IB_DRIVER_OPERAND_SIZE +
                        MCA_IBOFFLOAD_CALC_SIZE_EXT;
    /* Recursive-doubling exchange */
    rc = do_exchange(iboffload, coll_request, &mqe_ptr_to_set,
                     &last_wait, &l_operand, &r_operand);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    /* Need to send message to "extra" proc =>
       one more final result calc for extra node */
    calc_task = mca_bcol_iboffload_get_calc_task(iboffload,
                        extra_rank, coll_request->qp_index, NULL,
                        l_operand,
                        r_operand, coll_request, NO_INLINE);
    if (OPAL_UNLIKELY(NULL == calc_task)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for getting calc task.\n"));
        rc = OMPI_ERR_RESOURCE_BUSY;
        goto out_of_resources;
    }

    APPEND_TO_TASKLIST(mqe_ptr_to_set, calc_task, last_send);
    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, calc_task);

    if (false == coll_request->do_calc_in_cpu) {
        /* Calc and send the result to myself */
        calc_task = mca_bcol_iboffload_get_calc_task(iboffload,
                        my_rank, coll_request->qp_index, NULL,
                        l_operand,
                        r_operand, coll_request, NO_INLINE);
        if (OPAL_UNLIKELY(NULL == calc_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting calc task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST(mqe_ptr_to_set, calc_task, last_send);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, calc_task);

        preposted_recv_frag =
            mca_bcol_iboffload_get_preposted_recv_frag(
                    iboffload, my_rank, coll_request->qp_index);
        if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
            /* RLG need cleanup */
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        /* Wait for calc from myself */
        wait_task = mca_bcol_iboffload_get_wait_task(iboffload, my_rank, 1,
                        preposted_recv_frag, coll_request->qp_index, NULL);
        if (OPAL_UNLIKELY(NULL == wait_task)) {
            IBOFFLOAD_VERBOSE(10, ("Failing for getting wait task.\n"));
            rc = OMPI_ERR_RESOURCE_BUSY;
            goto out_of_resources;
        }

        APPEND_TO_TASKLIST(mqe_ptr_to_set, wait_task, last_wait);
        MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);
    } else {
        coll_request->l_operand = l_operand->addr;
        coll_request->r_operand = r_operand->addr;
    }

    *mqe_ptr_to_set = NULL;

    /* finish initializing full message descriptor */
    coll_request->n_fragments  = 1;
    coll_request->n_frags_sent = 1;

    assert(NULL != last_wait);

    last_wait->flags |= MQE_WR_FLAG_SIGNAL;
    coll_fragment->signal_task_wr_id = last_wait->wr_id;
    last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;

    /* post the mwr */
    IBOFFLOAD_VERBOSE(10, ("Post tasks.\n"));
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload, coll_fragment->to_post);
    if(OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload->super, coll_request->order_info);

    return OMPI_SUCCESS;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Adding collfrag to collfrag_pending"));
    return mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload);
}

static int mca_bcol_iboffload_allreduce_init(
                               bcol_function_args_t *fn_arguments,
                               mca_bcol_iboffload_module_t *iboffload,
                               struct mca_bcol_iboffload_collreq_t **coll_request,
                               bool if_bcol_last)
{
    int rc;

    bool exclude_case;
    ompi_free_list_item_t *item;
    mca_bcol_iboffload_collfrag_t *coll_fragment;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    IBOFFLOAD_VERBOSE(10, ("Calling for mca_bcol_iboffload_allreduce_init.\n"));

    OMPI_FREE_LIST_WAIT(&cm->collreqs_free, item, rc);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Failing for coll request free list waiting.\n"));
        return rc;
    }

    (*coll_request) = (mca_bcol_iboffload_collreq_t *) item;
    (*coll_request)->progress_fn = iboffload->allreduce_algth;

    (*coll_request)->if_bcol_last = if_bcol_last;

    exclude_case = (non_pure_recursive_doubling == iboffload->allreduce_algth &&
                                    (OMPI_OP_SUM == fn_arguments->op->op_type &&
                                     OMPI_DATATYPE_MPI_DOUBLE == fn_arguments->dtype->id));

    (*coll_request)->do_calc_in_cpu = cm->last_calc_in_cpu && !exclude_case;

    if (false == (*coll_request)->do_calc_in_cpu ||
            allreduce_extra_node == iboffload->allreduce_algth) {
        (*coll_request)->do_calc_in_cpu = false; /* Relevant for extra node only */
        (*coll_request)->completion_cb_fn =
                        mca_bcol_iboffload_unpack_res_to_user;
    } else {
        (*coll_request)->completion_cb_fn =
                       mca_bcol_iboffload_calc_res_to_user;
    }

    (*coll_request)->module = iboffload;
    (*coll_request)->op = fn_arguments->op;

    (*coll_request)->dtype = fn_arguments->dtype;
    (*coll_request)->count = fn_arguments->count;

    (*coll_request)->ml_buffer_index = fn_arguments->buffer_index;
    (*coll_request)->buffer_info[SBUF].lkey = iboffload->rdma_block.ib_info.lkey;

    (*coll_request)->order_info = &fn_arguments->order_info;

    /* ML buffer was provided, no need to pack the data.
     * It is few assumption here:
     * we CAN touch and change ML buffer
     */
    (*coll_request)->buffer_info[SBUF].buf = (void *) (
            (unsigned char *) fn_arguments->sbuf +
                     (size_t) fn_arguments->sbuf_offset);

    (*coll_request)->buffer_info[SBUF].offset = fn_arguments->sbuf_offset;

    (*coll_request)->buffer_info[RBUF].buf = (void *) (
        (unsigned char *) fn_arguments->rbuf +
                 (size_t) fn_arguments->rbuf_offset);

    (*coll_request)->buffer_info[RBUF].offset = fn_arguments->rbuf_offset;

    if(mca_bcol_iboffload_component.enable_rdma_calc) {
        (*coll_request)->qp_index = MCA_BCOL_IBOFFLOAD_QP_BARRIER;
    } else {
        (*coll_request)->qp_index = MCA_BCOL_IBOFFLOAD_QP_REGULAR;
    }

    (*coll_request)->n_frag_mpi_complete = 0;
    (*coll_request)->n_frag_net_complete = 0;

    fn_arguments->bcol_opaque_data = (void *) (*coll_request);

    /*
     * setup collective work request
     */

    /* get collective frag */
    coll_fragment = &((*coll_request)->first_collfrag);
    mca_bcol_iboffload_collfrag_init(coll_fragment);

    coll_fragment->mq_index = COLL_MQ;
    coll_fragment->alg = RECURSIVE_DOUBLING_ALLREDUCE_ALG;

    coll_fragment->mq_credits =
                iboffload->alg_task_consump[RECURSIVE_DOUBLING_ALLREDUCE_ALG];

    /* set pointers for (coll frag) <-> (coll full request) */
    MCA_BCOL_IBOFFLOAD_SET_COLL_REQ_LINKS(*coll_request, coll_fragment);

    coll_fragment->unpack_size =
                mca_bcol_base_get_buff_length(fn_arguments->dtype, fn_arguments->count);

    IBOFFLOAD_VERBOSE(10, ("The input data is %lf", *(double *) (*coll_request)->buffer_info[SBUF].buf));

    return OMPI_SUCCESS;
}

static int mca_bcol_iboffload_allreduce_intra(bcol_function_args_t *fn_arguments,
                                              struct coll_ml_function_t *const_args)
{
    /* local variables */
    int rc;

    mca_bcol_iboffload_collreq_t *coll_request = NULL;
    mca_bcol_iboffload_module_t *iboffload =
                 (mca_bcol_iboffload_module_t *) const_args->bcol_module;

    /* Pasha: please do not touch this line, it used for ML buffer recycling barrier call */
    bool if_bcol_last = ((const_args->index_of_this_type_in_collective + 1) ==
                          const_args->n_of_this_type_in_collective);

    MCA_BCOL_CHECK_ORDER(const_args->bcol_module, fn_arguments);

    IBOFFLOAD_VERBOSE(10, ("n_of_this_type_in_a_row %d, index_in_consecutive_same_bcol_calls %d",
                            const_args->n_of_this_type_in_a_row,
                            const_args->index_in_consecutive_same_bcol_calls + 1));

    IBOFFLOAD_VERBOSE(10, ("Allreduce started.\n"));
    fn_arguments->result_in_rbuf = true;

    rc = mca_bcol_iboffload_allreduce_init(fn_arguments, iboffload,
                                           &coll_request, if_bcol_last);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Get error from mca_bcol_iboffload_allreduce_init.\n"));
        return rc;
    }

    /* Allreduce starting */
    rc = iboffload->allreduce_algth(iboffload, coll_request);
    if (OPAL_UNLIKELY(OMPI_ERROR == rc)) {
        return BCOL_FN_NOT_STARTED;
    }

    IBOFFLOAD_VERBOSE(10, ("Wait for completions.\n"));

    /* done */
    return BCOL_FN_STARTED;
}

static int mca_bcol_iboffload_allreduce_progress(
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

        IBOFFLOAD_VERBOSE(10, ("Allreduce already done.\n"));
        return BCOL_FN_COMPLETE;
    }

    return BCOL_FN_STARTED;
}

int mca_bcol_iboffload_allreduce_first_call(mca_bcol_iboffload_module_t *iboffload,
                                            mca_bcol_iboffload_collreq_t *coll_request)
{
    netpatterns_pair_exchange_node_t *my_exchange_node =
                                          &iboffload->recursive_doubling_tree;

    int i = 0, my_rank = iboffload->ibnet->super.my_index,
        n_exchanges = my_exchange_node->n_exchanges,
        *exchanges = my_exchange_node->rank_exchanges,
        n_extra_src = my_exchange_node->n_extra_sources,
        rank_extra_src = my_exchange_node->rank_extra_source;

    mca_bcol_iboffload_endpoint_t *ep = iboffload->endpoints[my_rank];

    /* Connecting to myself */
    while (OMPI_SUCCESS !=
            check_endpoint_state(ep, NULL, NULL)) {
        opal_progress();
    }

    iboffload->alg_task_consump[RECURSIVE_DOUBLING_ALLREDUCE_ALG] = 0;

    if (0 < n_extra_src) {
        iboffload->alg_task_consump[RECURSIVE_DOUBLING_ALLREDUCE_ALG] += 4; /* Two CALCs and two WAITs tasks */
        ep = iboffload->endpoints[rank_extra_src];
        while (OMPI_SUCCESS !=
                check_endpoint_state(ep, NULL, NULL)) {
            opal_progress();
        }
    }

    for (i = 0; i < n_exchanges; ++i) {
        iboffload->alg_task_consump[RECURSIVE_DOUBLING_ALLREDUCE_ALG] += 4; /* Two CALCs and two WAITs tasks */
        ep = iboffload->endpoints[exchanges[i]];

        while (OMPI_SUCCESS !=
                check_endpoint_state(ep, NULL, NULL)) {
            opal_progress();
        }
    }

    iboffload->alg_task_consump[RECURSIVE_DOUBLING_ALLREDUCE_ALG] += 4; /* Two CALCs and two WAITs tasks */

    if (0 < my_exchange_node->n_extra_sources) {
        iboffload->allreduce_algth =
                (EXTRA_NODE == my_exchange_node->node_type)?
                 allreduce_extra_node:
                 non_pure_recursive_doubling;
    } else {
        if(mca_bcol_iboffload_component.enable_rdma_calc) {
            iboffload->allreduce_algth =
                rdma_pure_recursive_doubling;
        } else {
            iboffload->allreduce_algth =
                pure_recursive_doubling;
        }
    }

    return iboffload->allreduce_algth(iboffload, coll_request);
}

int mca_bcol_iboffload_allreduce_register(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    IBOFFLOAD_VERBOSE(10, ("Register iboffload Allreduce.\n"));

    comm_attribs.bcoll_type = BCOL_ALLREDUCE;

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
            mca_bcol_iboffload_allreduce_intra,
            mca_bcol_iboffload_allreduce_progress);

    return OMPI_SUCCESS;
}
