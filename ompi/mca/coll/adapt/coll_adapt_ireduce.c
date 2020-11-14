/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/communicator/communicator.h"
#include "coll_adapt.h"
#include "coll_adapt_algorithms.h"
#include "coll_adapt_context.h"
#include "coll_adapt_item.h"
#include "coll_adapt_topocache.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/base/coll_base_util.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_base_topo.h"

static int ompi_coll_adapt_ireduce_generic(IREDUCE_ARGS,
                                           ompi_coll_tree_t * tree, size_t seg_size);

/* MPI_Reduce and MPI_Ireduce in the ADAPT module only work for commutative operations */

/*
 * Set up MCA parameters of MPI_Reduce and MPI_Ireduce
 */
int ompi_coll_adapt_ireduce_register(void)
{
    mca_base_component_t *c = &mca_coll_adapt_component.super.collm_version;

    mca_coll_adapt_component.adapt_ireduce_algorithm = 1;
    mca_base_component_var_register(c, "reduce_algorithm",
                                    "Algorithm of reduce, 1: binomial, 2: in_order_binomial, 3: binary, 4: pipeline, 5: chain, 6: linear", MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
                                    &mca_coll_adapt_component.adapt_ireduce_algorithm);
    if( (mca_coll_adapt_component.adapt_ireduce_algorithm < 0) ||
        (mca_coll_adapt_component.adapt_ireduce_algorithm > OMPI_COLL_ADAPT_ALGORITHM_COUNT) ) {
        mca_coll_adapt_component.adapt_ireduce_algorithm = 1;
    }

    mca_coll_adapt_component.adapt_ireduce_segment_size = 163740;
    mca_base_component_var_register(c, "reduce_segment_size",
                                    "Segment size in bytes used by default for reduce algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation.",
                                    MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &mca_coll_adapt_component.adapt_ireduce_segment_size);

    mca_coll_adapt_component.adapt_ireduce_max_send_requests = 2;
    mca_base_component_var_register(c, "reduce_max_send_requests",
                                    "Maximum number of send requests",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &mca_coll_adapt_component.adapt_ireduce_max_send_requests);

    mca_coll_adapt_component.adapt_ireduce_max_recv_requests = 3;
    mca_base_component_var_register(c, "reduce_max_recv_requests",
                                    "Maximum number of receive requests per peer",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &mca_coll_adapt_component.adapt_ireduce_max_recv_requests);

    mca_coll_adapt_component.adapt_inbuf_free_list_min = 10;
    mca_base_component_var_register(c, "inbuf_free_list_min",
                                    "Minimum number of segment in inbuf free list",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &mca_coll_adapt_component.adapt_inbuf_free_list_min);

    mca_coll_adapt_component.adapt_inbuf_free_list_max = 10000;
    mca_base_component_var_register(c, "inbuf_free_list_max",
                                    "Maximum number of segment in inbuf free list",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &mca_coll_adapt_component.adapt_inbuf_free_list_max);


    mca_coll_adapt_component.adapt_inbuf_free_list_inc = 10;
    mca_base_component_var_register(c, "inbuf_free_list_inc",
                                    "Number of segments to allocate when growing the inbuf free list",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &mca_coll_adapt_component.adapt_inbuf_free_list_inc);

    mca_coll_adapt_component.adapt_ireduce_synchronous_send = true;
    (void) mca_base_component_var_register(c, "reduce_synchronous_send",
                                           "Whether to use synchronous send operations during setup of reduce operations",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_adapt_component.adapt_ireduce_synchronous_send);

    mca_coll_adapt_component.adapt_ireduce_context_free_list = NULL;
    return OMPI_SUCCESS;
}

/*
 * Release the free list created in ompi_coll_adapt_ireduce_generic
 */
int ompi_coll_adapt_ireduce_fini(void)
{
    if (NULL != mca_coll_adapt_component.adapt_ireduce_context_free_list) {
        OBJ_RELEASE(mca_coll_adapt_component.adapt_ireduce_context_free_list);
        mca_coll_adapt_component.adapt_ireduce_context_free_list = NULL;
        OPAL_OUTPUT_VERBOSE((10, mca_coll_adapt_component.adapt_output, "ireduce fini\n"));
    }
    return OMPI_SUCCESS;
}

/*
 * Functions to access list
 */
static ompi_coll_adapt_item_t *get_next_ready_item(ompi_coll_adapt_constant_reduce_context_t *con, int num_children)
{
    ompi_coll_adapt_item_t *item = NULL, *temp_item;
    if (opal_list_is_empty(&con->recv_list)) {
        return NULL;
    }
    OPAL_THREAD_LOCK(&con->mutex_recv_list);
    OPAL_LIST_FOREACH(temp_item, &con->recv_list, ompi_coll_adapt_item_t) {
        if (temp_item->count == num_children) {
            item = temp_item;
            opal_list_remove_item(&con->recv_list, (opal_list_item_t *) temp_item);
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&con->mutex_recv_list);
    return item;
}

static int add_to_recv_list(ompi_coll_adapt_constant_reduce_context_t *con, int id)
{
    ompi_coll_adapt_item_t *item;

    OPAL_THREAD_LOCK(&con->mutex_recv_list);
    OPAL_LIST_FOREACH(item, &con->recv_list, ompi_coll_adapt_item_t) {
        if (item->id == id) {
            (item->count)++;
            OPAL_THREAD_UNLOCK(&con->mutex_recv_list);
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output, "add_to_recv_list_return 1\n"));
            return 1;
        }
    }

    /* Add a new object to the list with count set to 1 */
    item = OBJ_NEW(ompi_coll_adapt_item_t);
    item->id = id;
    item->count = 1;
    opal_list_append(&con->recv_list, (opal_list_item_t *) item);
    OPAL_THREAD_UNLOCK(&con->mutex_recv_list);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output, "add_to_recv_list_return 2\n"));
    return 2;
}

/*
 * Get the inbuf address
 */
static ompi_coll_adapt_inbuf_t *to_inbuf(char *buf, int distance)
{
    return (ompi_coll_adapt_inbuf_t *) (buf - distance);
}

/*
 *  Finish a ireduce request
 */
static int ireduce_request_fini(ompi_coll_adapt_reduce_context_t * context)
{
    /* Return the allocated recourses */
    ompi_request_t *temp_req = context->con->request;
    if (context->con->accumbuf != NULL) {
        if (context->con->rank != context->con->root) {
            for (int i = 0; i < context->con->num_segs; i++) {
                OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                     "[%d]: Return accumbuf %d %p\n",
                                     ompi_comm_rank(context->con->comm), i,
                                     (void *) to_inbuf(context->con->accumbuf[i],
                                                       context->con->distance)));
                opal_free_list_return_st(&context->con->inbuf_list,
                                      (opal_free_list_item_t *) to_inbuf(context->con->accumbuf[i],
                                                                         context->con->distance));
            }
        }
        free(context->con->accumbuf);
    }
    for (int i = 0; i < context->con->num_segs; i++) {
        OBJ_DESTRUCT(&context->con->mutex_op_list[i]);
    }
    free(context->con->mutex_op_list);
    if (context->con->tree->tree_nextsize > 0) {
        free(context->con->next_recv_segs);
    }
    OBJ_RELEASE(context->con);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output, "return context_list\n"));
    opal_free_list_return(mca_coll_adapt_component.adapt_ireduce_context_free_list,
                          (opal_free_list_item_t *) context);
    /* Complete the request */
    ompi_request_complete(temp_req, 1);
    return OMPI_SUCCESS;
}

/*
 * Callback function of isend
 */
static int send_cb(ompi_request_t * req)
{
    ompi_coll_adapt_reduce_context_t *context =
        (ompi_coll_adapt_reduce_context_t *) req->req_complete_cb_data;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: ireduce_send_cb, peer %d, seg_id %d\n", context->con->rank,
                         context->peer, context->seg_index));
    int err;

    opal_atomic_sub_fetch_32(&(context->con->ongoing_send), 1);

    /* Send a new segment */
    ompi_coll_adapt_item_t *item =
        get_next_ready_item(context->con, context->con->tree->tree_nextsize);

    if (item != NULL) {
        /* Get new context item from free list */
        ompi_coll_adapt_reduce_context_t *send_context =
            (ompi_coll_adapt_reduce_context_t *) opal_free_list_wait(mca_coll_adapt_component.
                                                                    adapt_ireduce_context_free_list);
        if (context->con->tree->tree_nextsize > 0) {
            send_context->buff = context->con->accumbuf[item->id];
        } else {
            send_context->buff =
                context->buff + (item->id - context->seg_index) * context->con->segment_increment;
        }
        send_context->seg_index = item->id;
        send_context->peer = context->peer;
        send_context->con = context->con;

        opal_atomic_add_fetch_32(&(context->con->ongoing_send), 1);

        int send_count = send_context->con->seg_count;
        if (item->id == (send_context->con->num_segs - 1)) {
            send_count = send_context->con->count - item->id * send_context->con->seg_count;
        }

        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                             "[%d]: In send_cb, create isend to seg %d, peer %d, tag %d\n",
                             send_context->con->rank, send_context->seg_index, send_context->peer,
                             send_context->con->ireduce_tag - send_context->seg_index));

        ompi_request_t *send_req;
        err = MCA_PML_CALL(isend
                           (send_context->buff, send_count, send_context->con->datatype,
                            send_context->peer,
                            context->con->ireduce_tag - send_context->seg_index,
                            MCA_PML_BASE_SEND_STANDARD, send_context->con->comm, &send_req));
        if (MPI_SUCCESS != err) {
            return err;
        }

        /* Release the item */
        OBJ_RELEASE(item);

        /* Set the send call back */
        ompi_request_set_callback(send_req, send_cb, send_context);
    }

    int32_t num_sent = opal_atomic_add_fetch_32(&(context->con->num_sent_segs), 1);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: In send_cb, root = %d, num_sent = %d, num_segs = %d\n",
                         context->con->rank, context->con->tree->tree_root, num_sent,
                         context->con->num_segs));
    /* Check whether signal the condition, non root and sent all the segments */
    if (num_sent == context->con->num_segs &&
        context->con->num_recv_segs == context->con->num_segs * context->con->tree->tree_nextsize) {
        ireduce_request_fini(context);
    } else {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output, "return context_list\n"));
        opal_free_list_return(mca_coll_adapt_component.adapt_ireduce_context_free_list,
                              (opal_free_list_item_t *) context);
    }
    /* Call back function return 1, which means successful */
    req->req_free(&req);
    return 1;
}

/*
 * Callback function of irecv
 */
static int recv_cb(ompi_request_t * req)
{
    ompi_coll_adapt_reduce_context_t *context = (ompi_coll_adapt_reduce_context_t *) req->req_complete_cb_data;
    int32_t new_id = opal_atomic_add_fetch_32(&(context->con->next_recv_segs[context->child_id]), 1);
    int err;

    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: ireduce_recv_cb, peer %d, seg_id %d\n", context->con->rank,
                         context->peer, context->seg_index));

    /* Did we still need to receive subsequent fragments from this child ? */
    if (new_id < context->con->num_segs) {
        char *temp_recv_buf = NULL;
        ompi_coll_adapt_inbuf_t *inbuf = NULL;
        /* Set inbuf, if it it first child, recv on rbuf, else recv on inbuf */
        if (context->child_id == 0 && context->con->sbuf != MPI_IN_PLACE
            && context->con->root == context->con->rank) {
            temp_recv_buf = (char *) context->con->rbuf +
                            (ptrdiff_t) new_id *(ptrdiff_t) context->con->segment_increment;
        } else {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: In recv_cb, alloc inbuf\n", context->con->rank));
            inbuf = (ompi_coll_adapt_inbuf_t *) opal_free_list_wait(&context->con->inbuf_list);
            temp_recv_buf = inbuf->buff - context->con->lower_bound;
        }
        /* Get new context item from free list */
        ompi_coll_adapt_reduce_context_t *recv_context =
            (ompi_coll_adapt_reduce_context_t *) opal_free_list_wait(mca_coll_adapt_component.
                                                                     adapt_ireduce_context_free_list);
        recv_context->buff = temp_recv_buf;
        recv_context->seg_index = new_id;
        recv_context->child_id = context->child_id;
        recv_context->peer = context->peer;
        recv_context->con = context->con;
        recv_context->inbuf = inbuf;
        int recv_count = recv_context->con->seg_count;
        if (new_id == (recv_context->con->num_segs - 1)) {
            recv_count = recv_context->con->count - new_id * recv_context->con->seg_count;
        }
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                             "[%d]: In recv_cb, create irecv for seg %d, peer %d, inbuf %p, tag %d\n",
                             context->con->rank, recv_context->seg_index, recv_context->peer,
                             (void *) inbuf,
                             recv_context->con->ireduce_tag - recv_context->seg_index));
        ompi_request_t *recv_req;
        err = MCA_PML_CALL(irecv(temp_recv_buf, recv_count, recv_context->con->datatype,
                                 recv_context->peer,
                                 recv_context->con->ireduce_tag - recv_context->seg_index,
                                 recv_context->con->comm, &recv_req));
        if (MPI_SUCCESS != err) {
            return err;
        }
        /* Set the receive call back */
        ompi_request_set_callback(recv_req, recv_cb, recv_context);
    }

    /* Do the op */
    int op_count = context->con->seg_count;
    if (context->seg_index == (context->con->num_segs - 1)) {
        op_count = context->con->count - context->seg_index * context->con->seg_count;
    }

    int keep_inbuf = 0;
    OPAL_THREAD_LOCK(&context->con->mutex_op_list[context->seg_index]);
    if (NULL == context->con->accumbuf[context->seg_index]) {
        if (NULL == context->inbuf) {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: set accumbuf to rbuf\n", context->con->rank));
            context->con->accumbuf[context->seg_index] = context->buff;
        } else {
            keep_inbuf = 1;
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: set accumbuf to inbuf\n", context->con->rank));
            context->con->accumbuf[context->seg_index] = context->inbuf->buff - context->con->lower_bound;
        }
        /* Op sbuf and accmbuf to accumbuf */
        ompi_op_reduce(context->con->op,
                       context->con->sbuf + (ptrdiff_t) context->seg_index * (ptrdiff_t) context->con->segment_increment,
                       context->con->accumbuf[context->seg_index], op_count, context->con->datatype);

    } else {
        if (NULL == context->inbuf) {
            /* Op rbuf and accumbuf to rbuf */
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: op rbuf and accumbuf to rbuf\n", context->con->rank));
            ompi_op_reduce(context->con->op, context->con->accumbuf[context->seg_index],
                           context->buff, op_count, context->con->datatype);
            /* Free old accumbuf */
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: free old accumbuf %p\n", context->con->rank,
                                 (void *) to_inbuf(context->con->accumbuf[context->seg_index],
                                                   context->con->distance)));
            opal_free_list_return(&context->con->inbuf_list,
                                  (opal_free_list_item_t *) to_inbuf(context->con->accumbuf[context->seg_index],
                                                                     context->con->distance));
            /* Set accumbut to rbuf */
            context->con->accumbuf[context->seg_index] = context->buff;
        } else {
            /* Op inbuf and accmbuf to accumbuf */
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: op inbuf and accmbuf to accumbuf\n", context->con->rank));
            ompi_op_reduce(context->con->op, context->inbuf->buff - context->con->lower_bound,
                           context->con->accumbuf[context->seg_index], op_count,
                           context->con->datatype);
        }
    }
    OPAL_THREAD_UNLOCK(&context->con->mutex_op_list[context->seg_index]);

    /* Set recv list */
    if (context->con->rank != context->con->tree->tree_root) {
        add_to_recv_list(context->con, context->seg_index);
    }

    /* Send to parent */
    if (context->con->rank != context->con->tree->tree_root
        && context->con->ongoing_send < mca_coll_adapt_component.adapt_ireduce_max_send_requests) {
        ompi_coll_adapt_item_t *item = get_next_ready_item(context->con, context->con->tree->tree_nextsize);

        if (NULL != item) {
            /* Get new context item from free list */
            ompi_coll_adapt_reduce_context_t *send_context =
                (ompi_coll_adapt_reduce_context_t *) opal_free_list_wait(mca_coll_adapt_component.
                                                                        adapt_ireduce_context_free_list);
            send_context->buff = context->con->accumbuf[context->seg_index];
            send_context->seg_index = item->id;
            send_context->peer = context->con->tree->tree_prev;
            send_context->con = context->con;
            opal_atomic_add_fetch_32(&(context->con->ongoing_send), 1);

            int send_count = send_context->con->seg_count;
            if (item->id == (send_context->con->num_segs - 1)) {
                send_count = send_context->con->count - item->id * send_context->con->seg_count;
            }
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: In recv_cb, create isend to seg %d, peer %d, tag %d\n",
                                 send_context->con->rank, send_context->seg_index, send_context->peer,
                                 send_context->con->ireduce_tag - send_context->seg_index));

            ompi_request_t *send_req;
            err = MCA_PML_CALL(isend(send_context->buff, send_count, send_context->con->datatype,
                                     send_context->peer,
                                     send_context->con->ireduce_tag - send_context->seg_index,
                                     MCA_PML_BASE_SEND_STANDARD, send_context->con->comm, &send_req));
            if (MPI_SUCCESS != err) {
                return err;
            }
            OBJ_RELEASE(item);

            /* Set the send call back */
            ompi_request_set_callback(send_req, send_cb, send_context);
        }
    }

    int32_t num_recv_segs = opal_atomic_add_fetch_32(&(context->con->num_recv_segs), 1);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: In recv_cb, tree = %p, root = %d, num_recv = %d, num_segs = %d, num_child = %d\n",
                         context->con->rank, (void *) context->con->tree,
                         context->con->tree->tree_root, num_recv_segs, context->con->num_segs,
                         context->con->tree->tree_nextsize));
    /* Prepare for releasing all acquired resources */
    if (!keep_inbuf && NULL != context->inbuf) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                             "[%d]: root free context inbuf %p", context->con->rank,
                             (void *) context->inbuf));
        opal_free_list_return(&context->con->inbuf_list,
                              (opal_free_list_item_t *) context->inbuf);
    }
    /* If this is root and has received all the segments */
    if (num_recv_segs == context->con->num_segs * context->con->tree->tree_nextsize &&
        (context->con->tree->tree_root == context->con->rank || context->con->num_sent_segs == context->con->num_segs)) {
        ireduce_request_fini(context);
    } else {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output, "[%d]: return context_list",
                             context->con->rank));
        opal_free_list_return(mca_coll_adapt_component.adapt_ireduce_context_free_list,
                              (opal_free_list_item_t *) context);
    }
    req->req_free(&req);
    return 1;
}

int ompi_coll_adapt_ireduce(const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
                           struct ompi_op_t *op, int root, struct ompi_communicator_t *comm,
                           ompi_request_t ** request, mca_coll_base_module_t * module)
{

    /* Fall-back if operation is commutative */
    if (!ompi_op_is_commute(op)){
        mca_coll_adapt_module_t *adapt_module = (mca_coll_adapt_module_t *) module;
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                    "ADAPT cannot handle reduce with this (non-commutative) operation. It needs to fall back on another component\n"));
        return adapt_module->previous_ireduce(sbuf, rbuf, count, dtype, op, root,
                                              comm, request,
                                              adapt_module->previous_reduce_module);
    }


    OPAL_OUTPUT_VERBOSE((10, mca_coll_adapt_component.adapt_output,
                         "ireduce root %d, algorithm %d, coll_adapt_ireduce_segment_size %zu, coll_adapt_ireduce_max_send_requests %d, coll_adapt_ireduce_max_recv_requests %d\n",
                         root, mca_coll_adapt_component.adapt_ireduce_algorithm,
                         mca_coll_adapt_component.adapt_ireduce_segment_size,
                         mca_coll_adapt_component.adapt_ireduce_max_send_requests,
                         mca_coll_adapt_component.adapt_ireduce_max_recv_requests));

    if (OMPI_COLL_ADAPT_ALGORITHM_TUNED == mca_coll_adapt_component.adapt_ireduce_algorithm) {
        OPAL_OUTPUT_VERBOSE((10, mca_coll_adapt_component.adapt_output, "tuned not implemented\n"));
        return OMPI_ERR_NOT_IMPLEMENTED;
    }


    return ompi_coll_adapt_ireduce_generic(sbuf, rbuf, count, dtype, op, root, comm, request, module,
                                           adapt_module_cached_topology(module, comm, root, mca_coll_adapt_component.adapt_ireduce_algorithm),
                                           mca_coll_adapt_component.adapt_ireduce_segment_size);

}


int ompi_coll_adapt_ireduce_generic(const void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                    struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    mca_coll_base_module_t * module, ompi_coll_tree_t * tree,
                                    size_t seg_size)
{

    ptrdiff_t extent, lower_bound, segment_increment;
    ptrdiff_t true_lower_bound, true_extent, real_seg_size;
    size_t typelng;
    int seg_count = count, num_segs, rank, recv_count, send_count, err, min;
    /* Used to store the accumuate result, pointer to every segment */
    char **accumbuf = NULL;
    opal_mutex_t *mutex_op_list;
    /* A list to store the segments need to be sent */
    mca_pml_base_send_mode_t sendmode = (mca_coll_adapt_component.adapt_ireduce_synchronous_send)
                                        ? MCA_PML_BASE_SEND_SYNCHRONOUS : MCA_PML_BASE_SEND_STANDARD;

    /* Determine number of segments and number of elements sent per operation */
    rank = ompi_comm_rank(comm);
    ompi_datatype_get_extent(dtype, &lower_bound, &extent);
    ompi_datatype_type_size(dtype, &typelng);
    COLL_BASE_COMPUTED_SEGCOUNT(seg_size, typelng, seg_count);
    num_segs = (count + seg_count - 1) / seg_count;
    segment_increment = (ptrdiff_t) seg_count *extent;
    ompi_datatype_get_true_extent(dtype, &true_lower_bound, &true_extent);
    real_seg_size = true_extent + (ptrdiff_t) (seg_count - 1) * extent;

    /* Atomically set up free list */
    if (NULL == mca_coll_adapt_component.adapt_ireduce_context_free_list) {
        opal_free_list_t* fl = OBJ_NEW(opal_free_list_t);
        opal_free_list_init(fl,
                            sizeof(ompi_coll_adapt_reduce_context_t),
                            opal_cache_line_size,
                            OBJ_CLASS(ompi_coll_adapt_reduce_context_t),
                            0, opal_cache_line_size,
                            mca_coll_adapt_component.adapt_context_free_list_min,
                            mca_coll_adapt_component.adapt_context_free_list_max,
                            mca_coll_adapt_component.adapt_context_free_list_inc,
                            NULL, 0, NULL, NULL, NULL);
        if( !OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&mca_coll_adapt_component.adapt_ireduce_context_free_list,
                                                     &(intptr_t){0}, fl) ) {
            OBJ_RELEASE(fl);
        }
    }

    ompi_coll_base_nbc_request_t *temp_request = NULL;
    /* Set up request */
    temp_request = OBJ_NEW(ompi_coll_base_nbc_request_t);
    OMPI_REQUEST_INIT(&temp_request->super, false);
    temp_request->super.req_state = OMPI_REQUEST_ACTIVE;
    temp_request->super.req_type = OMPI_REQUEST_COLL;
    temp_request->super.req_free = ompi_coll_adapt_request_free;
    temp_request->super.req_status.MPI_SOURCE = 0;
    temp_request->super.req_status.MPI_TAG = 0;
    temp_request->super.req_status.MPI_ERROR = 0;
    temp_request->super.req_status._cancelled = 0;
    temp_request->super.req_status._ucount = 0;
    *request = (ompi_request_t*)temp_request;

    /* Set up mutex */
    mutex_op_list = (opal_mutex_t *) malloc(sizeof(opal_mutex_t) * num_segs);
    for (int32_t i = 0; i < num_segs; i++) {
        OBJ_CONSTRUCT(&mutex_op_list[i], opal_mutex_t);
    }

    /* Set constant context for send and recv call back */
    ompi_coll_adapt_constant_reduce_context_t *con =
        OBJ_NEW(ompi_coll_adapt_constant_reduce_context_t);
    con->count = count;
    con->seg_count = seg_count;
    con->datatype = dtype;
    con->comm = comm;
    con->segment_increment = segment_increment;
    con->num_segs = num_segs;
    con->request = (ompi_request_t*)temp_request;
    con->rank = rank;
    con->num_recv_segs = 0;
    con->num_sent_segs = 0;
    con->ongoing_send  = 0;
    con->mutex_op_list = mutex_op_list;
    con->op = op;
    con->tree = tree;
    con->lower_bound = lower_bound;
    con->sbuf = (char *) sbuf;
    con->rbuf = (char *) rbuf;
    con->root = root;
    con->distance = 0;
    con->ireduce_tag = ompi_coll_base_nbc_reserve_tags(comm, num_segs);
    con->real_seg_size = real_seg_size;

    /* If the current process is not leaf */
    if (tree->tree_nextsize > 0) {
        size_t num_allocate_elems = mca_coll_adapt_component.adapt_inbuf_free_list_min;
        if (((size_t) tree->tree_nextsize * num_segs) < num_allocate_elems) {
            num_allocate_elems = tree->tree_nextsize * num_segs;
        }
        opal_free_list_init(&con->inbuf_list,
                            sizeof(ompi_coll_adapt_inbuf_t) + real_seg_size,
                            opal_cache_line_size,
                            OBJ_CLASS(ompi_coll_adapt_inbuf_t),
                            0, opal_cache_line_size,
                            num_allocate_elems,
                            mca_coll_adapt_component.adapt_inbuf_free_list_max,
                            mca_coll_adapt_component.adapt_inbuf_free_list_inc,
                            NULL, 0, NULL, NULL, NULL);
        /* Set up next_recv_segs */
        con->next_recv_segs = (int32_t *) malloc(sizeof(int32_t) * tree->tree_nextsize);
        ompi_coll_adapt_inbuf_t *temp_inbuf =
            (ompi_coll_adapt_inbuf_t *) opal_free_list_wait_st(&con->inbuf_list);
        con->distance = (char *) temp_inbuf->buff - lower_bound - (char *) temp_inbuf;       //address of inbuf->buff to address of inbuf
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                             "[%d]: distance %d, inbuf %p, inbuf->buff %p, inbuf->buff-lb %p, to_inbuf %p, inbuf_list %p\n",
                             rank, con->distance, (void *) temp_inbuf, (void *) temp_inbuf->buff,
                             (void *) ((char *) temp_inbuf->buff - lower_bound),
                             (void *) to_inbuf((char *) temp_inbuf->buff - lower_bound, con->distance),
                             (void *) &con->inbuf_list));
        opal_free_list_return_st(&con->inbuf_list, (opal_free_list_item_t *) temp_inbuf);
    } else {
        con->next_recv_segs = NULL;
    }

    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: start ireduce root %d tag %d\n", rank, tree->tree_root,
                         con->ireduce_tag));

    /* If the current process is not leaf node */
    if (tree->tree_nextsize > 0) {
        /* Set up accumbuf */
        accumbuf = (char **) malloc(sizeof(char *) * num_segs);
        if (root == rank && sbuf == MPI_IN_PLACE) {
            for (int32_t i = 0; i < num_segs; i++) {
                accumbuf[i] = (char *) rbuf + (ptrdiff_t) i *(ptrdiff_t) segment_increment;
            }
        } else {
            for (int32_t i = 0; i < num_segs; i++) {
                accumbuf[i] = NULL;
            }
        }

        con->accumbuf = accumbuf;

        /* For the first batch of segments */
        min = mca_coll_adapt_component.adapt_ireduce_max_recv_requests;
        if (num_segs < mca_coll_adapt_component.adapt_ireduce_max_recv_requests) {
            min = num_segs;
        }
        for (int32_t i = 0; i < tree->tree_nextsize; i++) {
            con->next_recv_segs[i] = min - 1;
        }

        int num_recvs = 0;
        for (int32_t seg_index = 0; seg_index < min; seg_index++)
        {
            /* For each child */
            for (int32_t i = 0; i < tree->tree_nextsize; i++) {
                recv_count = seg_count;
                if (seg_index == (num_segs - 1)) {
                    recv_count = count - (ptrdiff_t) seg_count *(ptrdiff_t) seg_index;
                }
                char *temp_recv_buf = NULL;
                ompi_coll_adapt_inbuf_t *inbuf = NULL;
                /* Set inbuf, if it it first child, recv on rbuf, else recv on inbuf */
                if (i == 0 && sbuf != MPI_IN_PLACE && root == rank) {
                    temp_recv_buf = (char *) rbuf + (ptrdiff_t) seg_index *(ptrdiff_t) segment_increment;
                } else {
                    inbuf = (ompi_coll_adapt_inbuf_t *) opal_free_list_wait(&con->inbuf_list);
                    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                          "[%d]: In ireduce, alloc inbuf %p\n", rank,
                                          (void *) inbuf));
                    temp_recv_buf = inbuf->buff - lower_bound;
                }
                /* Get context */
                ompi_coll_adapt_reduce_context_t *context =
                    (ompi_coll_adapt_reduce_context_t *)opal_free_list_wait(mca_coll_adapt_component.
                                                                            adapt_ireduce_context_free_list);
                context->buff = temp_recv_buf;
                context->seg_index = seg_index;
                context->child_id = i;      //the id of peer in in the tree
                context->peer = tree->tree_next[i]; //the actual rank of the peer
                context->con = con;
                context->inbuf = inbuf;

                OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                      "[%d]: In ireduce, create irecv for seg %d, peer %d, recv_count %d, inbuf %p tag %d\n",
                                      context->con->rank, context->seg_index, context->peer,
                                      recv_count, (void *) inbuf,
                                      con->ireduce_tag - seg_index));

                /* Create a recv request */
                ompi_request_t *recv_req;
                err = MCA_PML_CALL(irecv
                                    (temp_recv_buf, recv_count, dtype, tree->tree_next[i],
                                    con->ireduce_tag - seg_index, comm, &recv_req));
                if (MPI_SUCCESS != err) {
                    return err;
                }
                /* Set the recv callback */
                ompi_request_set_callback(recv_req, recv_cb, context);

                ++num_recvs;
            }
        }
    }

    /* Leaf nodes */
    else {
        /* Set up recv_list */
        min = mca_coll_adapt_component.adapt_ireduce_max_send_requests;
        if (num_segs <= mca_coll_adapt_component.adapt_ireduce_max_send_requests) {
            min = num_segs;
        }
        /* put all items into the recv_list that won't be sent immediately */
        for (int32_t seg_index = min; seg_index < num_segs; seg_index++) {
            ompi_coll_adapt_item_t *item;
            item = OBJ_NEW(ompi_coll_adapt_item_t);
            item->id = seg_index;
            item->count = tree->tree_nextsize;
            opal_list_append(&con->recv_list, (opal_list_item_t *) item);
        }
        con->accumbuf = accumbuf;
        con->ongoing_send = min;
        for (int32_t seg_index = 0; seg_index < min; seg_index++) {
            send_count = seg_count;
            if (seg_index == (num_segs - 1)) {
                send_count = count - (ptrdiff_t) seg_count *(ptrdiff_t) seg_index;
            }
            ompi_coll_adapt_reduce_context_t *context =
                (ompi_coll_adapt_reduce_context_t *)opal_free_list_wait(mca_coll_adapt_component.adapt_ireduce_context_free_list);
            context->buff = (char *) sbuf + (ptrdiff_t) seg_index * (ptrdiff_t) segment_increment;
            context->seg_index = seg_index;
            /* Actural rank of the peer */
            context->peer = tree->tree_prev;
            context->con = con;
            context->inbuf = NULL;

            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                  "[%d]: In ireduce, create isend to seg %d, peer %d, send_count %d tag %d\n",
                                  context->con->rank, context->seg_index, context->peer,
                                  send_count, con->ireduce_tag - context->seg_index));

            /* Create send request */
            ompi_request_t *send_req;
            err = MCA_PML_CALL(isend
                                (context->buff, send_count, dtype, tree->tree_prev,
                                con->ireduce_tag - context->seg_index,
                                sendmode, comm, &send_req));
            if (MPI_SUCCESS != err) {
                return err;
            }

            /* Set the send callback */
            ompi_request_set_callback(send_req, send_cb, context);
        }

    }

    return MPI_SUCCESS;
}
