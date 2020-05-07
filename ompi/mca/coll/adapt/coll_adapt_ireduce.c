/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_base_topo.h"

/* MPI_Reduce and MPI_Ireduce in the ADAPT module only work for commutative operations */

typedef int (*ompi_coll_adapt_ireduce_fn_t) (const void *sbuf,
                                            void *rbuf,
                                            int count,
                                            struct ompi_datatype_t * datatype,
                                            struct ompi_op_t * op,
                                            int root,
                                            struct ompi_communicator_t * comm,
                                            ompi_request_t ** request,
                                            mca_coll_base_module_t * module, int ireduce_tag);

static ompi_coll_adapt_algorithm_index_t ompi_coll_adapt_ireduce_algorithm_index[] = {
    {0, (uintptr_t)ompi_coll_adapt_ireduce_tuned},
    {1, (uintptr_t) ompi_coll_adapt_ireduce_binomial},
    {2, (uintptr_t) ompi_coll_adapt_ireduce_in_order_binomial},
    {3, (uintptr_t) ompi_coll_adapt_ireduce_binary},
    {4, (uintptr_t) ompi_coll_adapt_ireduce_pipeline},
    {5, (uintptr_t) ompi_coll_adapt_ireduce_chain},
    {6, (uintptr_t) ompi_coll_adapt_ireduce_linear},
};

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
                                    "Maximum number of receive requests",
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
                                    "Maximum number of segment in inbuf free list",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &mca_coll_adapt_component.adapt_inbuf_free_list_inc);

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
static ompi_coll_adapt_item_t *get_next_ready_item(opal_list_t * list, int num_children)
{
    ompi_coll_adapt_item_t *item;
    if (opal_list_is_empty(list)) {
        return NULL;
    }
    for (item = (ompi_coll_adapt_item_t *) opal_list_get_first(list);
         item != (ompi_coll_adapt_item_t *) opal_list_get_end(list);
         item = (ompi_coll_adapt_item_t *) ((opal_list_item_t *) item)->opal_list_next) {
        if (item->count == num_children) {
            opal_list_remove_item(list, (opal_list_item_t *) item);
            return item;
        }
    }
    return NULL;
}

static int add_to_list(opal_list_t * list, int id)
{
    ompi_coll_adapt_item_t *item;
    for (item = (ompi_coll_adapt_item_t *) opal_list_get_first(list);
         item != (ompi_coll_adapt_item_t *) opal_list_get_end(list);
         item = (ompi_coll_adapt_item_t *) ((opal_list_item_t *) item)->opal_list_next) {
        if (item->id == id) {
            (item->count)++;
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output, "add_to_list_return 1\n"));
            return 1;
        }
    }
    /* Add a new object to the list with count set to 1 */
    item = OBJ_NEW(ompi_coll_adapt_item_t);
    item->id = id;
    item->count = 1;
    opal_list_append(list, (opal_list_item_t *) item);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output, "add_to_list_return 1\n"));
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
    int i;
    ompi_request_t *temp_req = context->con->request;
    if (context->con->accumbuf != NULL) {
        if (context->con->rank != context->con->root) {
            for (i = 0; i < context->con->num_segs; i++) {
                OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                     "[%d]: Return accumbuf %d %p\n",
                                     ompi_comm_rank(context->con->comm), i,
                                     (void *) to_inbuf(context->con->accumbuf[i],
                                                       context->con->distance)));
                opal_free_list_return(context->con->inbuf_list,
                                      (opal_free_list_item_t *) to_inbuf(context->con->accumbuf[i],
                                                                         context->con->distance));
            }
        }
        free(context->con->accumbuf);
    }
    OBJ_RELEASE(context->con->recv_list);
    for (i = 0; i < context->con->num_segs; i++) {
        OBJ_RELEASE(context->con->mutex_op_list[i]);
    }
    free(context->con->mutex_op_list);
    OBJ_RELEASE(context->con->mutex_num_recv_segs);
    OBJ_RELEASE(context->con->mutex_recv_list);
    OBJ_RELEASE(context->con->mutex_num_sent);
    if (context->con->tree->tree_nextsize > 0) {
        OBJ_RELEASE(context->con->inbuf_list);
        free(context->con->next_recv_segs);
    }
    OBJ_RELEASE(context->con);
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
                         context->peer, context->frag_id));
    int err;

    opal_atomic_sub_fetch_32(&(context->con->ongoing_send), 1);

    /* Send a new segment */
    OPAL_THREAD_LOCK(context->con->mutex_recv_list);
    ompi_coll_adapt_item_t *item =
        get_next_ready_item(context->con->recv_list, context->con->tree->tree_nextsize);
    OPAL_THREAD_UNLOCK(context->con->mutex_recv_list);

    if (item != NULL) {
        /* Get new context item from free list */
        ompi_coll_adapt_reduce_context_t *send_context =
            (ompi_coll_adapt_reduce_context_t *) opal_free_list_wait(mca_coll_adapt_component.
                                                                    adapt_ireduce_context_free_list);
        if (context->con->tree->tree_nextsize > 0) {
            send_context->buff = context->con->accumbuf[item->id];
        } else {
            send_context->buff =
                context->buff + (item->id - context->frag_id) * context->con->segment_increment;
        }
        send_context->frag_id = item->id;
        send_context->peer = context->peer;
        send_context->con = context->con;
        OBJ_RETAIN(context->con);

        opal_atomic_add_fetch_32(&(context->con->ongoing_send), 1);

        int send_count = send_context->con->seg_count;
        if (item->id == (send_context->con->num_segs - 1)) {
            send_count = send_context->con->count - item->id * send_context->con->seg_count;
        }

        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                             "[%d]: In send_cb, create isend to seg %d, peer %d, tag %d\n",
                             send_context->con->rank, send_context->frag_id, send_context->peer,
                             (send_context->con->ireduce_tag << 16) + send_context->frag_id));

        ompi_request_t *send_req;
        err =
            MCA_PML_CALL(isend
                         (send_context->buff, send_count, send_context->con->datatype,
                          send_context->peer,
                          (context->con->ireduce_tag << 16) + send_context->frag_id,
                          MCA_PML_BASE_SEND_SYNCHRONOUS, send_context->con->comm, &send_req));
        if (MPI_SUCCESS != err) {
            return err;
        }

        /* Release the item */
        OBJ_RELEASE(item);

        /* Invoke send call back */
        ompi_request_set_callback(send_req, send_cb, send_context);
    }

    OPAL_THREAD_LOCK(context->con->mutex_num_sent);
    int32_t num_sent = ++(context->con->num_sent_segs);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: In send_cb, root = %d, num_sent = %d, num_segs = %d\n",
                         context->con->rank, context->con->tree->tree_root, num_sent,
                         context->con->num_segs));
    /* Check whether signal the condition, non root and sent all the segments */
    if (context->con->tree->tree_root != context->con->rank && num_sent == context->con->num_segs) {
        OPAL_THREAD_UNLOCK(context->con->mutex_num_sent);
        ireduce_request_fini(context);
    } else {
        OPAL_THREAD_UNLOCK(context->con->mutex_num_sent);
        OBJ_RELEASE(context->con);
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
    ompi_coll_adapt_reduce_context_t *context =
        (ompi_coll_adapt_reduce_context_t *) req->req_complete_cb_data;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: ireduce_recv_cb, peer %d, seg_id %d\n", context->con->rank,
                         context->peer, context->frag_id));

    int err;
    int32_t new_id =
        opal_atomic_add_fetch_32(&(context->con->next_recv_segs[context->child_id]), 1);

    /* Receive new segment */
    if (new_id < context->con->num_segs) {
        char *temp_recv_buf = NULL;
        ompi_coll_adapt_inbuf_t *inbuf = NULL;
        /* Set inbuf, if it it first child, recv on rbuf, else recv on inbuf */
        if (context->child_id == 0 && context->con->sbuf != MPI_IN_PLACE
            && context->con->root == context->con->rank) {
            temp_recv_buf =
                (char *) context->con->rbuf +
                (ptrdiff_t) new_id *(ptrdiff_t) context->con->segment_increment;
        } else {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: In recv_cb, alloc inbuf\n", context->con->rank));
            inbuf = (ompi_coll_adapt_inbuf_t *) opal_free_list_wait(context->con->inbuf_list);
            temp_recv_buf = inbuf->buff - context->con->lower_bound;
        }
        /* Get new context item from free list */
        ompi_coll_adapt_reduce_context_t *recv_context =
            (ompi_coll_adapt_reduce_context_t *) opal_free_list_wait(mca_coll_adapt_component.
                                                                    adapt_ireduce_context_free_list);
        recv_context->buff = temp_recv_buf;
        recv_context->frag_id = new_id;
        recv_context->child_id = context->child_id;
        recv_context->peer = context->peer;
        recv_context->con = context->con;
        OBJ_RETAIN(context->con);
        recv_context->inbuf = inbuf;
        int recv_count = recv_context->con->seg_count;
        if (new_id == (recv_context->con->num_segs - 1)) {
            recv_count = recv_context->con->count - new_id * recv_context->con->seg_count;
        }
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                             "[%d]: In recv_cb, create irecv for seg %d, peer %d, inbuf %p, tag %d\n",
                             context->con->rank, recv_context->frag_id, recv_context->peer,
                             (void *) inbuf,
                             (recv_context->con->ireduce_tag << 16) + recv_context->frag_id));
        ompi_request_t *recv_req;
        err =
            MCA_PML_CALL(irecv
                         (temp_recv_buf, recv_count, recv_context->con->datatype,
                          recv_context->peer,
                          (recv_context->con->ireduce_tag << 16) + recv_context->frag_id,
                          recv_context->con->comm, &recv_req));
        if (MPI_SUCCESS != err) {
            return err;
        }
        /* Invoke receive call back */
        ompi_request_set_callback(recv_req, recv_cb, recv_context);
    }

    /* Do the op */
    int op_count = context->con->seg_count;
    if (context->frag_id == (context->con->num_segs - 1)) {
        op_count = context->con->count - context->frag_id * context->con->seg_count;
    }

    int keep_inbuf = 0;
    OPAL_THREAD_LOCK(context->con->mutex_op_list[context->frag_id]);
    if (context->con->accumbuf[context->frag_id] == NULL) {
        if (context->inbuf == NULL) {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: set accumbuf to rbuf\n", context->con->rank));
            context->con->accumbuf[context->frag_id] = context->buff;
        } else {
            keep_inbuf = 1;
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: set accumbuf to inbuf\n", context->con->rank));
            context->con->accumbuf[context->frag_id] =
                context->inbuf->buff - context->con->lower_bound;
        }
        /* Op sbuf and accmbuf to accumbuf */
        ompi_op_reduce(context->con->op,
                       context->con->sbuf +
                       (ptrdiff_t) context->frag_id * (ptrdiff_t) context->con->segment_increment,
                       context->con->accumbuf[context->frag_id], op_count, context->con->datatype);

    } else {
        if (context->inbuf == NULL) {
            /* Op rbuf and accumbuf to rbuf */
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: op rbuf and accumbuf to rbuf\n", context->con->rank));
            ompi_op_reduce(context->con->op, context->con->accumbuf[context->frag_id],
                           context->buff, op_count, context->con->datatype);
            /* Free old accumbuf */
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: free old accumbuf %p\n", context->con->rank,
                                 (void *) to_inbuf(context->con->accumbuf[context->frag_id],
                                                   context->con->distance)));
            opal_free_list_return(context->con->inbuf_list,
                                  (opal_free_list_item_t *) to_inbuf(context->con->
                                                                     accumbuf[context->frag_id],
                                                                     context->con->distance));
            /* Set accumbut to rbuf */
            context->con->accumbuf[context->frag_id] = context->buff;
        } else {
            /* Op inbuf and accmbuf to accumbuf */
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: op inbuf and accmbuf to accumbuf\n", context->con->rank));
            ompi_op_reduce(context->con->op, context->inbuf->buff - context->con->lower_bound,
                           context->con->accumbuf[context->frag_id], op_count,
                           context->con->datatype);
        }
    }

    OPAL_THREAD_UNLOCK(context->con->mutex_op_list[context->frag_id]);

    /* Set recv list */
    if (context->con->rank != context->con->tree->tree_root) {
        OPAL_THREAD_LOCK(context->con->mutex_recv_list);
        add_to_list(context->con->recv_list, context->frag_id);
        OPAL_THREAD_UNLOCK(context->con->mutex_recv_list);
    }

    /* Send to parent */
    if (context->con->rank != context->con->tree->tree_root
        && context->con->ongoing_send < mca_coll_adapt_component.adapt_ireduce_max_send_requests) {
        OPAL_THREAD_LOCK(context->con->mutex_recv_list);
        ompi_coll_adapt_item_t *item =
            get_next_ready_item(context->con->recv_list, context->con->tree->tree_nextsize);
        OPAL_THREAD_UNLOCK(context->con->mutex_recv_list);

        if (item != NULL) {
            /* Get new context item from free list */
            ompi_coll_adapt_reduce_context_t *send_context =
                (ompi_coll_adapt_reduce_context_t *) opal_free_list_wait(mca_coll_adapt_component.
                                                                        adapt_ireduce_context_free_list);
            send_context->buff = context->con->accumbuf[context->frag_id];
            send_context->frag_id = item->id;
            send_context->peer = context->con->tree->tree_prev;
            send_context->con = context->con;
            OBJ_RETAIN(context->con);
            opal_atomic_add_fetch_32(&(context->con->ongoing_send), 1);

            int send_count = send_context->con->seg_count;
            if (item->id == (send_context->con->num_segs - 1)) {
                send_count = send_context->con->count - item->id * send_context->con->seg_count;
            }
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: In recv_cb, create isend to seg %d, peer %d, tag %d\n",
                                 send_context->con->rank, send_context->frag_id, send_context->peer,
                                 (send_context->con->ireduce_tag << 16) + send_context->frag_id));

            ompi_request_t *send_req;
            err =
                MCA_PML_CALL(isend
                             (send_context->buff, send_count, send_context->con->datatype,
                              send_context->peer,
                              (send_context->con->ireduce_tag << 16) + send_context->frag_id,
                              MCA_PML_BASE_SEND_SYNCHRONOUS, send_context->con->comm, &send_req));
            if (MPI_SUCCESS != err) {
                return err;
            }
            OBJ_RELEASE(item);

            /* Invoke send call back */
            ompi_request_set_callback(send_req, send_cb, send_context);
        }
    }

    OPAL_THREAD_LOCK(context->con->mutex_num_recv_segs);
    int num_recv_segs_t = ++(context->con->num_recv_segs);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: In recv_cb, tree = %p, root = %d, num_recv = %d, num_segs = %d, num_child = %d\n",
                         context->con->rank, (void *) context->con->tree,
                         context->con->tree->tree_root, num_recv_segs_t, context->con->num_segs,
                         context->con->tree->tree_nextsize));
    /* If this is root and has received all the segments */
    if (context->con->tree->tree_root == context->con->rank
        && num_recv_segs_t == context->con->num_segs * context->con->tree->tree_nextsize) {
        OPAL_THREAD_UNLOCK(context->con->mutex_num_recv_segs);
        if (!keep_inbuf && context->inbuf != NULL) {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: root free context inbuf %p", context->con->rank,
                                 (void *) context->inbuf));
            opal_free_list_return(context->con->inbuf_list,
                                  (opal_free_list_item_t *) context->inbuf);
        }
        ireduce_request_fini(context);
    } else {
        OPAL_THREAD_UNLOCK(context->con->mutex_num_recv_segs);
        if (!keep_inbuf && context->inbuf != NULL) {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: free context inbuf %p", context->con->rank,
                                 (void *) context->inbuf));
            opal_free_list_return(context->con->inbuf_list,
                                  (opal_free_list_item_t *) context->inbuf);
        }
        OBJ_RELEASE(context->con);
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
    if (count == 0) {
        return MPI_SUCCESS;
    }
    int ireduce_tag = opal_atomic_add_fetch_32(&(comm->c_ireduce_tag), 1);
    ireduce_tag = (ireduce_tag % 4096) + 4096;

    OPAL_OUTPUT_VERBOSE((10, mca_coll_adapt_component.adapt_output,
                         "ireduce tag %d root %d, algorithm %d, coll_adapt_ireduce_segment_size %zu, coll_adapt_ireduce_max_send_requests %d, coll_adapt_ireduce_max_recv_requests %d\n",
                         ireduce_tag, root, mca_coll_adapt_component.adapt_ireduce_algorithm,
                         mca_coll_adapt_component.adapt_ireduce_segment_size,
                         mca_coll_adapt_component.adapt_ireduce_max_send_requests,
                         mca_coll_adapt_component.adapt_ireduce_max_recv_requests));

    ompi_coll_adapt_ireduce_fn_t reduce_func =
        (ompi_coll_adapt_ireduce_fn_t)
        ompi_coll_adapt_ireduce_algorithm_index[mca_coll_adapt_component.
                                               adapt_ireduce_algorithm].algorithm_fn_ptr;
    return reduce_func(sbuf, rbuf, count, dtype, op, root, comm, request, module, ireduce_tag);
}

/*
 * Ireduce functions with different algorithms
 */
int ompi_coll_adapt_ireduce_tuned(const void *sbuf, void *rbuf, int count,
                                 struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                 int root, struct ompi_communicator_t *comm,
                                 ompi_request_t ** request,
                                 mca_coll_base_module_t *module, int ireduce_tag)
{
    OPAL_OUTPUT_VERBOSE((10, mca_coll_adapt_component.adapt_output, "tuned not implemented\n"));
    return OMPI_ERR_NOT_IMPLEMENTED;
}

int ompi_coll_adapt_ireduce_binomial(const void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                    struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    mca_coll_base_module_t * module, int ireduce_tag)
{
    ompi_coll_tree_t *tree = ompi_coll_base_topo_build_bmtree(comm, root);
    int err =
        ompi_coll_adapt_ireduce_generic(sbuf, rbuf, count, dtype, op, root, comm, request, module,
                                       tree, mca_coll_adapt_component.adapt_ireduce_segment_size,
                                       ireduce_tag);
    return err;
}

int ompi_coll_adapt_ireduce_in_order_binomial(const void *sbuf, void *rbuf, int count,
                                             struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                             int root, struct ompi_communicator_t *comm,
                                             ompi_request_t ** request,
                                             mca_coll_base_module_t * module, int ireduce_tag)
{
    ompi_coll_tree_t *tree = ompi_coll_base_topo_build_in_order_bmtree(comm, root);
    int err =
        ompi_coll_adapt_ireduce_generic(sbuf, rbuf, count, dtype, op, root, comm, request, module,
                                       tree, mca_coll_adapt_component.adapt_ireduce_segment_size,
                                       ireduce_tag);
    return err;
}

int ompi_coll_adapt_ireduce_binary(const void *sbuf, void *rbuf, int count,
                                  struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                  struct ompi_communicator_t *comm, ompi_request_t ** request,
                                  mca_coll_base_module_t * module, int ireduce_tag)
{
    ompi_coll_tree_t *tree = ompi_coll_base_topo_build_tree(2, comm, root);
    int err =
        ompi_coll_adapt_ireduce_generic(sbuf, rbuf, count, dtype, op, root, comm, request, module,
                                       tree, mca_coll_adapt_component.adapt_ireduce_segment_size,
                                       ireduce_tag);
    return err;
}

int ompi_coll_adapt_ireduce_pipeline(const void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                    struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    mca_coll_base_module_t * module, int ireduce_tag)
{
    ompi_coll_tree_t *tree = ompi_coll_base_topo_build_chain(1, comm, root);
    int err =
        ompi_coll_adapt_ireduce_generic(sbuf, rbuf, count, dtype, op, root, comm, request, module,
                                       tree, mca_coll_adapt_component.adapt_ireduce_segment_size,
                                       ireduce_tag);
    return err;
}


int ompi_coll_adapt_ireduce_chain(const void *sbuf, void *rbuf, int count,
                                 struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                 struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 mca_coll_base_module_t * module, int ireduce_tag)
{
    ompi_coll_tree_t *tree = ompi_coll_base_topo_build_chain(4, comm, root);
    int err =
        ompi_coll_adapt_ireduce_generic(sbuf, rbuf, count, dtype, op, root, comm, request, module,
                                       tree, mca_coll_adapt_component.adapt_ireduce_segment_size,
                                       ireduce_tag);
    return err;
}

int ompi_coll_adapt_ireduce_linear(const void *sbuf, void *rbuf, int count,
                                  struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                  struct ompi_communicator_t *comm, ompi_request_t ** request,
                                  mca_coll_base_module_t * module, int ireduce_tag)
{
    int fanout = ompi_comm_size(comm) - 1;
    ompi_coll_tree_t *tree;
    if (fanout < 1) {
        tree = ompi_coll_base_topo_build_chain(1, comm, root);
    } else if (fanout <= MAXTREEFANOUT) {
        tree = ompi_coll_base_topo_build_tree(ompi_comm_size(comm) - 1, comm, root);
    } else {
        tree = ompi_coll_base_topo_build_tree(MAXTREEFANOUT, comm, root);
    }
    int err =
        ompi_coll_adapt_ireduce_generic(sbuf, rbuf, count, dtype, op, root, comm, request, module,
                                       tree, mca_coll_adapt_component.adapt_ireduce_segment_size,
                                       ireduce_tag);
    return err;
}


int ompi_coll_adapt_ireduce_generic(const void *sbuf, void *rbuf, int count,
                                   struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                   struct ompi_communicator_t *comm, ompi_request_t ** request,
                                   mca_coll_base_module_t * module, ompi_coll_tree_t * tree,
                                   size_t seg_size, int ireduce_tag)
{

    ptrdiff_t extent, lower_bound, segment_increment;
    ptrdiff_t true_lower_bound, true_extent, real_seg_size;
    size_t typelng;
    int seg_count = count, num_segs, rank, recv_count, send_count, i, j, err, min, distance = 0;
    int32_t seg_index;
    opal_atomic_int_t *next_recv_segs = NULL;
    /* Used to store the accumuate result, pointer to every segment */
    char **accumbuf = NULL;
    /* A free list contains all recv data */
    opal_free_list_t *inbuf_list;
    opal_mutex_t *mutex_recv_list;
    opal_mutex_t *mutex_num_recv_segs;
    opal_mutex_t *mutex_num_sent;
    opal_mutex_t **mutex_op_list;
    /* A list to store the segments need to be sent */
    opal_list_t *recv_list;

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
        if( !OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR((opal_atomic_intptr_t *)&mca_coll_adapt_component.adapt_ireduce_context_free_list,
                                                     &(intptr_t){0}, fl) ) {
            OBJ_RELEASE(fl);
        }
    }

    /* If the current process is not leaf */
    if (tree->tree_nextsize > 0) {
        inbuf_list = OBJ_NEW(opal_free_list_t);
        opal_free_list_init(inbuf_list,
                            sizeof(ompi_coll_adapt_inbuf_t) + real_seg_size,
                            opal_cache_line_size,
                            OBJ_CLASS(ompi_coll_adapt_inbuf_t),
                            0, opal_cache_line_size,
                            mca_coll_adapt_component.adapt_inbuf_free_list_min,
                            mca_coll_adapt_component.adapt_inbuf_free_list_max,
                            mca_coll_adapt_component.adapt_inbuf_free_list_inc,
                            NULL, 0, NULL, NULL, NULL);
        /* Set up next_recv_segs */
        next_recv_segs = (opal_atomic_int32_t *) malloc(sizeof(int32_t) * tree->tree_nextsize);
        ompi_coll_adapt_inbuf_t *temp_inbuf =
            (ompi_coll_adapt_inbuf_t *) opal_free_list_wait(inbuf_list);
        distance = (char *) temp_inbuf->buff - lower_bound - (char *) temp_inbuf;       //address of inbuf->buff to address of inbuf
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                             "[%d]: distance %d, inbuf %p, inbuf->buff %p, inbuf->buff-lb %p, to_inbuf %p, inbuf_list %p\n",
                             rank, distance, (void *) temp_inbuf, (void *) temp_inbuf->buff,
                             (char *) temp_inbuf->buff - lower_bound,
                             (void *) to_inbuf((char *) temp_inbuf->buff - lower_bound, distance),
                             (void *) inbuf_list));
        opal_free_list_return(inbuf_list, (opal_free_list_item_t *) temp_inbuf);
    } else {
        inbuf_list = NULL;
        next_recv_segs = NULL;
    }

    ompi_request_t *temp_request = NULL;
    /* Set up request */
    temp_request = OBJ_NEW(ompi_request_t);
    OMPI_REQUEST_INIT(temp_request, false);
    temp_request->req_state = OMPI_REQUEST_ACTIVE;
    temp_request->req_type = 0;
    temp_request->req_free = ompi_coll_adapt_request_free;
    temp_request->req_status.MPI_SOURCE = 0;
    temp_request->req_status.MPI_TAG = 0;
    temp_request->req_status.MPI_ERROR = 0;
    temp_request->req_status._cancelled = 0;
    temp_request->req_status._ucount = 0;
    *request = temp_request;

    /* Set up mutex */
    mutex_recv_list = OBJ_NEW(opal_mutex_t);
    mutex_num_recv_segs = OBJ_NEW(opal_mutex_t);
    mutex_op_list = (opal_mutex_t **) malloc(sizeof(opal_mutex_t *) * num_segs);
    for (i = 0; i < num_segs; i++) {
        mutex_op_list[i] = OBJ_NEW(opal_mutex_t);
    }
    mutex_num_sent = OBJ_NEW(opal_mutex_t);
    /* Create recv_list */
    recv_list = OBJ_NEW(opal_list_t);

    /* Set constant context for send and recv call back */
    ompi_coll_adapt_constant_reduce_context_t *con =
        OBJ_NEW(ompi_coll_adapt_constant_reduce_context_t);
    con->count = count;
    con->seg_count = seg_count;
    con->datatype = dtype;
    con->comm = comm;
    con->segment_increment = segment_increment;
    con->num_segs = num_segs;
    con->request = temp_request;
    con->rank = rank;
    con->num_recv_segs = 0;
    con->num_sent_segs = 0;
    con->next_recv_segs = next_recv_segs;
    con->mutex_recv_list = mutex_recv_list;
    con->mutex_num_recv_segs = mutex_num_recv_segs;
    con->mutex_num_sent = mutex_num_sent;
    con->mutex_op_list = mutex_op_list;
    con->op = op;
    con->tree = tree;
    con->inbuf_list = inbuf_list;
    con->recv_list = recv_list;
    con->lower_bound = lower_bound;
    con->ongoing_send = 0;
    con->sbuf = (char *) sbuf;
    con->rbuf = (char *) rbuf;
    con->root = root;
    con->distance = distance;
    con->ireduce_tag = ireduce_tag;
    con->real_seg_size = real_seg_size;

    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: start ireduce root %d tag %d\n", rank, tree->tree_root,
                         ireduce_tag));

    /* If the current process is not leaf node */
    if (tree->tree_nextsize > 0) {
        /* Set up accumbuf */
        accumbuf = (char **) malloc(sizeof(char *) * num_segs);
        if (root == rank && sbuf == MPI_IN_PLACE) {
            for (i = 0; i < num_segs; i++) {
                accumbuf[i] = (char *) rbuf + (ptrdiff_t) i *(ptrdiff_t) segment_increment;
            }
        } else {
            for (i = 0; i < num_segs; i++) {
                accumbuf[i] = NULL;
            }
        }

        con->accumbuf = accumbuf;

        /* For the first batch of segments */
        if (num_segs <= mca_coll_adapt_component.adapt_ireduce_max_recv_requests) {
            min = num_segs;
        } else {
            min = mca_coll_adapt_component.adapt_ireduce_max_recv_requests;
        }
        for (i = 0; i < tree->tree_nextsize; i++) {
            next_recv_segs[i] = min - 1;
        }

        for (j = 0; j < min; j++) {
            /* For each child */
            for (i = 0; i < tree->tree_nextsize; i++) {
                seg_index = j;
                if (seg_index < num_segs) {
                    recv_count = seg_count;
                    if (seg_index == (num_segs - 1)) {
                        recv_count = count - (ptrdiff_t) seg_count *(ptrdiff_t) seg_index;
                    }
                    char *temp_recv_buf = NULL;
                    ompi_coll_adapt_inbuf_t *inbuf = NULL;
                    /* Set inbuf, if it it first child, recv on rbuf, else recv on inbuf */
                    if (i == 0 && sbuf != MPI_IN_PLACE && root == rank) {
                        temp_recv_buf =
                            (char *) rbuf + (ptrdiff_t) j *(ptrdiff_t) segment_increment;
                    } else {
                        inbuf = (ompi_coll_adapt_inbuf_t *) opal_free_list_wait(inbuf_list);
                        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                             "[%d]: In ireduce, alloc inbuf %p\n", rank,
                                             (void *) inbuf));
                        temp_recv_buf = inbuf->buff - lower_bound;
                    }
                    /* Get context */
                    ompi_coll_adapt_reduce_context_t *context =
                        (ompi_coll_adapt_reduce_context_t *)
                        opal_free_list_wait(mca_coll_adapt_component.
                                            adapt_ireduce_context_free_list);
                    context->buff = temp_recv_buf;
                    context->frag_id = seg_index;
                    context->child_id = i;      //the id of peer in in the tree
                    context->peer = tree->tree_next[i]; //the actural rank of the peer
                    context->con = con;
                    OBJ_RETAIN(con);
                    context->inbuf = inbuf;

                    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                         "[%d]: In ireduce, create irecv for seg %d, peer %d, recv_count %d, inbuf %p tag %d\n",
                                         context->con->rank, context->frag_id, context->peer,
                                         recv_count, (void *) inbuf,
                                         (ireduce_tag << 16) + seg_index));

                    /* Create a recv request */
                    ompi_request_t *recv_req;
                    err =
                        MCA_PML_CALL(irecv
                                     (temp_recv_buf, recv_count, dtype, tree->tree_next[i],
                                      (ireduce_tag << 16) + seg_index, comm, &recv_req));
                    if (MPI_SUCCESS != err) {
                        return err;
                    }
                    /* Invoke recv call back */
                    ompi_request_set_callback(recv_req, recv_cb, context);
                }
            }
        }
    }

    /* Leaf nodes */
    else {
        ompi_coll_adapt_item_t *item;
        /* Set up recv_list */
        for (seg_index = 0; seg_index < num_segs; seg_index++) {
            item = OBJ_NEW(ompi_coll_adapt_item_t);
            item->id = seg_index;
            item->count = tree->tree_nextsize;
            opal_list_append(recv_list, (opal_list_item_t *) item);
        }
        if (num_segs <= mca_coll_adapt_component.adapt_ireduce_max_send_requests) {
            min = num_segs;
        } else {
            min = mca_coll_adapt_component.adapt_ireduce_max_send_requests;
        }
        con->accumbuf = accumbuf;
        for (i = 0; i < min; i++) {
            OPAL_THREAD_LOCK(mutex_recv_list);
            item = get_next_ready_item(recv_list, tree->tree_nextsize);
            OPAL_THREAD_UNLOCK(mutex_recv_list);
            if (item != NULL) {
                send_count = seg_count;
                if (item->id == (num_segs - 1)) {
                    send_count = count - (ptrdiff_t) seg_count *(ptrdiff_t) item->id;
                }
                ompi_coll_adapt_reduce_context_t *context =
                    (ompi_coll_adapt_reduce_context_t *)
                    opal_free_list_wait(mca_coll_adapt_component.adapt_ireduce_context_free_list);
                context->buff =
                    (char *) sbuf + (ptrdiff_t) item->id * (ptrdiff_t) segment_increment;
                context->frag_id = item->id;
                /* Actural rank of the peer */
                context->peer = tree->tree_prev;
                context->con = con;
                OBJ_RETAIN(con);
                context->inbuf = NULL;

                opal_atomic_add_fetch_32(&(context->con->ongoing_send), 1);
                OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                     "[%d]: In ireduce, create isend to seg %d, peer %d, send_count %d tag %d\n",
                                     context->con->rank, context->frag_id, context->peer,
                                     send_count, (ireduce_tag << 16) + context->frag_id));

                /* Create send request */
                ompi_request_t *send_req;
                err =
                    MCA_PML_CALL(isend
                                 (context->buff, send_count, dtype, tree->tree_prev,
                                  (ireduce_tag << 16) + context->frag_id,
                                  MCA_PML_BASE_SEND_SYNCHRONOUS, comm, &send_req));
                if (MPI_SUCCESS != err) {
                    return err;
                }
                OBJ_RELEASE(item);

                /* Invoke send call back */
                ompi_request_set_callback(send_req, send_cb, context);
            }
        }

    }

    return MPI_SUCCESS;
}
