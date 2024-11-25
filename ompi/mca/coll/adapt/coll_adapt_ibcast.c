/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * Copyright (c) 2024      NVIDIA CORPORATION. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/pml/pml.h"
#include "coll_adapt.h"
#include "coll_adapt_algorithms.h"
#include "coll_adapt_context.h"
#include "coll_adapt_topocache.h"
#include "ompi/mca/coll/base/coll_base_util.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "opal/util/bit_ops.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/pml/ob1/pml_ob1.h"

static int ompi_coll_adapt_ibcast_generic(IBCAST_ARGS,
                                   ompi_coll_tree_t * tree, size_t seg_size);

/*
 * Set up MCA parameters of MPI_Bcast and MPI_IBcast
 */
int ompi_coll_adapt_ibcast_register(void)
{
    mca_base_component_t *c = &mca_coll_adapt_component.super.collm_version;

    mca_coll_adapt_component.adapt_ibcast_algorithm = 1;
    mca_base_component_var_register(c, "bcast_algorithm",
                                    "Algorithm of broadcast, 0: tuned, 1: binomial, 2: in_order_binomial, 3: binary, 4: pipeline, 5: chain, 6: linear",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_ALL,
                                    &mca_coll_adapt_component.adapt_ibcast_algorithm);
    if( (mca_coll_adapt_component.adapt_ibcast_algorithm < 0) ||
        (mca_coll_adapt_component.adapt_ibcast_algorithm >= OMPI_COLL_ADAPT_ALGORITHM_COUNT) ) {
        mca_coll_adapt_component.adapt_ibcast_algorithm = 1;
    }

    mca_coll_adapt_component.adapt_ibcast_segment_size = 0;
    mca_base_component_var_register(c, "bcast_segment_size",
                                    "Segment size in bytes used by default for bcast algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation.",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_ALL,
                                    &mca_coll_adapt_component.adapt_ibcast_segment_size);

    mca_coll_adapt_component.adapt_ibcast_max_send_requests = 2;
    mca_base_component_var_register(c, "bcast_max_send_requests",
                                    "Maximum number of send requests",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_ALL,
                                    &mca_coll_adapt_component.adapt_ibcast_max_send_requests);

    mca_coll_adapt_component.adapt_ibcast_max_recv_requests = 3;
    mca_base_component_var_register(c, "bcast_max_recv_requests",
                                    "Maximum number of receive requests",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_ALL,
                                    &mca_coll_adapt_component.adapt_ibcast_max_recv_requests);

    mca_coll_adapt_component.adapt_ibcast_synchronous_send = true;
    (void) mca_base_component_var_register(c, "bcast_synchronous_send",
                                           "Whether to use synchronous send operations during setup of bcast operations",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_ALL,
                                           &mca_coll_adapt_component.adapt_ibcast_synchronous_send);

    mca_coll_adapt_component.adapt_ibcast_context_free_list = NULL;
    return OMPI_SUCCESS;
}

/*
 * Release the free list created in ompi_coll_adapt_ibcast_generic
 */
int ompi_coll_adapt_ibcast_fini(void)
{
    if (NULL != mca_coll_adapt_component.adapt_ibcast_context_free_list) {
        OBJ_RELEASE(mca_coll_adapt_component.adapt_ibcast_context_free_list);
        mca_coll_adapt_component.adapt_ibcast_context_free_list = NULL;
        OPAL_OUTPUT_VERBOSE((10, mca_coll_adapt_component.adapt_output, "ibcast fini\n"));
    }
    return OMPI_SUCCESS;
}

/*
 *  Finish a ibcast request
 */
static int ibcast_request_fini(ompi_coll_adapt_bcast_context_t * context)
{
    ompi_request_t *temp_req = context->con->request;
    if (context->con->tree->tree_nextsize != 0) {
        free(context->con->send_array);
    }
    if (context->con->num_segs != 0) {
        free(context->con->recv_array);
    }
    OBJ_RELEASE(context->con->mutex);
    OBJ_RELEASE(context->con);
    ompi_request_complete(temp_req, 1);

    return OMPI_SUCCESS;
}

/*
 * Callback function of isend
 */
static int send_cb(ompi_request_t * req)
{
    ompi_coll_adapt_bcast_context_t *context =
        (ompi_coll_adapt_bcast_context_t *) req->req_complete_cb_data;
    int err;

    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: Send(cb): segment %d to %d at buff %p root %d\n",
                         ompi_comm_rank(context->con->comm), context->frag_id,
                         context->peer, (void *) context->buff, context->con->root));

    OPAL_THREAD_LOCK(context->con->mutex);
    int sent_id = context->con->send_array[context->child_id];
    /* If the current process has fragments in recv_array can be sent */
    if (sent_id < context->con->num_recv_segs) {
        ompi_coll_adapt_bcast_context_t *send_context;
        int new_id = context->con->recv_array[sent_id];
        ompi_request_t *send_req;
        ++(context->con->send_array[context->child_id]);
        OPAL_THREAD_UNLOCK(context->con->mutex);

        send_context = (ompi_coll_adapt_bcast_context_t *) opal_free_list_wait(mca_coll_adapt_component.adapt_ibcast_context_free_list);
        send_context->buff =
            context->buff + (new_id - context->frag_id) * context->con->real_seg_size;
        send_context->frag_id = new_id;
        send_context->child_id = context->child_id;
        send_context->peer = context->peer;
        send_context->con = context->con;
        int send_count = send_context->con->seg_count;
        if (new_id == (send_context->con->num_segs - 1)) {
            send_count = send_context->con->count - new_id * send_context->con->seg_count;
        }
        char *send_buff = send_context->buff;
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                             "[%d]: Send(start in send cb): segment %d to %d at buff %p send_count %d tag %d\n",
                             ompi_comm_rank(send_context->con->comm), send_context->frag_id,
                             send_context->peer, (void *) send_context->buff, send_count,
                             send_context->con->ibcast_tag - new_id));
        err = MCA_PML_CALL(isend
                           (send_buff, send_count, send_context->con->datatype, send_context->peer,
                            send_context->con->ibcast_tag - new_id,
                            MCA_PML_BASE_SEND_STANDARD, send_context->con->comm, &send_req));
        if (MPI_SUCCESS != err) {
            opal_free_list_return(mca_coll_adapt_component.adapt_ibcast_context_free_list,
                                  (opal_free_list_item_t *)send_context);
            OBJ_RELEASE(context->con);
            return err;
        }
        /* Set send callback */
        ompi_request_set_callback(send_req, send_cb, send_context);
        OPAL_THREAD_LOCK(context->con->mutex);
    } else {
        /* No future send here, we can release the ref */
        OBJ_RELEASE(context->con);
    }

    int num_sent = ++(context->con->num_sent_segs);
    int num_recv_fini = context->con->num_recv_fini;
    int rank = ompi_comm_rank(context->con->comm);
    OPAL_THREAD_UNLOCK(context->con->mutex);
    /* Check whether signal the condition */
    if ((rank == context->con->root
         && num_sent == context->con->tree->tree_nextsize * context->con->num_segs)
        || (context->con->tree->tree_nextsize > 0 && rank != context->con->root
            && num_sent == context->con->tree->tree_nextsize * context->con->num_segs
            && num_recv_fini == context->con->num_segs)) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output, "[%d]: Signal in send\n",
                             ompi_comm_rank(context->con->comm)));
        ibcast_request_fini(context);
    }
    opal_free_list_return(mca_coll_adapt_component.adapt_ibcast_context_free_list,
                          (opal_free_list_item_t *) context);
    req->req_free(&req);
    /* Call back function return 1 to signal that request has been free'd */
    return 1;
}

/*
 * Callback function of irecv
 */
static int recv_cb(ompi_request_t * req)
{
    /* Get necessary info from request */
    ompi_coll_adapt_bcast_context_t *context =
        (ompi_coll_adapt_bcast_context_t *) req->req_complete_cb_data;

    int err, i;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: Recv(cb): segment %d from %d at buff %p root %d\n",
                         ompi_comm_rank(context->con->comm), context->frag_id,
                         context->peer, (void *) context->buff, context->con->root));

    /* Store the frag_id to seg array */
    OPAL_THREAD_LOCK(context->con->mutex);
    int num_recv_segs = ++(context->con->num_recv_segs);
    context->con->recv_array[num_recv_segs - 1] = context->frag_id;
    OPAL_THREAD_UNLOCK(context->con->mutex);

    int new_id = num_recv_segs + mca_coll_adapt_component.adapt_ibcast_max_recv_requests - 1;
    /* Receive new segment */
    if (new_id < context->con->num_segs) {
        ompi_request_t *recv_req;
        ompi_coll_adapt_bcast_context_t *recv_context;
        /* Get new context item from free list */
        recv_context = (ompi_coll_adapt_bcast_context_t *) opal_free_list_wait(mca_coll_adapt_component.adapt_ibcast_context_free_list);
        recv_context->buff =
            context->buff + (new_id - context->frag_id) * context->con->real_seg_size;
        recv_context->frag_id = new_id;
        recv_context->child_id = context->child_id;
        recv_context->peer = context->peer;
        recv_context->con = context->con;
        OBJ_RETAIN(context->con);
        int recv_count = recv_context->con->seg_count;
        if (new_id == (recv_context->con->num_segs - 1)) {
            recv_count = recv_context->con->count - new_id * recv_context->con->seg_count;
        }
        char *recv_buff = recv_context->buff;
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                             "[%d]: Recv(start in recv cb): segment %d from %d at buff %p recv_count %d tag %d\n",
                             ompi_comm_rank(context->con->comm), context->frag_id, context->peer,
                             (void *) recv_buff, recv_count,
                             recv_context->con->ibcast_tag - recv_context->frag_id));
        MCA_PML_CALL(irecv
                     (recv_buff, recv_count, recv_context->con->datatype, recv_context->peer,
                      recv_context->con->ibcast_tag - recv_context->frag_id,
                      recv_context->con->comm, &recv_req));

        /* Set the receive callback */
        ompi_request_set_callback(recv_req, recv_cb, recv_context);
    }

    OPAL_THREAD_LOCK(context->con->mutex);
    /* Propagate segment to all children */
    for (i = 0; i < context->con->tree->tree_nextsize; i++) {
        /* If the current process can send the segment now, which means the only segment need to be sent is the just arrived one */
        if (num_recv_segs - 1 == context->con->send_array[i]) {
            ompi_request_t *send_req;

            ++(context->con->send_array[i]);

            /* release mutex to avoid deadlock in case a callback is triggered below */
            OPAL_THREAD_UNLOCK(context->con->mutex);

            int send_count = context->con->seg_count;
            if (context->frag_id == (context->con->num_segs - 1)) {
                send_count = context->con->count - context->frag_id * context->con->seg_count;
            }

            ompi_coll_adapt_bcast_context_t *send_context;
            send_context = (ompi_coll_adapt_bcast_context_t *) opal_free_list_wait(mca_coll_adapt_component.adapt_ibcast_context_free_list);
            send_context->buff = context->buff;
            send_context->frag_id = context->frag_id;
            send_context->child_id = i;
            send_context->peer = context->con->tree->tree_next[i];
            send_context->con = context->con;
            OBJ_RETAIN(context->con);
            char *send_buff = send_context->buff;
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: Send(start in recv cb): segment %d to %d at buff %p send_count %d tag %d\n",
                                 ompi_comm_rank(send_context->con->comm), send_context->frag_id,
                                 send_context->peer, (void *) send_context->buff, send_count,
                                 send_context->con->ibcast_tag - send_context->frag_id));
            err =
                MCA_PML_CALL(isend
                             (send_buff, send_count, send_context->con->datatype,
                              send_context->peer,
                              send_context->con->ibcast_tag - send_context->frag_id,
                              MCA_PML_BASE_SEND_STANDARD, send_context->con->comm, &send_req));
            if (MPI_SUCCESS != err) {
                opal_free_list_return(mca_coll_adapt_component.adapt_ibcast_context_free_list,
                                      (opal_free_list_item_t *)send_context);
                OBJ_RELEASE(context->con);
                return err;
            }
            /* Set send callback */
            ompi_request_set_callback(send_req, send_cb, send_context);

            /* retake the mutex for next iteration */
            OPAL_THREAD_LOCK(context->con->mutex);
        }
    }

    OBJ_RELEASE(context->con);

    int num_sent = context->con->num_sent_segs;
    int num_recv_fini = ++(context->con->num_recv_fini);

    OPAL_THREAD_UNLOCK(context->con->mutex);
    /* If this process is leaf and has received all the segments */
    if ((context->con->tree->tree_nextsize > 0
            && num_sent == context->con->tree->tree_nextsize * context->con->num_segs
            && num_recv_fini == context->con->num_segs)
        || (context->con->tree->tree_nextsize == 0
            && num_recv_fini == context->con->num_segs)) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output, "[%d]: Signal in recv\n",
                             ompi_comm_rank(context->con->comm)));
        ibcast_request_fini(context);
    }
    opal_free_list_return(mca_coll_adapt_component.adapt_ibcast_context_free_list,
                          (opal_free_list_item_t *) context);
    req->req_free(&req);

    /* Call back function return 1 to signal that request has been free'd */
    return 1;
}

int ompi_coll_adapt_ibcast(void *buff, int count, struct ompi_datatype_t *datatype, int root,
                          struct ompi_communicator_t *comm, ompi_request_t ** request,
                          mca_coll_base_module_t * module)
{
    OPAL_OUTPUT_VERBOSE((10, mca_coll_adapt_component.adapt_output,
                         "ibcast root %d, algorithm %d, coll_adapt_ibcast_segment_size %zu, coll_adapt_ibcast_max_send_requests %d, coll_adapt_ibcast_max_recv_requests %d\n",
                         root, mca_coll_adapt_component.adapt_ibcast_algorithm,
                         mca_coll_adapt_component.adapt_ibcast_segment_size,
                         mca_coll_adapt_component.adapt_ibcast_max_send_requests,
                         mca_coll_adapt_component.adapt_ibcast_max_recv_requests));

    if (OMPI_COLL_ADAPT_ALGORITHM_TUNED == mca_coll_adapt_component.adapt_ibcast_algorithm) {
        OPAL_OUTPUT_VERBOSE((10, mca_coll_adapt_component.adapt_output, "tuned not implemented\n"));
        return OMPI_ERR_NOT_IMPLEMENTED;
    }

    return ompi_coll_adapt_ibcast_generic(buff, count, datatype, root, comm, request, module,
                                          ompi_coll_adapt_module_cached_topology(module, comm, root, mca_coll_adapt_component.adapt_ibcast_algorithm),
                                          mca_coll_adapt_component.adapt_ibcast_segment_size);
}


int ompi_coll_adapt_ibcast_generic(void *buff, int count, struct ompi_datatype_t *datatype, int root,
                                   struct ompi_communicator_t *comm, ompi_request_t ** request,
                                   mca_coll_base_module_t * module, ompi_coll_tree_t * tree,
                                   size_t seg_size)
{
    int i, j, rank, err;
    /* The min of num_segs and SEND_NUM or RECV_NUM, in case the num_segs is less than SEND_NUM or RECV_NUM */
    int min;

    /* Number of datatype in a segment */
    int seg_count = count;
    /* Size of a datatype */
    size_t type_size;
    /* Real size of a segment */
    size_t real_seg_size;
    ptrdiff_t extent, lb;
    /* Number of segments */
    int num_segs;

    mca_pml_base_send_mode_t sendmode = (mca_coll_adapt_component.adapt_ibcast_synchronous_send)
                                        ? MCA_PML_BASE_SEND_SYNCHRONOUS : MCA_PML_BASE_SEND_STANDARD;

    /* The request passed outside */
    ompi_coll_base_nbc_request_t *temp_request = NULL;
    opal_mutex_t *mutex;
    /* Store the segments which are received */
    int *recv_array = NULL;
    /* Record how many isends have been issued for every child */
    int *send_array = NULL;

    /* Atomically set up free list */
    if (NULL == mca_coll_adapt_component.adapt_ibcast_context_free_list) {
        opal_free_list_t* fl = OBJ_NEW(opal_free_list_t);
        opal_free_list_init(fl,
                            sizeof(ompi_coll_adapt_bcast_context_t),
                            opal_cache_line_size,
                            OBJ_CLASS(ompi_coll_adapt_bcast_context_t),
                            0, opal_cache_line_size,
                            mca_coll_adapt_component.adapt_context_free_list_min,
                            mca_coll_adapt_component.adapt_context_free_list_max,
                            mca_coll_adapt_component.adapt_context_free_list_inc,
                            NULL, 0, NULL, NULL, NULL);
        if( !OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR((opal_atomic_intptr_t *)&mca_coll_adapt_component.adapt_ibcast_context_free_list,
                                                     &(intptr_t){0}, fl) ) {
            OBJ_RELEASE(fl);
        }
    }

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
    mutex = OBJ_NEW(opal_mutex_t);

    rank = ompi_comm_rank(comm);

    /* Determine number of elements sent per operation */
    ompi_datatype_type_size(datatype, &type_size);
    COLL_BASE_COMPUTED_SEGCOUNT(seg_size, type_size, seg_count);

    ompi_datatype_get_extent(datatype, &lb, &extent);
    num_segs = (count + seg_count - 1) / seg_count;
    real_seg_size = (ptrdiff_t) seg_count *extent;

    /* Set memory for recv_array and send_array, created on heap because they are needed to be accessed by other functions (callback functions) */
    if (num_segs != 0) {
        recv_array = (int *) malloc(sizeof(int) * num_segs);
    }
    if (tree->tree_nextsize != 0) {
        send_array = (int *) malloc(sizeof(int) * tree->tree_nextsize);
    }

    /* Set constant context for send and recv call back */
    ompi_coll_adapt_constant_bcast_context_t *con = OBJ_NEW(ompi_coll_adapt_constant_bcast_context_t);
    con->root = root;
    con->count = count;
    con->seg_count = seg_count;
    con->datatype = datatype;
    con->comm = comm;
    con->real_seg_size = real_seg_size;
    con->num_segs = num_segs;
    con->recv_array = recv_array;
    con->num_recv_segs = 0;
    con->num_recv_fini = 0;
    con->send_array = send_array;
    con->num_sent_segs = 0;
    con->mutex = mutex;
    con->request = (ompi_request_t*)temp_request;
    con->tree = tree;
    con->ibcast_tag = ompi_coll_base_nbc_reserve_tags(comm, num_segs);

    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: Ibcast, root %d, tag %d\n", rank, root,
                         con->ibcast_tag));
    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: con->mutex = %p, num_children = %d, num_segs = %d, real_seg_size = %d, seg_count = %d, tree_adreess = %p\n",
                         rank, (void *) con->mutex, tree->tree_nextsize, num_segs,
                         (int) real_seg_size, seg_count, (void *) con->tree));

    OPAL_THREAD_LOCK(mutex);

    /* If the current process is root, it sends segment to every children */
    if (rank == root) {
        /* Handle the situation when num_segs < SEND_NUM */
        if (num_segs <= mca_coll_adapt_component.adapt_ibcast_max_send_requests) {
            min = num_segs;
        } else {
            min = mca_coll_adapt_component.adapt_ibcast_max_send_requests;
        }

        /* Set recv_array, root has already had all the segments */
        for (i = 0; i < num_segs; i++) {
            recv_array[i] = i;
        }
        con->num_recv_segs = num_segs;
        /* Set send_array, will send ompi_coll_adapt_ibcast_max_send_requests segments */
        for (i = 0; i < tree->tree_nextsize; i++) {
            send_array[i] = mca_coll_adapt_component.adapt_ibcast_max_send_requests;
        }

        ompi_request_t *send_req;
        /* Number of datatypes in each send */
        int send_count = seg_count;
        for (i = 0; i < min; i++) {
            if (i == (num_segs - 1)) {
                send_count = count - i * seg_count;
            }
            for (j = 0; j < tree->tree_nextsize; j++) {
                ompi_coll_adapt_bcast_context_t *context =
                    (ompi_coll_adapt_bcast_context_t *) opal_free_list_wait(mca_coll_adapt_component.
                                                                           adapt_ibcast_context_free_list);
                context->buff = (char *) buff + i * real_seg_size;
                context->frag_id = i;
                /* The id of peer in in children_list */
                context->child_id = j;
                /* Actual rank of the peer */
                context->peer = tree->tree_next[j];
                context->con = con;
                OBJ_RETAIN(con);

                char *send_buff = context->buff;
                OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                     "[%d]: Send(start in main): segment %d to %d at buff %p send_count %d tag %d\n",
                                     rank, context->frag_id, context->peer,
                                     (void *) send_buff, send_count, con->ibcast_tag - i));
                err =
                    MCA_PML_CALL(isend
                                 (send_buff, send_count, datatype, context->peer,
                                  con->ibcast_tag - i, sendmode, comm,
                                  &send_req));
                if (MPI_SUCCESS != err) {
                    return err;
                }
                /* Set send callback */
                OPAL_THREAD_UNLOCK(mutex);
                ompi_request_set_callback(send_req, send_cb, context);
                OPAL_THREAD_LOCK(mutex);
            }
        }

    }

    /* If the current process is not root, it receives data from parent in the tree. */
    else {
        /* Handle the situation when num_segs < RECV_NUM */
        if (num_segs <= mca_coll_adapt_component.adapt_ibcast_max_recv_requests) {
            min = num_segs;
        } else {
            min = mca_coll_adapt_component.adapt_ibcast_max_recv_requests;
        }

        /* Set recv_array, recv_array is empty */
        for (i = 0; i < num_segs; i++) {
            recv_array[i] = 0;
        }
        /* Set send_array to empty */
        for (i = 0; i < tree->tree_nextsize; i++) {
            send_array[i] = 0;
        }

        /* Create a recv request */
        ompi_request_t *recv_req;

        /* Recevice some segments from its parent */
        int recv_count = seg_count;
        for (i = 0; i < min; i++) {
            if (i == (num_segs - 1)) {
                recv_count = count - i * seg_count;
            }
            ompi_coll_adapt_bcast_context_t *context =
                (ompi_coll_adapt_bcast_context_t *) opal_free_list_wait(mca_coll_adapt_component.
                                                                       adapt_ibcast_context_free_list);
            context->buff = (char *) buff + i * real_seg_size;
            context->frag_id = i;
            context->peer = tree->tree_prev;
            context->con = con;
            OBJ_RETAIN(con);
            char *recv_buff = context->buff;
            OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                                 "[%d]: Recv(start in main): segment %d from %d at buff %p recv_count %d tag %d\n",
                                 ompi_comm_rank(context->con->comm), context->frag_id,
                                 context->peer, (void *) recv_buff, recv_count,
                                 con->ibcast_tag - i));
            err =
                MCA_PML_CALL(irecv
                             (recv_buff, recv_count, datatype, context->peer,
                              con->ibcast_tag - i, comm, &recv_req));
            /* Set receive callback */
            OPAL_THREAD_UNLOCK(mutex);
            if (MPI_SUCCESS != err) {
                return err;
            }
            ompi_request_set_callback(recv_req, recv_cb, context);
            OPAL_THREAD_LOCK(mutex);
        }

    }

    OPAL_THREAD_UNLOCK(mutex);

    OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                         "[%d]: End of Ibcast\n", rank));

    return MPI_SUCCESS;
}
