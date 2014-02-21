/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2013 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/include/ompi/constants.h"
#include "ompi/mca/bcol/bcol.h"
#include "bcol_ptpcoll_reduce.h"
#include "bcol_ptpcoll_utils.h"

static int bcol_ptpcoll_reduce_narray_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);

static int bcol_ptpcoll_reduce_narray(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);


#define NARRAY_RECV_NB(narray_node, process_shift, group_size,                            \
                        recv_buffer, pack_len, tag, comm, recv_requests,                      \
                        num_pending_recvs)                                                 \
do {                                                                                       \
    int n, rc = OMPI_SUCCESS;                                                              \
    int dst;                                                                               \
    int comm_dst;                                                                          \
    int offset = 0 ;                                                                       \
                                                                                           \
    /* Recieve data from all relevant childrens  */                                        \
    for (n = 0; n < narray_node->n_children; n++) {                                        \
                                                                                           \
        dst = narray_node->children_ranks[n] + process_shift;                              \
        if (dst >= group_size) {                                                           \
            dst -= group_size;                                                             \
        }                                                                                  \
        comm_dst = group_list[dst];                                                        \
                                                                                           \
        /* Non blocking send .... */                                                       \
        PTPCOLL_VERBOSE(1 , ("Reduce, Irecv data to %d[%d], count %d, tag %d, addr %p",    \
                    dst, comm_dst, pack_len, tag,                                             \
                    data_buffer));                                                         \
        rc = MCA_PML_CALL(irecv((void *)((unsigned char*)recv_buffer + offset), pack_len, MPI_BYTE,                     \
                    comm_dst, tag, comm,                                      \
                    &(recv_requests[*num_pending_recvs])));                                \
        if( OMPI_SUCCESS != rc ) {                                                         \
            PTPCOLL_VERBOSE(10, ("Failed to start non-blocking receive"));                 \
            return OMPI_ERROR;                                                             \
        }                                                                                  \
        ++(*num_pending_recvs);                                                            \
        offset += pack_len;                                                                \
    }                                                                                      \
} while(0)


static inline int narray_reduce(void *data_buffer, void *recv_buffer,
                                int nrecvs, int count,
                                struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                int *reduction_status) {
    int pack_len = count * dtype->super.size;
    int i = 0;
    void *source_buffer = NULL, *result_buffer = NULL;

    source_buffer = data_buffer;
    result_buffer = recv_buffer;

    for (i = 0; i < nrecvs; i++) {
        ompi_op_reduce(op, (void*)((unsigned char*) source_buffer) ,
                       (void*)((unsigned char*) result_buffer),
                       count,dtype);

        source_buffer = (void *)((unsigned char*)recv_buffer
                                 + (i+1) * pack_len);
    }

    *reduction_status = 1;
    return OMPI_SUCCESS;
}
static int bcol_ptpcoll_reduce_narray_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag = -1;
    int rc;
    int group_size = ptpcoll_module->group_size;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    uint32_t buffer_index = input_args->buffer_index;
    struct ompi_op_t *op = input_args->op;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **send_request =
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests[0];
    ompi_request_t **recv_requests =
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests[1];
    void *data_buffer = NULL;
    void *src_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    void *recv_buffer = (void *) (
            (unsigned char *)input_args->rbuf +
            (size_t)input_args->rbuf_offset);
    int count = input_args->count;
    struct ompi_datatype_t *dtype = input_args->dtype;
    int pack_len = input_args->count * input_args->dtype->super.size;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int matched = false;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int relative_group_index = 0;
    netpatterns_tree_node_t *narray_node = NULL;
    bool not_sent = false;
    int parent_rank  = -1, comm_parent_rank = -1;
    int group_root_index = input_args->root;

    if (!ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].reduce_init_called) {
        bcol_ptpcoll_reduce_narray(input_args, const_args);
    }
    /*
     * By default the src buffer is the data buffer,
     * only after reduction, the recv buffer becomes the
     * data buffer
     */
    data_buffer = src_buffer;

    relative_group_index = my_group_index - group_root_index;
    if (relative_group_index < 0) {
        relative_group_index +=group_size;
    }

    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level tags */
    tag = -tag;

    narray_node = &ptpcoll_module->narray_node[relative_group_index];

    PTPCOLL_VERBOSE(3, ("reduce, Narray tree Progress"));

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_reduce_narray, buffer index: %d "
                         "tag: %d "
                         "tag_mask: %d "
                         "sn: %d "
                         "root: %d [%d]"
                         "buff: %p ",
                         buffer_index, tag,
                         ptpcoll_module->tag_mask, input_args->sequence_num,
                         input_args->root_flag, input_args->root_route->rank,
                         data_buffer));

    /*
      Check if the data was received
     */
    if (0 != *active_requests) {
        matched = mca_bcol_ptpcoll_test_all_for_match
            (active_requests, recv_requests, &rc);
        if (OMPI_SUCCESS != rc) {
            return OMPI_ERROR;
        }


        /* All data was received, then do a reduction*/
        if(matched) {
           narray_reduce(data_buffer, recv_buffer, narray_node->n_children, count, dtype, op,
                   &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].reduction_status);

           /*
            * The reduction result is in the recv buffer, so it is the new data
            * buffer
            */
           data_buffer = recv_buffer;

           /* If not reduced, means also, you might not posted a send */
            not_sent = true;
        } else {
            PTPCOLL_VERBOSE(10, ("reduce root is started"));
            return BCOL_FN_STARTED;
        }
    }

    /* I'm root, I'm done  */
    if (input_args->root_flag) {
       return BCOL_FN_COMPLETE;
    }

    PTPCOLL_VERBOSE(1,("Testing Sending Match"));

    /* If send was not posted */
    /* Manju: Leaf node should never post in the progress logic */
    if (not_sent) {
        parent_rank =
            ptpcoll_module->narray_node[relative_group_index].parent_rank +
            group_root_index;
        if (parent_rank >= group_size) {
            parent_rank -= group_size;
        }

        comm_parent_rank = group_list[parent_rank];
        PTPCOLL_VERBOSE(1,("Sending data to %d ",comm_parent_rank));

        rc = MCA_PML_CALL(isend(data_buffer, pack_len, MPI_BYTE,
                    comm_parent_rank,
                    tag, MCA_PML_BASE_SEND_STANDARD, comm, send_request));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            return OMPI_ERROR;
        }
    }

    if (0 == mca_bcol_ptpcoll_test_for_match(send_request, &rc)) {
        PTPCOLL_VERBOSE(10, ("Test was not matched - %d", rc));
        /* Data has not been sent. Return that the collective has been stated
         * because we MUST call test on this request once it is finished to
         * ensure that it is properly freed. */
        return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
    }

    return BCOL_FN_COMPLETE;
}

static int bcol_ptpcoll_reduce_narray(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag;
    int rc;
    int group_size = ptpcoll_module->group_size;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    uint32_t buffer_index = input_args->buffer_index;

    struct ompi_op_t *op = input_args->op;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **recv_requests =
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests[1];
    ompi_request_t **send_request =
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests[0];

    void *data_buffer = NULL;
    void *src_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    void *recv_buffer = (void *) (
            (unsigned char *)input_args->rbuf +
            (size_t)input_args->rbuf_offset);
    int count = input_args->count;
    struct ompi_datatype_t *dtype = input_args->dtype;
    int pack_len = input_args->count * input_args->dtype->super.size;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int matched = true;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int group_root_index  = -1;
    int relative_group_index = 0;
    netpatterns_tree_node_t *narray_node = NULL;
    int parent_rank  = -1, comm_parent_rank = -1;


    /* This is first function call that should be called, not progress.
     * The fragmentation code does this, so switch from progress to here.
     * The flag indicates whether, we have entered this code *
     */
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].reduce_init_called = true;

    PTPCOLL_VERBOSE(1, ("Reduce, Narray tree"));
    /* reset active request counter */
    (*active_requests) = 0;
    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;

    PTPCOLL_VERBOSE(1, ("bcol_ptpcoll_reduce_narray, buffer index: %d "
                         "tag: %d "
                         "tag_mask: %d "
                         "sn: %d "
                         "root: %d "
                         "buff: %p ",
                         buffer_index, tag,
                         ptpcoll_module->tag_mask, input_args->sequence_num,
                         input_args->root_flag,
                         src_buffer));

    /* Compute Root Index Shift */
    group_root_index = input_args->root;
    relative_group_index = my_group_index - group_root_index;
    if (relative_group_index < 0) {
        relative_group_index += group_size;
    }

    narray_node = &ptpcoll_module->narray_node[relative_group_index];

    if (0 == narray_node->n_children) {
        PTPCOLL_VERBOSE(10, ("I'm leaf of the data"));
        /*
         * I'm root of the operation
         * send data to N childrens
         */
        data_buffer = src_buffer;
        goto NARRAY_SEND_DATA;
    }

    /* Not leaf, either an internal node or root */
    NARRAY_RECV_NB(narray_node, group_root_index, group_size,
                    recv_buffer, pack_len, tag, comm, recv_requests,
                    active_requests);


    /* We have not done reduction, yet */
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].reduction_status = 0;

    /* We can not block. So run couple of test for data arrival */
    matched = mca_bcol_ptpcoll_test_all_for_match
        (active_requests, recv_requests, &rc);

    /* Check if received the data */
    if(matched) {

        narray_reduce(src_buffer, recv_buffer, narray_node->n_children,
                        count, dtype, op, &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].reduction_status);
        PTPCOLL_VERBOSE(1, ("Reduce, received data from  all childrend "));
        data_buffer = recv_buffer;

    } else {

        PTPCOLL_VERBOSE(1, ("reduce root is started"));
        return BCOL_FN_STARTED;
    }

    /* I'm root, I'm done  */
    if (input_args->root_flag) {
       return BCOL_FN_COMPLETE;
    }


NARRAY_SEND_DATA:

    /*
     * Send the data (reduce in case of internal nodes, or just data in
     * case of leaf nodes) to the parent
     */
    narray_node = &ptpcoll_module->narray_node[relative_group_index];

    parent_rank =
        ptpcoll_module->narray_node[relative_group_index].parent_rank +
        group_root_index;
    if (parent_rank >= group_size) {
        parent_rank -= group_size;
    }

    comm_parent_rank = group_list[parent_rank];
    PTPCOLL_VERBOSE(1,("Sending data to %d ",comm_parent_rank));

    rc = MCA_PML_CALL(isend(data_buffer, pack_len, MPI_BYTE,
                comm_parent_rank,
                tag, MCA_PML_BASE_SEND_STANDARD, comm, send_request));
    if( OMPI_SUCCESS != rc ) {
        PTPCOLL_VERBOSE(10, ("Failed to send data"));
        return OMPI_ERROR;
    }

    /* We can not block. So run couple of test for data arrival */
    if (0 == mca_bcol_ptpcoll_test_for_match(send_request, &rc)) {
        PTPCOLL_VERBOSE(10, ("Test was not matched - %d", rc));
        /* No data was received, return no match error */
        return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
    }

    return BCOL_FN_COMPLETE;
}


int bcol_ptpcoll_reduce_init(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    PTPCOLL_VERBOSE(1,("Initialization Reduce - Narray"));
    comm_attribs.bcoll_type = BCOL_REDUCE;
    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1024 * 1024;
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000; /* range 1 */

    inv_attribs.datatype_bitmap = 0xffffffff;
    inv_attribs.op_types_bitmap = 0xffffffff;


    comm_attribs.data_src = DATA_SRC_KNOWN;
    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                bcol_ptpcoll_reduce_narray,
                bcol_ptpcoll_reduce_narray_progress);

    comm_attribs.data_src = DATA_SRC_KNOWN;

    return OMPI_SUCCESS;
}
