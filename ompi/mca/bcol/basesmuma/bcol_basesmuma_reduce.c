/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2013 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/constants.h"
#include "ompi/op/op.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/mca/bcol/bcol.h"

#include "opal/include/opal_stdint.h"

#include "bcol_basesmuma.h"
#include "bcol_basesmuma_reduce.h"
/**
 * gvm - Shared memory reduce
 */

static int bcol_basesmuma_reduce_intra_fanin_progress(bcol_function_args_t *input_args,
                                                      mca_bcol_base_function_t *c_input_args);

int bcol_basesmuma_reduce_init(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    comm_attribs.bcoll_type = BCOL_REDUCE;
    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1048576;
    comm_attribs.data_src = DATA_SRC_KNOWN;
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000;
    inv_attribs.datatype_bitmap = 0x11111111;
    inv_attribs.op_types_bitmap = 0x11111111;


    /* Set attributes for fanin fanout algorithm */
    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs, bcol_basesmuma_reduce_intra_fanin,
                                 bcol_basesmuma_reduce_intra_fanin_progress);

    inv_attribs.bcol_msg_min = 10000000;
    inv_attribs.bcol_msg_max = 10485760; /* range 4 */

    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs, NULL, NULL);

    return OMPI_SUCCESS;
}

/*
 * Small data fanin reduce
 * ML buffers are used for both payload and control structures
 * This functions works with hierarchical allreduce and
 * progress engine
 */
static inline int reduce_children (mca_bcol_basesmuma_module_t *bcol_module, volatile void *rbuf, netpatterns_tree_node_t *my_reduction_node,
                                   int *iteration, volatile mca_bcol_basesmuma_header_t *my_ctl_pointer, ompi_datatype_t *dtype,
                                   volatile mca_bcol_basesmuma_payload_t *data_buffs, int count, struct ompi_op_t *op, int process_shift) {
    volatile mca_bcol_basesmuma_header_t * child_ctl_pointer;
    int bcol_id = (int) bcol_module->super.bcol_id;
    int64_t sequence_number = my_ctl_pointer->sequence_number;
    int8_t ready_flag = my_ctl_pointer->ready_flag;
    int group_size = bcol_module->colls_no_user_data.size_of_group;

    if (LEAF_NODE != my_reduction_node->my_node_type) {
        volatile char *child_data_pointer;
        volatile void *child_rbuf;

        /* for each child */
        /* my_result_data = child_result_data (op) my_source_data */

        for (int child = *iteration ; child < my_reduction_node->n_children ; ++child) {
            int child_rank = my_reduction_node->children_ranks[child] + process_shift;

            if (group_size <= child_rank){
                child_rank -= group_size;
            }

            child_ctl_pointer = data_buffs[child_rank].ctl_struct;
            child_data_pointer = data_buffs[child_rank].payload;

            if (!IS_PEER_READY(child_ctl_pointer, ready_flag, sequence_number, REDUCE_FLAG, bcol_id)) {
                *iteration = child;
                return BCOL_FN_STARTED;
            }

            child_rbuf = child_data_pointer + child_ctl_pointer->roffsets[bcol_id];

            ompi_op_reduce(op,(void *)child_rbuf,(void *)rbuf, count, dtype);
        } /* end child loop */
    }

    if (ROOT_NODE != my_reduction_node->my_node_type) {
        opal_atomic_wmb ();
        my_ctl_pointer->flags[REDUCE_FLAG][bcol_id] = ready_flag;
    }

    return BCOL_FN_COMPLETE;
}

static int bcol_basesmuma_reduce_intra_fanin_progress(bcol_function_args_t *input_args,
                                                      mca_bcol_base_function_t *c_input_args)
{
    mca_bcol_basesmuma_module_t* bcol_module =
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;

    netpatterns_tree_node_t *my_reduction_node;
    int my_rank, my_node_index;
    struct ompi_datatype_t *dtype = input_args->dtype;
    int leading_dim, idx;

    /* Buffer index */
    int buff_idx = input_args->src_desc->buffer_index;

    int *iteration = &bcol_module->ml_mem.nb_coll_desc[buff_idx].iteration;

    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    void *data_addr = (void *)input_args->src_desc->data_addr;
    volatile void *rbuf;

    /* get addressing information */
    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    leading_dim = bcol_module->colls_no_user_data.size_of_group;
    idx = SM_ARRAY_INDEX(leading_dim, buff_idx, 0);

    data_buffs = (volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs + idx;

    /* Get control structure and payload buffer */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;

    my_node_index = my_rank - input_args->root;
    if (0 > my_node_index) {
        int group_size = bcol_module->colls_no_user_data.size_of_group;
        my_node_index += group_size;
    }

    my_reduction_node = bcol_module->reduction_tree + my_node_index;
    rbuf = (volatile void *)((uintptr_t) data_addr + input_args->rbuf_offset);

    return reduce_children (bcol_module, rbuf, my_reduction_node, iteration, my_ctl_pointer, dtype,
                            data_buffs, input_args->count, input_args->op, input_args->root);
}

int bcol_basesmuma_reduce_intra_fanin(bcol_function_args_t *input_args,
                                      mca_bcol_base_function_t *c_input_args)
{
    /* local variables */
    int rc=BCOL_FN_COMPLETE;
    int my_rank,group_size,my_node_index;
    mca_bcol_basesmuma_module_t* bcol_module =
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;

    netpatterns_tree_node_t *my_reduction_node;
    volatile int8_t ready_flag;
    int bcol_id = (int) bcol_module->super.bcol_id;
    volatile void *sbuf,*rbuf;
    int sbuf_offset,rbuf_offset;
    int root,count;
    int64_t sequence_number=input_args->sequence_num;
    struct ompi_datatype_t *dtype;
    int leading_dim,idx;

    /* Buffer index */
    int buff_idx = input_args->src_desc->buffer_index;

    int *iteration = &bcol_module->ml_mem.nb_coll_desc[buff_idx].iteration;

    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    volatile char * my_data_pointer;
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    void *data_addr = (void *)input_args->src_desc->data_addr;

#if 0
    fprintf(stderr,"777 entering sm reduce \n");
#endif

    /* get addressing information */
    my_rank=bcol_module->super.sbgp_partner_module->my_index;
    group_size=bcol_module->colls_no_user_data.size_of_group;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);

    data_buffs = (volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;
    /* fprintf(stderr,"AAA the devil!!\n"); */
    /* Get control structure and payload buffer */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;
    my_data_pointer = (volatile char *)data_addr;

    /* Align node index to around sbgp root */
    root = input_args->root;
    my_node_index = my_rank - root;
    if (0 > my_node_index) {
        my_node_index += group_size;
    }

    /* get arguments */
    sbuf_offset = input_args->sbuf_offset;
    rbuf_offset = input_args->rbuf_offset;
    sbuf = (volatile void *)(my_data_pointer + sbuf_offset);
    data_buffs[my_rank].payload = (void*)sbuf;
    rbuf = (volatile void *)(my_data_pointer + rbuf_offset);
    count = input_args->count;
    dtype = input_args->dtype;

    /* Cache my rbuf_offset */
    my_ctl_pointer->roffsets[bcol_id] = rbuf_offset;

    /* get my node for the reduction tree */
    my_reduction_node=&(bcol_module->reduction_tree[my_node_index]);

    /* init the header */
    BASESMUMA_HEADER_INIT(my_ctl_pointer, ready_flag, sequence_number, bcol_id);

    input_args->result_in_rbuf = (ROOT_NODE == my_reduction_node->my_node_type);

    /* set starting point for progress loop */
    *iteration = 0;
    my_ctl_pointer->ready_flag = ready_flag;

    if (sbuf != rbuf) {
        rc = ompi_datatype_copy_content_same_ddt(dtype, count, (char *)rbuf,
                                                 (char *)sbuf);
        if( 0 != rc ) {
            return OMPI_ERROR;
        }
    }

    rc = reduce_children (bcol_module, rbuf, my_reduction_node, iteration, my_ctl_pointer, dtype,
                          data_buffs, count, input_args->op, root);

    /* Flag value if other bcols are called */
    my_ctl_pointer->starting_flag_value[bcol_id]++;

    /* Recycle payload buffers */

    return rc;
}

/* Small data fanin reduce
 * Uses SM buffer (backed by SM file) for both control structures and
 * payload
 *
 * NTH: How does this differ from the new one? Can we replace this
 * with a call to the new init then a call the new progress until
 * complete?
 */
int bcol_basesmuma_reduce_intra_fanin_old(bcol_function_args_t *input_args,
                                          mca_bcol_base_function_t *c_input_args)
{
    /* local variables */
    int rc=OMPI_SUCCESS;
    int my_rank,group_size,process_shift,my_node_index;
    int n_children,child;
    mca_bcol_basesmuma_module_t* bcol_module =
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;

    netpatterns_tree_node_t *my_reduction_node;
    volatile int8_t ready_flag;
    volatile void *sbuf,*rbuf;
    int sbuf_offset,rbuf_offset;
    int root,count;
    struct ompi_op_t *op;
    int64_t sequence_number=input_args->sequence_num;
    struct ompi_datatype_t *dtype;
    int leading_dim,idx;
    int buff_idx;
    int child_rank;
    int bcol_id = (int) bcol_module->super.bcol_id;

    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    volatile char * my_data_pointer;
    volatile char * child_data_pointer;
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    volatile mca_bcol_basesmuma_header_t * child_ctl_pointer;

#if 0
    fprintf(stderr,"Entering fanin reduce \n");
#endif

    /* Buffer index */
    buff_idx = input_args->src_desc->buffer_index;
    /* get addressing information */
    my_rank=bcol_module->super.sbgp_partner_module->my_index;
    group_size=bcol_module->colls_no_user_data.size_of_group;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);

    /*ctl_structs=(mca_bcol_basesmuma_ctl_struct_t **)
      bcol_module->colls_with_user_data.ctl_buffs+idx;*/
    data_buffs = (volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;

    /* Get control structure and payload buffer */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;
    my_data_pointer = (volatile char *) data_buffs[my_rank].payload;

    /* Align node index to around sbgp root */
    root = input_args->root;
    process_shift = root;
    my_node_index = my_rank - root;
    if (0 > my_node_index ) {
        my_node_index += group_size;
    }

    /* get arguments */
    sbuf_offset = input_args->sbuf_offset;
    rbuf_offset = input_args->rbuf_offset;
    sbuf = (volatile void *)(my_data_pointer + sbuf_offset);
    rbuf = (volatile void *)(my_data_pointer + rbuf_offset);
    op   = input_args->op;
    count = input_args->count;
    dtype = input_args->dtype;

    /* get my node for the reduction tree */
    my_reduction_node=&(bcol_module->reduction_tree[my_node_index]);
    n_children=my_reduction_node->n_children;

    /* init the header */
    BASESMUMA_HEADER_INIT(my_ctl_pointer, ready_flag, sequence_number, bcol_id);

    input_args->result_in_rbuf = (ROOT_NODE == my_reduction_node->my_node_type);

    rc = ompi_datatype_copy_content_same_ddt(dtype, count, (char *)rbuf,
                                             (char *)sbuf);
    if (0 != rc) {
        return OMPI_ERROR;
    }

    if (LEAF_NODE != my_reduction_node->my_node_type) {
        volatile void *child_rbuf;
        /* for each child */
        /* my_result_data = child_result_data (op) my_source_data */

        for (child = 0 ; child < n_children ; ++child) {
            child_rank = my_reduction_node->children_ranks[child];
            child_rank += process_shift;

            /* wrap around */
            if( group_size <= child_rank ){
                child_rank-=group_size;
            }

            /*child_ctl_pointer = ctl_structs[child_rank];*/
            child_ctl_pointer = data_buffs[child_rank].ctl_struct;
            child_data_pointer = data_buffs[child_rank].payload;

            child_rbuf = child_data_pointer + rbuf_offset;
            /* wait until child child's data is ready for use */
            while (!IS_PEER_READY(child_ctl_pointer, ready_flag, sequence_number, REDUCE_FLAG, bcol_id)) {
                opal_progress();
            }

            /* apply collective operation */
            ompi_op_reduce(op,(void *)child_rbuf,(void *)rbuf, count,dtype);
        } /* end child loop */
    }

    if (ROOT_NODE != my_reduction_node->my_node_type) {
        opal_atomic_wmb ();
        my_ctl_pointer->flags[REDUCE_FLAG][bcol_id] = ready_flag;
    }

    my_ctl_pointer->starting_flag_value[bcol_id]++;

    return rc;
}
