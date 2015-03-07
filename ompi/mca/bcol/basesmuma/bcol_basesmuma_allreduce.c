/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2013 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
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

#include "opal/include/opal_stdint.h"

#include "ompi/mca/bcol/base/base.h"
#include "bcol_basesmuma.h"

static int bcol_basesmuma_allreduce_intra_fanin_fanout_progress (bcol_function_args_t *input_args, mca_bcol_base_function_t *c_input_args);

int bcol_basesmuma_allreduce_init(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    comm_attribs.bcoll_type = BCOL_ALLREDUCE;
    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1048576;
    comm_attribs.data_src = DATA_SRC_KNOWN;

    /* selection logic at the ml level specifies a
     * request for a non-blocking algorithm
     * however, these algorithms are blocking
     * following what was done at the p2p level
     * we will specify non-blocking, but beware,
     * these algorithms are blocking and will not make use
     * of the progress engine
     */
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000;
    inv_attribs.datatype_bitmap = 0xffffffff;
    inv_attribs.op_types_bitmap = 0xffffffff;

    /* Set attributes for fanin fanout algorithm */
    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_basesmuma_allreduce_intra_fanin_fanout,
                                 bcol_basesmuma_allreduce_intra_fanin_fanout_progress);

    inv_attribs.bcol_msg_min = 20000;
    inv_attribs.bcol_msg_max = 10485760; /* range 4 */
    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_basesmuma_allreduce_intra_fanin_fanout,
                                 bcol_basesmuma_allreduce_intra_fanin_fanout_progress);

    /* Differs only in comm size */

    comm_attribs.data_src = DATA_SRC_UNKNOWN;
    comm_attribs.waiting_semantics = BLOCKING;

    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 8;

    /* Set attributes for recursive doubling algorithm */
    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_basesmuma_allreduce_intra_recursive_doubling,
                                 NULL);


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
                                   volatile mca_bcol_basesmuma_payload_t *data_buffs, int count, struct ompi_op_t *op, int process_shift)
{
    volatile mca_bcol_basesmuma_header_t *child_ctl_pointer;
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

            if (!IS_PEER_READY(child_ctl_pointer, ready_flag, sequence_number, ALLREDUCE_FLAG, bcol_id)) {
                *iteration = child;
                return BCOL_FN_STARTED;
            }

            child_data_pointer = data_buffs[child_rank].payload;
            child_rbuf = child_data_pointer + child_ctl_pointer->roffsets[bcol_id];

            ompi_op_reduce(op, (void *)child_rbuf, (void *)rbuf, count, dtype);
        } /* end child loop */
    }

    if (ROOT_NODE != my_reduction_node->my_node_type) {
        opal_atomic_wmb ();
        my_ctl_pointer->flags[ALLREDUCE_FLAG][bcol_id] = ready_flag;
    }

    /* done with this step. move on to fan out */
    *iteration = -1;

    return BCOL_FN_COMPLETE;
}

static int allreduce_fanout (mca_bcol_basesmuma_module_t *bcol_module, volatile mca_bcol_basesmuma_header_t *my_ctl_pointer,
                             volatile void *my_data_pointer, int process_shift, volatile mca_bcol_basesmuma_payload_t *data_buffs,
                             int sequence_number, int group_size, int rbuf_offset, size_t pack_len)
{
    volatile mca_bcol_basesmuma_header_t *parent_ctl_pointer;
    int bcol_id = (int) bcol_module->super.bcol_id;
    int8_t ready_flag = my_ctl_pointer->ready_flag + 1;
    netpatterns_tree_node_t *my_fanout_read_tree;
    volatile void *parent_data_pointer;
    int my_fanout_parent, my_rank;
    void *parent_rbuf, *rbuf;

    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    my_fanout_read_tree = &(bcol_module->fanout_read_tree[my_rank]);

    if (ROOT_NODE != my_fanout_read_tree->my_node_type) {
        my_fanout_parent = my_fanout_read_tree->parent_rank + process_shift;
        if (group_size <= my_fanout_parent) {
            my_fanout_parent -= group_size;
        }

        rbuf = (void *)((char *) my_data_pointer + rbuf_offset);

        /*
         * Get parent payload data and control data.
         * Get the pointer to the base address of the parent's payload buffer.
         * Get the parent's control buffer.
         */
        parent_data_pointer = data_buffs[my_fanout_parent].payload;
        parent_ctl_pointer = data_buffs[my_fanout_parent].ctl_struct;

        parent_rbuf = (void *) ((char *) parent_data_pointer + rbuf_offset);

        /* Wait until parent signals that data is ready */
        /* The order of conditions checked in this loop is important, as it can
         * result in a race condition.
         */
        if (!IS_PEER_READY(parent_ctl_pointer, ready_flag, sequence_number, ALLREDUCE_FLAG, bcol_id)) {
            return BCOL_FN_STARTED;
        }

        assert (parent_ctl_pointer->flags[ALLREDUCE_FLAG][bcol_id] == ready_flag);

        /* Copy the rank to a shared buffer writable by the current rank */
        memcpy ((void *) rbuf, (const void*) parent_rbuf, pack_len);
    }

    if (LEAF_NODE != my_fanout_read_tree->my_node_type) {
        opal_atomic_wmb ();

        /* Signal to children that they may read the data from my shared buffer (bump the ready flag) */
        my_ctl_pointer->flags[ALLREDUCE_FLAG][bcol_id] = ready_flag;
    }

    my_ctl_pointer->starting_flag_value[bcol_id] += 1;

    return BCOL_FN_COMPLETE;

}

static int bcol_basesmuma_allreduce_intra_fanin_fanout_progress (bcol_function_args_t *input_args, mca_bcol_base_function_t *c_input_args)
{
    mca_bcol_basesmuma_module_t *bcol_module = (mca_bcol_basesmuma_module_t *) c_input_args->bcol_module;
    int buff_idx = input_args->src_desc->buffer_index;
    int *iteration = &bcol_module->ml_mem.nb_coll_desc[buff_idx].iteration;
    void *data_addr = (void *) input_args->src_desc->data_addr;
    int my_node_index, my_rank, group_size, leading_dim, idx;
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    int64_t sequence_number = input_args->sequence_num;
    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    struct ompi_datatype_t *dtype = input_args->dtype;
    netpatterns_tree_node_t *my_reduction_node;
    struct ompi_op_t *op = input_args->op;
    volatile void *my_data_pointer;
    int count = input_args->count;
    int rc, process_shift;
    ptrdiff_t lb, extent;
    volatile void *rbuf;

    /* get addressing information */
    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    group_size = bcol_module->colls_no_user_data.size_of_group;
    leading_dim = bcol_module->colls_no_user_data.size_of_group;
    idx = SM_ARRAY_INDEX(leading_dim,buff_idx,0);

    /* Align node index to around sbgp root */
    process_shift = input_args->root;
    my_node_index = my_rank - input_args->root;
    if (0 > my_node_index ) {
        my_node_index += group_size;
    }

    data_buffs = (volatile mca_bcol_basesmuma_payload_t *) bcol_module->colls_with_user_data.data_buffs + idx;
    /* Get control structure and payload buffer */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;
    my_data_pointer = (volatile char *) data_addr;

    my_data_pointer = (volatile char *) data_addr;
    rbuf = (volatile void *)((char *) my_data_pointer + input_args->rbuf_offset);

    /***************************
     * Fan into root phase
     ***************************/

    my_reduction_node = &(bcol_module->reduction_tree[my_node_index]);
    if (-1 != *iteration) {
        rc = reduce_children (bcol_module, rbuf, my_reduction_node, iteration, my_ctl_pointer,
                              dtype, data_buffs, count, op, process_shift);
        if (BCOL_FN_COMPLETE != rc) {
            return rc;
        }
    }

    /* there might be non-contig dtype - so compute the length with get_extent */
    ompi_datatype_get_extent(dtype, &lb, &extent);

    /***************************
     * Fan out from root
     ***************************/

    /* all nodes will have the result after fanout */
    input_args->result_in_rbuf = true;

    /* Signal that you are ready for fanout phase */
    return allreduce_fanout (bcol_module, my_ctl_pointer, my_data_pointer, process_shift, data_buffs,
                             sequence_number, group_size, input_args->rbuf_offset, count * (size_t) extent);
}

/**
 * Shared memory blocking allreduce.
 */
int bcol_basesmuma_allreduce_intra_fanin_fanout(bcol_function_args_t *input_args, mca_bcol_base_function_t *c_input_args)
{
    /* local variables */
    mca_bcol_basesmuma_module_t *bcol_module = (mca_bcol_basesmuma_module_t *) c_input_args->bcol_module;
    int buff_idx = input_args->src_desc->buffer_index;
    int *iteration = &bcol_module->ml_mem.nb_coll_desc[buff_idx].iteration;
    void *data_addr = (void *) input_args->src_desc->data_addr;
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    struct ompi_datatype_t *dtype = input_args->dtype;
    int bcol_id = (int) bcol_module->super.bcol_id;
    int rc, my_rank, leading_dim, idx;
    volatile void *my_data_pointer;
    volatile void *sbuf, *rbuf;
    int8_t ready_flag;

    /* get addressing information */
    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    leading_dim = bcol_module->colls_no_user_data.size_of_group;
    idx = SM_ARRAY_INDEX(leading_dim, buff_idx, 0);

    data_buffs = (volatile mca_bcol_basesmuma_payload_t *) bcol_module->colls_with_user_data.data_buffs + idx;
    /* Get control structure */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;

    my_data_pointer = (volatile char *) data_addr;
    rbuf = (volatile void *)((char *) my_data_pointer + input_args->rbuf_offset);
    sbuf = (volatile void *)((char *) my_data_pointer + input_args->sbuf_offset);

    /* Setup resource recycling */
    /* Set for multiple instances of bcols */
    BASESMUMA_HEADER_INIT(my_ctl_pointer, ready_flag, input_args->sequence_num, bcol_id);

    if (sbuf != rbuf) {
        rc = ompi_datatype_copy_content_same_ddt (dtype, input_args->count, (char *)rbuf,
                                                  (char *)sbuf);
        if( 0 != rc ) {
            return OMPI_ERROR;
        }
    }

    *iteration = 0;
    my_ctl_pointer->ready_flag = ready_flag;

    return bcol_basesmuma_allreduce_intra_fanin_fanout_progress (input_args, c_input_args);
}



/* this thing uses the old bcol private control structures */
int bcol_basesmuma_allreduce_intra_recursive_doubling(bcol_function_args_t *input_args,
                                                      mca_bcol_base_function_t *c_input_args)
{

    int my_rank,group_size,my_node_index;
    int pair_rank, exchange, extra_rank, payload_len;
    size_t dt_size;
    int read_offset, write_offset;
    volatile void *my_data_pointer;
    volatile mca_bcol_basesmuma_ctl_struct_t *my_ctl_pointer = NULL,
        *partner_ctl_pointer = NULL,
        *extra_ctl_pointer = NULL;
    volatile void *my_read_pointer, *my_write_pointer, *partner_read_pointer,
        *extra_rank_readwrite_data_pointer,*extra_rank_read_data_pointer;
    mca_bcol_basesmuma_module_t* bcol_module =
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;

    int8_t ready_flag;
    int sbuf_offset,rbuf_offset,flag_offset;
    int root,count;
    struct ompi_op_t *op;
    int64_t sequence_number=input_args->sequence_num;
    struct ompi_datatype_t *dtype;
    int first_instance = 0;
    int leading_dim,idx;
    int buff_idx;
    mca_bcol_basesmuma_ctl_struct_t **ctl_structs;
    /*volatile void **data_buffs;*/
    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    netpatterns_pair_exchange_node_t *my_exchange_node;


    /*
     * Get addressing information
     */
    buff_idx = input_args->src_desc->buffer_index;

    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    group_size = bcol_module->colls_no_user_data.size_of_group;
    leading_dim = bcol_module->colls_no_user_data.size_of_group;
    idx = SM_ARRAY_INDEX(leading_dim,buff_idx,0);

    /*
     * Get SM control structures and payload buffers
     */
    ctl_structs = (mca_bcol_basesmuma_ctl_struct_t **)
        bcol_module->colls_with_user_data.ctl_buffs+idx;
    /*data_buffs = (volatile void **)
      bcol_module->colls_with_user_data.data_buffs+idx;*/

    data_buffs = (volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs + idx;


    /*
     * Get control structure and payload buffer
     */
    my_ctl_pointer = ctl_structs[my_rank];
    if (my_ctl_pointer->sequence_number < sequence_number) {
        first_instance=1;
    }
    my_data_pointer = data_buffs[my_rank].payload;

    /*
     * Align node index to around sbgp root
     */
    root = input_args->root;
    my_node_index = my_rank - root;
    if (0 > my_node_index) {
        my_node_index += group_size;
    }

    /*
     * Get data from arguments
     */
    sbuf_offset = input_args->sbuf_offset;
    rbuf_offset = input_args->rbuf_offset;
    op   = input_args->op;
    count = input_args->count;
    dtype = input_args->dtype;

    /*
     * Get my node for the reduction tree
     */
    my_exchange_node = &(bcol_module->recursive_doubling_tree);


    if (first_instance) {
        my_ctl_pointer->index = 1;
        my_ctl_pointer->starting_flag_value = 0;
        flag_offset = 0;
        my_ctl_pointer->flag = -1;
        /*
          for( i = 0; i < NUM_SIGNAL_FLAGS; i++){
          my_ctl_pointer->flags[ALLREDUCE_FLAG] = -1;
          }
        */
    } else {
        my_ctl_pointer->index++;
        flag_offset = my_ctl_pointer->starting_flag_value;
    }

    /* signal that I have arrived */
    /* opal_atomic_wmb (); */
    my_ctl_pointer->sequence_number = sequence_number;

    /* If we use this buffer more than once by an sm module in
     * a given collective, will need to distinguish between instances, so
     * we pick up the right data.
     */
    ready_flag = flag_offset + sequence_number + 1;

    /*
     * Set up pointers for using during recursive doubling phase
     */
    read_offset = sbuf_offset;
    write_offset = rbuf_offset;
    fprintf(stderr,"read offset %d write offset %d\n",read_offset,write_offset);
    my_read_pointer =  (volatile void *)((char *) my_data_pointer + read_offset);
    my_write_pointer = (volatile void *)((char *) my_data_pointer + write_offset);

    /*
     * When there are non-power 2 nodes, the extra nodes' data is copied and
     * reduced by partner exchange nodes.
     * Extra nodes: Nodes with rank greater nearest power of 2
     * Exchange nodes: Nodes with rank lesser than nearest power of 2 that
     * partner with extras nodes during reduction
     */

    if (0 < my_exchange_node->n_extra_sources) {
        /*
         * Signal extra node that data is ready
         */
        opal_atomic_wmb ();

        my_ctl_pointer->flag = ready_flag;

        if (EXCHANGE_NODE == my_exchange_node->node_type) {
            extra_rank = my_exchange_node->rank_extra_source;
            extra_ctl_pointer = ctl_structs[extra_rank];
            extra_rank_readwrite_data_pointer = (void *) ((char *) data_buffs[extra_rank].payload +
                                                          read_offset);

            /*
             * Wait for data to get ready
             */
            while (!((sequence_number == extra_ctl_pointer->sequence_number) &&
                     (extra_ctl_pointer->flag >= ready_flag))){
            }

            ompi_op_reduce(op,(void *)extra_rank_readwrite_data_pointer,
                           (void *)my_read_pointer, count, dtype);
        }
    }


    /* --Exchange node that reduces with extra node --: Signal to extra node that data is read
     * --Exchange node that doesn't reduce data with extra node --: This assignment
     * is used so it can sync with other nodes during exchange phase
     * --Extra node--: It can pass to next phase
     */
    ready_flag++;
    /*my_ctl_pointer->flags[ALLREDUCE_FLAG] = ready_flag;*/
    my_ctl_pointer->flag = ready_flag;


    /*
     * Exchange data with all the nodes that are less than max_power_2
     */
    for (exchange=0 ; exchange < my_exchange_node->n_exchanges ; exchange++) {
        int tmp=0;

        /*my_ctl_pointer->flags[ALLREDUCE_FLAG] = ready_flag;*/
        my_ctl_pointer->flag = ready_flag;
        pair_rank=my_exchange_node->rank_exchanges[exchange];
        partner_ctl_pointer = ctl_structs[pair_rank];
        partner_read_pointer = (volatile void *) ((char *)data_buffs[pair_rank].payload + read_offset);

        my_read_pointer =  (volatile void *)((char *) my_data_pointer + read_offset);
        my_write_pointer = (volatile void *)((char *) my_data_pointer + write_offset);

        /*
         * Wait for partner to be ready, so we can read
         */
        /*
          JSL ----  FIX ME  !!!!! MAKE ME COMPLIANT WITH NEW BUFFERS
          while (!IS_ALLREDUCE_PEER_READY(partner_ctl_pointer,
          ready_flag, sequence_number)) {
          }
        */

        /*
         * Perform reduction operation
         */
        ompi_3buff_op_reduce(op,(void *)my_read_pointer, (void *)partner_read_pointer,
                             (void *)my_write_pointer, count, dtype);


        /*
         * Signal that I am done reading my partner's data
         */
        ready_flag++;
        /*my_ctl_pointer->flags[ALLREDUCE_FLAG] = ready_flag;*/
        my_ctl_pointer->flag = ready_flag;

        while (ready_flag > partner_ctl_pointer->flag){
            opal_progress();
        }

        /*
         * Swap read and write offsets
         */
        tmp = read_offset;
        read_offset = write_offset;
        write_offset = tmp;

    }


    /*
     * Copy data in from the "extra" source, if need be
     */

    if (0 < my_exchange_node->n_extra_sources) {

        if (EXTRA_NODE == my_exchange_node->node_type) {

            int extra_rank_read_offset=-1,my_write_offset=-1;

            /* Offset the ready flag to sync with
             * exchange node which might going through exchange phases
             * unlike the extra node
             */
            ready_flag = ready_flag + my_exchange_node->log_2;

            if (my_exchange_node->log_2%2) {
                extra_rank_read_offset = rbuf_offset;
                my_write_offset = rbuf_offset;

            } else {
                extra_rank_read_offset = sbuf_offset;
                my_write_offset = sbuf_offset;

            }

            my_write_pointer = (volatile void*)((char *)my_data_pointer + my_write_offset);
            extra_rank = my_exchange_node->rank_extra_source;
            extra_ctl_pointer = ctl_structs[extra_rank];

            extra_rank_read_data_pointer = (volatile void *) ((char *)data_buffs[extra_rank].payload +
                                                              extra_rank_read_offset);

            /*
             * Wait for the exchange node to be ready
             */
            ompi_datatype_type_size(dtype, &dt_size);
            payload_len = count*dt_size;
#if 0
            fix me JSL !!!!!
                while (!IS_DATA_READY(extra_ctl_pointer, ready_flag, sequence_number)){
                }
#endif
            memcpy((void *)my_write_pointer,(const void *)
                   extra_rank_read_data_pointer, payload_len);

            ready_flag++;
            /*my_ctl_pointer->flags[ALLREDUCE_FLAG] = ready_flag;*/
            my_ctl_pointer->flag = ready_flag;


        } else {

            /*
             * Signal parent that data is ready
             */
            opal_atomic_wmb ();
            /*my_ctl_pointer->flags[ALLREDUCE_FLAG] = ready_flag;*/
            my_ctl_pointer->flag = ready_flag;

            /* wait until child is done to move on - this buffer will
             *   be reused for the next stripe, so don't want to move
             *   on too quick.
             */
            extra_rank = my_exchange_node->rank_extra_source;
            extra_ctl_pointer = ctl_structs[extra_rank];
        }
    }

    input_args->result_in_rbuf = my_exchange_node->log_2 & 1;

    my_ctl_pointer->starting_flag_value += 1;

    return BCOL_FN_COMPLETE;
}
