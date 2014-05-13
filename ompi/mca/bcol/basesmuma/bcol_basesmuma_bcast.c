/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"

#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"

#include "bcol_basesmuma.h"

#define __TEST_BLOCKING__   1
#define __TEST_WAIT__       0
#define __TEST_TEST__       0

/* debug
 *   #include "opal/sys/timer.h"
 *
 *   extern uint64_t timers[7];
 *   end debug */

/* debug */
/* end debug */
int bcol_basesmuma_bcast_init(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    comm_attribs.bcoll_type = BCOL_BCAST;
    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1048576;
    comm_attribs.data_src = DATA_SRC_KNOWN;
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000; /* range 1 */
    inv_attribs.datatype_bitmap = 0xffffffff;
    inv_attribs.op_types_bitmap = 0xffffffff;

    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_basesmuma_bcast_k_nomial_knownroot,
                                 bcol_basesmuma_bcast_k_nomial_knownroot);

    inv_attribs.bcol_msg_min = 10000000;
    inv_attribs.bcol_msg_max = 10485760; /* range 4 */

    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_basesmuma_bcast_k_nomial_knownroot,
                                 bcol_basesmuma_bcast_k_nomial_knownroot);

    comm_attribs.data_src = DATA_SRC_UNKNOWN;
    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000; /* range 1 */

    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_basesmuma_bcast_k_nomial_anyroot,
                                 bcol_basesmuma_bcast_k_nomial_anyroot);

    comm_attribs.data_src = DATA_SRC_UNKNOWN;
    inv_attribs.bcol_msg_min = 10000000;
    inv_attribs.bcol_msg_max = 10485760; /* range 4 */

#ifdef __PORTALS_AVAIL__

    comm_attribs.waiting_semantics = BLOCKING;
    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_basesmuma_lmsg_scatter_allgather_portals_bcast,
                                 bcol_basesmuma_lmsg_scatter_allgather_portals_bcast);


    comm_attribs.waiting_semantics = NON_BLOCKING;
    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_basesmuma_lmsg_scatter_allgather_portals_nb_bcast,
                                 bcol_basesmuma_lmsg_scatter_allgather_portals_nb_bcast);

    comm_attribs.data_src = DATA_SRC_KNOWN;
    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_basesmuma_lmsg_scatter_allgather_portals_nb_knownroot_bcast,
                                 bcol_basesmuma_lmsg_scatter_allgather_portals_nb_knownroot_bcast);

#else
    /*
      if (super->use_hdl) {
      mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
      bcol_basesmuma_hdl_zerocopy_bcast,
      bcol_basesmuma_hdl_zerocopy_bcast);
      } else { */
    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs, NULL, NULL);
    /*
      bcol_basesmuma_binary_scatter_allgather_segment,
      bcol_basesmuma_binary_scatter_allgather_segment);
    */
    /*    } */
#endif

    return OMPI_SUCCESS;
}

/* includes shared memory optimization */

/**
 * Shared memory blocking Broadcast - fanin, for small data buffers.
 * This routine assumes that buf (the input buffer) is a single writer
 * multi reader (SWMR) shared memory buffer owned by the calling rank
 * which is the only rank that can write to this buffers.
 * It is also assumed that the buffers are registered and fragmented
 * at the ML level and that buf is sufficiently large to hold the data.
 *
 *
 * @param buf - SWMR shared buffer within a sbgp that the
 * executing rank can write to.
 * @param count - the number of elements in the shared buffer.
 * @param dtype - the datatype of a shared buffer element.
 * @param root - the index within the sbgp of the root.
 * @param module - basesmuma module.
 */
int bcol_basesmuma_bcast(bcol_function_args_t *input_args,
                         mca_bcol_base_function_t *c_input_args)
{
    /* local variables */
    int group_size, process_shift, my_node_index;
    int my_rank;
    int rc = OMPI_SUCCESS;
    int my_fanout_parent;
    int leading_dim, buff_idx, idx;
    volatile int8_t ready_flag;
    int count=input_args->count;
    struct ompi_datatype_t* dtype=input_args->dtype;
    int root=input_args->root;
    int64_t sequence_number=input_args->sequence_num;
    mca_bcol_basesmuma_module_t* bcol_module=
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
    int bcol_id = (int) bcol_module->super.bcol_id;
    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    volatile char* parent_data_pointer;
    mca_bcol_basesmuma_header_t *my_ctl_pointer;
    volatile mca_bcol_basesmuma_header_t *parent_ctl_pointer;
    netpatterns_tree_node_t* my_fanout_read_tree;
    size_t pack_len = 0, dt_size;

    void *data_addr = (void *)((unsigned char *)input_args->src_desc->data_addr );

#if 0
    fprintf(stderr,"Entering sm broadcast input_args->sbuf_offset %d \n",input_args->sbuf_offset);
    fflush(stderr);
#endif


    /* we will work only on packed data - so compute the length*/
    ompi_datatype_type_size(dtype, &dt_size);
    pack_len=count*dt_size;

    buff_idx = input_args->src_desc->buffer_index;

    /* Get addressing information */
    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    group_size = bcol_module->colls_no_user_data.size_of_group;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);
    data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;

    /* Align node index to around sbgp root */
    process_shift = root;
    my_node_index = my_rank - root;
    if(0 > my_node_index ) {
        my_node_index += group_size;
    }

    /* get my node for the bcast tree */
    my_fanout_read_tree = &(bcol_module->fanout_read_tree[my_node_index]);
    my_fanout_parent = my_fanout_read_tree->parent_rank + process_shift;
    if(group_size <= my_fanout_parent){
        my_fanout_parent -= group_size;
    }

    /* Set pointer to current proc ctrl region */
    /*my_ctl_pointer = ctl_structs[my_rank]; */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;

    /* setup resource recycling */

    BASESMUMA_HEADER_INIT(my_ctl_pointer, ready_flag, sequence_number, bcol_id);

    /*
     * Fan out from root
     */
    if(ROOT_NODE == my_fanout_read_tree->my_node_type) {
        input_args->result_in_rbuf = false;
        /* Root should only signal it is ready */
        my_ctl_pointer->flags[BCAST_FLAG][bcol_id] = ready_flag;

    }else if(LEAF_NODE == my_fanout_read_tree->my_node_type) {
        input_args->result_in_rbuf = false;
        /*
         * Get parent payload data and control data.
         * Get the pointer to the base address of the parent's payload buffer.
         * Get the parent's control buffer.
         */
        parent_data_pointer = data_buffs[my_fanout_parent].payload;
        parent_ctl_pointer = data_buffs[my_fanout_parent].ctl_struct;

        /* Wait until parent signals that data is ready */
        /* The order of conditions checked in this loop is important, as it can
         * result in a race condition.
         */
        while (!IS_PEER_READY(parent_ctl_pointer, ready_flag, sequence_number, BCAST_FLAG, bcol_id)){
            opal_progress();
        }

        /* Copy the rank to a shared buffer writable by the current rank */
        memcpy(data_addr, (void *)parent_data_pointer, pack_len);

        if( 0 != rc ) {
            return OMPI_ERROR;
        }

    }else{
        input_args->result_in_rbuf = false;
        /* Interior node */

        /* Get parent payload data and control data */
        parent_data_pointer = data_buffs[my_fanout_parent].payload;
        parent_ctl_pointer =  data_buffs[my_fanout_parent].ctl_struct;


        /* Wait until parent signals that data is ready */
        /* The order of conditions checked in this loop is important, as it can
         * result in a race condition.
         */
        while (!IS_PEER_READY(parent_ctl_pointer, ready_flag, sequence_number, BCAST_FLAG, bcol_id)){
            opal_progress();
        }

        /* Copy the rank to a shared buffer writable by the current rank */
        memcpy(data_addr, (void *)parent_data_pointer,pack_len);

        /* Signal to children that they may read the data from my shared buffer */
        opal_atomic_wmb ();
        my_ctl_pointer->flags[BCAST_FLAG][bcol_id] = ready_flag;
    }

    /* if I am the last instance of a basesmuma function in this collectie,
     *   release the resrouces */

    my_ctl_pointer->starting_flag_value[bcol_id]++;

    return rc;
}


/*zero-copy large massage communication methods*/
#if 0
int bcol_basesmuma_hdl_zerocopy_bcast(bcol_function_args_t *input_args,
                                      mca_bcol_base_function_t   *c_input_args)
{
    /* local variables */
    int group_size, process_shift, my_node_index;
    int my_rank, first_instance=0, flag_offset;
    int rc = OMPI_SUCCESS;
    int my_fanout_parent;
    int leading_dim, buff_idx, idx;
    volatile int64_t ready_flag;
    int count=input_args->count;
    struct ompi_datatype_t* dtype=input_args->dtype;
    int root=input_args->root;
    int64_t sequence_number=input_args->sequence_num;
    mca_bcol_basesmuma_module_t* bcol_module=
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;

    netpatterns_tree_node_t* my_fanout_read_tree;
    size_t pack_len = 0, dt_size;

    void *data_addr = (void *)((unsigned char *)input_args->src_desc->data_addr);

    struct mca_hdl_base_descriptor_t *hdl_desc;
    struct mca_hdl_base_segment_t *hdl_seg;
    int ret, completed, ridx/*remote rank index*/;
    bool status;
    volatile mca_bcol_basesmuma_ctl_struct_t **ctl_structs;
    mca_bcol_basesmuma_ctl_struct_t  *my_ctl_pointer= NULL;
    volatile mca_bcol_basesmuma_ctl_struct_t  *parent_ctl_pointer= NULL;
    volatile mca_bcol_basesmuma_ctl_struct_t  *child_ctl_pointer= NULL;
    struct mca_hdl_base_module_t* hdl = bcol_module->hdl_module[0];


    /* we will work only on packed data - so compute the length*/
    ompi_datatype_type_size(dtype, &dt_size);
    pack_len = count * dt_size;

    buff_idx = input_args->src_desc->buffer_index;

    /* Get addressing information */
    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    group_size = bcol_module->colls_no_user_data.size_of_group;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);
    ctl_structs = (volatile mca_bcol_basesmuma_ctl_struct_t **)
        bcol_module->colls_with_user_data.ctl_buffs+idx;
    my_ctl_pointer = ctl_structs[my_rank];

    /* Align node index to around sbgp root */
    process_shift = root;
    my_node_index = my_rank - root;
    if(0 > my_node_index ) {
        my_node_index += group_size;
    }

    /* get my node for the bcast tree */
    my_fanout_read_tree = &(bcol_module->fanout_read_tree[my_node_index]);
    my_fanout_parent = my_fanout_read_tree->parent_rank + process_shift;
    if(group_size <= my_fanout_parent){
        my_fanout_parent -= group_size;
    }

    /* setup resource recycling */
    if( my_ctl_pointer->sequence_number < sequence_number ) {
        first_instance = 1;
    }

    if( first_instance ) {
        /* Signal arrival */
        my_ctl_pointer->flag  = -1;
        my_ctl_pointer->index = 1;
        /* this does not need to use any flag values , so only need to
         * set the value for subsequent values that may need this */
        my_ctl_pointer->starting_flag_value = 0;
        flag_offset = 0;
    } else {
        /* only one thread at a time will be making progress on this
         *   collective, so no need to make this atomic */
        my_ctl_pointer->index++;
    }


    /* increment the starting flag by one and return */
    flag_offset = my_ctl_pointer->starting_flag_value;
    ready_flag = flag_offset + sequence_number + 1;
    my_ctl_pointer->sequence_number = sequence_number;

    hdl_desc = (mca_hdl_base_descriptor_t *)
        malloc (sizeof (mca_hdl_base_descriptor_t) * 1);

    /*prepare a hdl data segment*/
    hdl_seg = (mca_hdl_base_segment_t*)
        malloc ( sizeof (mca_hdl_base_segment_t) * 1);
    hdl_seg->seg_addr.pval = input_args->sbuf;
    hdl_seg->seg_len = pack_len;


    hdl->endpoint->ready_flag = ready_flag;
    hdl->endpoint->local_ctrl  = my_ctl_pointer;
    hdl->endpoint->sbgp_contextid =
        bcol_module->super.sbgp_partner_module->group_comm->c_contextid;

    /*
     * Fan out from root
     */
    if(ROOT_NODE == my_fanout_read_tree->my_node_type) {
        input_args->result_in_rbuf = false;

        hdl_desc->des_src = hdl_seg;
        hdl_desc->des_src_cnt = 1;
        hdl_desc->isroot = true;

        /*As the general semantics, there might multiple pairs of send/recv
         *on the topology tree*/
        for (ridx = 0; ridx < my_fanout_read_tree->n_children; ridx++) {
            child_ctl_pointer =
                ctl_structs[my_fanout_read_tree->children_ranks[ridx]];
            hdl->endpoint->remote_ctrl = child_ctl_pointer;
            ret = hdl->hdl_send(hdl, hdl->endpoint, hdl_desc);
            if (ret !=  OMPI_SUCCESS) {
                BASESMUMA_VERBOSE(1, ("send eror on rank %d ........", my_rank));
                goto exit_ERROR;
            }
        }
    }else if(LEAF_NODE == my_fanout_read_tree->my_node_type) {
        input_args->result_in_rbuf = false;
        /*
         * Get parent payload data and control data.
         * Get the pointer to the base address of the parent's payload buffer.
         * Get the parent's control buffer.
         */
        parent_ctl_pointer = ctl_structs[my_fanout_parent];

        hdl_desc->des_dst = hdl_seg;
        hdl_desc->des_dst_cnt = 1;
        hdl_desc->isroot = false;
        hdl->endpoint->remote_ctrl = parent_ctl_pointer;

#if __TEST_BLOCKING__
        ret = hdl->hdl_recv(hdl, hdl->endpoint, hdl_desc);
#else
        ret = hdl->hdl_recvi(hdl, hdl->endpoint, NULL, 0, 0, &hdl_desc);
#endif

#if __TEST_WAIT__
        ret = hdl->hdl_wait(hdl, hdl->endpoint, hdl_desc);
        BASESMUMA_VERBOSE(1,("wait on rank %d is done!", my_rank));
#endif
        if (OMPI_SUCCESS != ret) {
            BASESMUMA_VERBOSE(1, ("recvi eror on rank %d ........", my_rank));
            goto exit_ERROR;
        }

        status = false;
#if __TEST_TEST__
        while (!status) {
            hdl->hdl_test(&hdl_desc, &completed, &status);
            opal_progress();
            BASESMUMA_VERBOSE(1, ("test on rank %d ........", my_rank));
        }
#endif

        goto Release;

    }else{
        input_args->result_in_rbuf = false;
        /* Interior node */

        /* Get parent payload data and control data */
        parent_ctl_pointer = ctl_structs[my_fanout_parent];

        hdl_desc->des_dst = hdl_seg;
        hdl_desc->des_dst_cnt = 1;
        hdl_desc->isroot = false;

        hdl->endpoint->remote_ctrl = parent_ctl_pointer;

        ret = hdl->hdl_recv(hdl, hdl->endpoint, hdl_desc);
        if (OMPI_SUCCESS != ret) {
            goto exit_ERROR;
        }
        if (OMPI_SUCCESS != ret) {
            BASESMUMA_VERBOSE(1, ("recvi eror on rank %d ........", my_rank));
            goto exit_ERROR;
        }

        /* Signal to children that they may read the data from my shared buffer */
        opal_atomic_wmb ();
        hdl_desc->des_src = hdl_seg;
        hdl_desc->des_src_cnt = 1;
        for (ridx = 0; ridx < my_fanout_read_tree->n_children; ridx++) {
            child_ctl_pointer =
                ctl_structs[my_fanout_read_tree->children_ranks[ridx]];
            hdl->endpoint->remote_ctrl = child_ctl_pointer;

            ret = hdl->hdl_send(hdl, hdl->endpoint, hdl_desc);
            if (ret !=  OMPI_SUCCESS) {
                BASESMUMA_VERBOSE(1, ("send eror on rank %d ........", my_rank));
                goto exit_ERROR;
            }
        }
        goto Release;
    }

 Release:
    /* if I am the last instance of a basesmuma function in this collectie,
     *   release the resrouces */
    if (IS_LAST_BCOL_FUNC(c_input_args)) {
        rc = bcol_basesmuma_free_buff(
                                      &(bcol_module->colls_with_user_data),
                                      sequence_number);
    }

    my_ctl_pointer->starting_flag_value += 1;

    return BCOL_FN_COMPLETE;
 exit_ERROR:
    return OMPI_ERROR;
}
#endif
