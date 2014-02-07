/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Recursive doubling blocking barrier */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/patterns/net/netpatterns.h"

#include "opal/sys/atomic.h"

#include "ompi/mca/bcol/base/base.h"
#include "bcol_basesmuma.h"

/***********************************************************************************/
/*********************************** New Fan-Out ***********************************/
/***********************************************************************************/

static int bcol_basesmuma_fanout_new(
                bcol_function_args_t *input_args,
                mca_bcol_base_function_t *c_input_args)
{
    /* local variables */
    int64_t sequence_number;

    mca_bcol_basesmuma_module_t* bcol_module =
        (mca_bcol_basesmuma_module_t *) c_input_args->bcol_module;

    int idx, probe, 
        my_rank = bcol_module->super.sbgp_partner_module->my_index,
        leading_dim = bcol_module->colls_no_user_data.size_of_group;
    int8_t  ready_flag;
    int8_t bcol_id = (int8_t) bcol_module->super.bcol_id;
    int buff_index = input_args->buffer_index;
    mca_bcol_basesmuma_component_t *cm = &mca_bcol_basesmuma_component;

    
    volatile mca_bcol_basesmuma_payload_t *ctl_structs;

    /* control structures */
    volatile mca_bcol_basesmuma_header_t *my_ctl;
    volatile mca_bcol_basesmuma_header_t *parent_ctl;


    netpatterns_tree_node_t *my_tree_node = &(bcol_module->fanin_node);

    /* Figure out - what instance of the basesmuma bcol I am */
    sequence_number = input_args->sequence_num;

    idx = SM_ARRAY_INDEX(leading_dim, buff_index, 0);
    ctl_structs = (volatile mca_bcol_basesmuma_payload_t *)
                        bcol_module->colls_with_user_data.data_buffs + idx;
    my_ctl = ctl_structs[my_rank].ctl_struct;

    /* init the header */
    BASESMUMA_HEADER_INIT(my_ctl, ready_flag, sequence_number, bcol_id);
    
    /* Wait on my parent to arrive */
    if (my_tree_node->n_parents) {
        parent_ctl = ctl_structs[my_tree_node->parent_rank].ctl_struct;
        for( probe = 0; probe < cm->num_to_probe; probe++){
           if (IS_PEER_READY(parent_ctl, ready_flag, sequence_number, BARRIER_FANOUT_FLAG, bcol_id)) {
              /* signal my children */
               my_ctl->flags[BARRIER_FANOUT_FLAG][bcol_id] = ready_flag;
               /* bump the starting flag */
               my_ctl->starting_flag_value[bcol_id]++;
               return BCOL_FN_COMPLETE;

            }
        }

    } else {
        /* I am the root of the fanout */
        my_ctl->flags[BARRIER_FANOUT_FLAG][bcol_id] = ready_flag;
        /* bump the starting flag */
        my_ctl->starting_flag_value[bcol_id]++;
        return BCOL_FN_COMPLETE;
    }





    return BCOL_FN_STARTED;
}

int bcol_basesmuma_fanout_init(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    BASESMUMA_VERBOSE(10, ("Basesmuma Fan-Out register.\n"));

    comm_attribs.bcoll_type = BCOL_FANOUT;

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
        bcol_basesmuma_fanout_new,
        bcol_basesmuma_fanout_new);

    return OMPI_SUCCESS;
}
