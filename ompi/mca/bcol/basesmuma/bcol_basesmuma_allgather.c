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

#include "ompi/include/ompi/constants.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/basesmuma/bcol_basesmuma.h"
/*
  #define IS_AGDATA_READY(peer, my_flag, my_sequence_number)\
  (((peer)->sequence_number == (my_sequence_number) && \
  (peer)->flags[ALLGATHER_FLAG][bcol_id] >= (my_flag) \
  )? true : false )
*/

#define CALC_ACTIVE_REQUESTS(active_requests,peers, tree_order) \
    do{                                                         \
        for( j = 0; j < (tree_order - 1); j++){                 \
            if( 0 > peers[j] ) {                                \
                /* set the bit */                               \
                *active_requests ^= (1<<j);                     \
            }                                                   \
        }                                                       \
    }while(0)



/*
 * Recursive K-ing allgather
 */

/*
 *
 * Recurssive k-ing algorithm
 * Example k=3 n=9
 *
 *
 * Number of Exchange steps = log (basek) n
 * Number of steps in exchange step = k (radix)
 *
 */
int bcol_basesmuma_k_nomial_allgather_init(bcol_function_args_t *input_args,
                                           struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_basesmuma_module_t *bcol_module = (mca_bcol_basesmuma_module_t *) const_args->bcol_module;
    netpatterns_k_exchange_node_t *exchange_node = &bcol_module->knomial_allgather_tree;
    int bcol_id = (int) bcol_module->super.bcol_id;
    uint32_t buffer_index = input_args->buffer_index;
    int *active_requests =
        &(bcol_module->ml_mem.nb_coll_desc[buffer_index].active_requests);

    int *iteration = &bcol_module->ml_mem.nb_coll_desc[buffer_index].iteration;
    int *status = &bcol_module->ml_mem.nb_coll_desc[buffer_index].status;
    int leading_dim, buff_idx, idx;

    int64_t sequence_number = input_args->sequence_num;
    int my_rank = bcol_module->super.sbgp_partner_module->my_index;

    volatile mca_bcol_basesmuma_payload_t *data_buffs;

    /* control structures */
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;

    volatile int8_t ready_flag;

    /* initialize the iteration counter */
    buff_idx = input_args->src_desc->buffer_index;
    leading_dim = bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);
    data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;

    /* Set pointer to current proc ctrl region */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;

    /* initialize headers and ready flag */
    BASESMUMA_HEADER_INIT(my_ctl_pointer, ready_flag, sequence_number, bcol_id);

    /* initialize these */
    *iteration = -1;
    *active_requests = 0;
    *status = ready_flag;

    if (EXTRA_NODE == exchange_node->node_type) {
        /* I am ready at this level */
        opal_atomic_wmb ();
        my_ctl_pointer->flags[ALLGATHER_FLAG][bcol_id] = ready_flag;
    }

    return bcol_basesmuma_k_nomial_allgather_progress (input_args, const_args);
}


/* allgather progress function */

int bcol_basesmuma_k_nomial_allgather_progress(bcol_function_args_t *input_args,
                                               struct mca_bcol_base_function_t *const_args)
{
    /* local variables */
    int8_t flag_offset;
    uint32_t buffer_index = input_args->buffer_index;
    volatile int8_t ready_flag;
    mca_bcol_basesmuma_module_t *bcol_module = (mca_bcol_basesmuma_module_t *) const_args->bcol_module;
    netpatterns_k_exchange_node_t *exchange_node = &bcol_module->knomial_allgather_tree;
    int group_size = bcol_module->colls_no_user_data.size_of_group;
    int *list_connected = bcol_module->super.list_n_connected; /* critical for hierarchical colls */
    int bcol_id = (int) bcol_module->super.bcol_id;
    mca_bcol_basesmuma_component_t *cm = &mca_bcol_basesmuma_component;
    int *active_requests =
        &(bcol_module->ml_mem.nb_coll_desc[buffer_index].active_requests);

    int *iteration = &bcol_module->ml_mem.nb_coll_desc[buffer_index].iteration;
    int *status = &bcol_module->ml_mem.nb_coll_desc[buffer_index].status;
    int leading_dim, idx, buff_idx;

    int i, j, probe;
    int knt;
    int src;
    int recv_offset, recv_len;
    int max_requests = 0; /* critical to set this */
    int pow_k, tree_order;

    int64_t sequence_number=input_args->sequence_num;
    int my_rank = bcol_module->super.sbgp_partner_module->my_index;

    int pack_len = input_args->count * input_args->dtype->super.size;

    void *data_addr = (void*)(
        (unsigned char *) input_args->sbuf +
        (size_t) input_args->sbuf_offset);
    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    volatile char *peer_data_pointer;

    /* control structures */
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    volatile mca_bcol_basesmuma_header_t *peer_ctl_pointer;

#if 0
    fprintf(stderr,"%d: entering sm allgather progress active requests %d iter %d ready_flag %d\n", my_rank,
            *active_requests, *iteration, *status);
#endif

    buff_idx = input_args->src_desc->buffer_index;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);
    data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;

    /* Set pointer to current proc ctrl region */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;

    /* increment the starting flag by one and return */
    /* flag offset seems unnecessary here */
    flag_offset = my_ctl_pointer->starting_flag_value[bcol_id];
    ready_flag = *status;
    my_ctl_pointer->sequence_number = sequence_number;
    /* k-nomial parameters */
    tree_order = exchange_node->tree_order;
    pow_k = exchange_node->log_tree_order;

    /* calculate the maximum number of requests
     * at each level each rank communicates with
     * at most (k - 1) peers
     * so if we set k - 1 bit fields in "max_requests", then
     * we have max_request  == 2^(k - 1) -1
     */
    for(i = 0; i < (tree_order - 1); i++){
        max_requests ^= (1<<i);
    }

    /* let's begin the collective, starting with extra ranks and their
     * respective proxies
     */

    if (OPAL_UNLIKELY(-1 == *iteration)) {
        if (EXTRA_NODE == exchange_node->node_type) {
            /* If I'm in here, then I must be looking for data */
            ready_flag = flag_offset + 1 + pow_k + 2;

            src = exchange_node->rank_extra_sources_array[0];
            peer_data_pointer = data_buffs[src].payload;
            peer_ctl_pointer = data_buffs[src].ctl_struct;

            /* calculate the count */
            for (i = 0, knt = 0 ; i < group_size ; ++i){
                knt += list_connected[i];
            }

            for (i = 0 ; i < cm->num_to_probe ; ++i) {
                if (IS_PEER_READY(peer_ctl_pointer, ready_flag, sequence_number, ALLGATHER_FLAG, bcol_id)) {
                    /* we receive the entire message */
                    opal_atomic_mb ();
                    memcpy (data_addr, (void *) peer_data_pointer, knt * pack_len);

                    goto FINISHED;
                }
            }

            /* haven't found it, state is saved, bail out */
            return BCOL_FN_STARTED;
        } else if (0 < exchange_node->n_extra_sources) {
            /* I am a proxy for someone */
            src = exchange_node->rank_extra_sources_array[0];
            peer_data_pointer = data_buffs[src].payload;
            peer_ctl_pointer = data_buffs[src].ctl_struct;

            /* calculate the offset */
            for (i = 0, knt = 0 ; i < src ; ++i){
                knt += list_connected[i];
            }

            /* probe for extra rank's arrival */
            for (i = 0 ; i < cm->num_to_probe ; ++i) {
                if (IS_PEER_READY(peer_ctl_pointer, ready_flag, sequence_number, ALLGATHER_FLAG, bcol_id)) {
                    opal_atomic_mb ();
                    /* copy it in */
                    memcpy ((void *) ((uintptr_t) data_addr + knt * pack_len),
                            (void *) ((uintptr_t) peer_data_pointer + knt * pack_len),
                            pack_len * list_connected[src]);
                    break;
                }
            }

            if (i == cm->num_to_probe) {
                return BCOL_FN_STARTED;
            }
        }

        /* bump the ready flag to indicate extra node exchange complete */
        ++ready_flag;
        *iteration = 0;
    }

    /* start the recursive k - ing phase */
    for (i = *iteration ; i < pow_k ; ++i) {
        /* I am ready at this level */
        opal_atomic_wmb ();
        my_ctl_pointer->flags[ALLGATHER_FLAG][bcol_id] = ready_flag;

        if (0 == *active_requests) {
            /* flip some bits, if we don't have active requests from a previous visit */
            CALC_ACTIVE_REQUESTS(active_requests,exchange_node->rank_exchanges[i],tree_order);
        }

        for (j = 0; j < (tree_order - 1); ++j) {

            /* recv phase */
            src = exchange_node->rank_exchanges[i][j];

            if (src < 0) {
                /* then not a valid rank, continue */
                continue;
            }

            if (!(*active_requests&(1<<j))) {
                /* then this peer hasn't been processed at this level */
                peer_data_pointer = data_buffs[src].payload;
                peer_ctl_pointer = data_buffs[src].ctl_struct;

                recv_offset = exchange_node->payload_info[i][j].r_offset * pack_len;
                recv_len = exchange_node->payload_info[i][j].r_len * pack_len;

                /* I am putting the probe loop as the inner most loop to achieve
                 * better temporal locality
                 */
                for (probe = 0 ; probe < cm->num_to_probe ; ++probe) {
                    if (IS_PEER_READY(peer_ctl_pointer, ready_flag, sequence_number, ALLGATHER_FLAG, bcol_id)) {
                        /* flip the request's bit */
                        *active_requests ^= (1<<j);
                        /* copy the data */
                        memcpy((void *)((unsigned char *) data_addr + recv_offset),
                               (void *)((unsigned char *) peer_data_pointer + recv_offset),
                               recv_len);
                        break;
                    }
                }
            }
        }

        if( max_requests == *active_requests ){
            /* bump the ready flag */
            ready_flag++;
            /* reset the active requests for the next level */
            *active_requests = 0;
            /* calculate the number of active requests
             * logically makes sense to do it here. We don't
             * want to inadvertantly flip a bit to zero that we
             * set previously
             */
        } else {
            /* state is saved hop out
             */
            *status = my_ctl_pointer->flags[ALLGATHER_FLAG][bcol_id];
            *iteration = i;
            return BCOL_FN_STARTED;
        }
    }

    /* bump the flag one more time for the extra rank */
    ready_flag = flag_offset + 1 + pow_k + 2;

    /* finish off the last piece, send the data back to the extra  */
    if( 0 < exchange_node->n_extra_sources ) {
        /* simply announce my arrival */
        opal_atomic_wmb ();
        my_ctl_pointer->flags[ALLGATHER_FLAG][bcol_id] = ready_flag;
    }

FINISHED:
    /* bump this up for others to see */
    my_ctl_pointer->starting_flag_value[bcol_id]++;
    return BCOL_FN_COMPLETE;
}

/* Register allreduce functions to the BCOL function table,
 * so they can be selected
 */
int bcol_basesmuma_allgather_init(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    comm_attribs.bcoll_type = BCOL_ALLGATHER;
    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1024 * 1024;
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000; /* range 1 */

    inv_attribs.datatype_bitmap = 0xffffffff;
    inv_attribs.op_types_bitmap = 0xffffffff;

    comm_attribs.data_src = DATA_SRC_KNOWN;

    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_basesmuma_k_nomial_allgather_init,
                                 bcol_basesmuma_k_nomial_allgather_progress);

    return OMPI_SUCCESS;
}
