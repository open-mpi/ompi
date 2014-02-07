/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/include/ompi/constants.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/mca/bcol/basesmuma/bcol_basesmuma.h"

/*
#define IS_BARRIER_READY(peer, my_flag, my_sequence_number)\
    (((peer)->sequence_number == (my_sequence_number) && \
      (peer)->flags[BARRIER_RKING_FLAG][bcol_id] >= (my_flag) \
     )? true : false )
*/

#define CALC_ACTIVE_REQUESTS(active_requests,peers, tree_order) \
do{                                                             \
    for( j = 0; j < (tree_order - 1); j++){                     \
       if( 0 > peers[j] ) {                                     \
           /* set the bit */                                    \
           *active_requests ^= (1<<j);                          \
       }                                                        \
    }                                                           \
}while(0)                                       



/*
 * Recursive K-ing barrier
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
int bcol_basesmuma_k_nomial_barrier_init(bcol_function_args_t *input_args,
                struct mca_bcol_base_function_t *const_args)
{
    /* local variables */
    int flag_offset = 0;
    volatile int8_t ready_flag;
    mca_bcol_basesmuma_module_t *bcol_module = (mca_bcol_basesmuma_module_t *) const_args->bcol_module;
    netpatterns_k_exchange_node_t *exchange_node = &bcol_module->knomial_allgather_tree;
    mca_bcol_basesmuma_component_t *cm = &mca_bcol_basesmuma_component;
    uint32_t buffer_index = input_args->buffer_index;
    int *active_requests =
        &(bcol_module->ml_mem.nb_coll_desc[buffer_index].active_requests);

    int *iteration = &bcol_module->ml_mem.nb_coll_desc[buffer_index].iteration;
    int *status = &bcol_module->ml_mem.nb_coll_desc[buffer_index].status;
    int leading_dim, buff_idx, idx;
    int bcol_id = (int) bcol_module->super.bcol_id;

    int i, j, probe;
    int src;

    int pow_k, tree_order;
    int max_requests = 0; /* important to initialize this */

    int matched = 0;
    int64_t sequence_number=input_args->sequence_num;
    int my_rank = bcol_module->super.sbgp_partner_module->my_index;
    
    volatile mca_bcol_basesmuma_payload_t *data_buffs; 

    /* control structures */
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    volatile mca_bcol_basesmuma_header_t *peer_ctl_pointer;
#if 0 
    fprintf(stderr,"entering sm barrier sn = %d buff index = %d\n",sequence_number,input_args->buffer_index);
#endif
    /* initialize the iteration counter */
    buff_idx = input_args->buffer_index; 
    leading_dim = bcol_module->colls_no_user_data.size_of_group; 
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);
    data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;
    /* Set pointer to current proc ctrl region */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;

    /* init the header */
    BASESMUMA_HEADER_INIT(my_ctl_pointer, ready_flag, sequence_number, bcol_id);
    /* initialize these */
    *iteration = 0;
    *active_requests = 0;
    *status = 0;
    
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
        max_requests ^=  (1<<i);
    }
    /* let's begin the collective, starting with extra ranks and their
     * respective proxies
     */

    if( EXTRA_NODE == exchange_node->node_type ) {

        /* then I will signal to my proxy rank*/

        my_ctl_pointer->flags[BARRIER_RKING_FLAG][bcol_id] = ready_flag;
        ready_flag = flag_offset + 1 + pow_k + 2;
        /* now, poll for completion */

        src = exchange_node->rank_extra_sources_array[0];
        peer_ctl_pointer = data_buffs[src].ctl_struct;

        for( i = 0; i < cm->num_to_probe && (0 == matched); i++ ) {
            if(IS_PEER_READY(peer_ctl_pointer, ready_flag, sequence_number, BARRIER_RKING_FLAG, bcol_id)){
                matched = 1;
                
                goto FINISHED;
            } 

        }

        /* cache state and bail */
        *iteration = -1;
        return BCOL_FN_STARTED;

    }else if ( 0 < exchange_node->n_extra_sources ) {

        /* I am a proxy for someone */
        src = exchange_node->rank_extra_sources_array[0];
        peer_ctl_pointer = data_buffs[src].ctl_struct;
        
        /* probe for extra rank's arrival */
        for( i = 0; i < cm->num_to_probe && ( 0 == matched); i++) {
            if(IS_PEER_READY(peer_ctl_pointer,ready_flag, sequence_number, BARRIER_RKING_FLAG, bcol_id)){
                matched = 1;
                /* copy it in */
                goto MAIN_PHASE;
            } 
        }
        *status = ready_flag;
        *iteration = -1;
        return BCOL_FN_STARTED;

    }

MAIN_PHASE:
    /* bump the ready flag */
    ready_flag++;

    /* we start the recursive k - ing phase */
    for( *iteration = 0; *iteration < pow_k; (*iteration)++) {
        /* announce my arrival */
        my_ctl_pointer->flags[BARRIER_RKING_FLAG][bcol_id] = ready_flag;
        /* calculate the number of active requests */
        CALC_ACTIVE_REQUESTS(active_requests,exchange_node->rank_exchanges[*iteration],tree_order);
        /* Now post the recv's */
        for( j = 0; j < (tree_order - 1); j++ ) {
            
            /* recv phase */
            src = exchange_node->rank_exchanges[*iteration][j];
            if( src < 0 ) {
                /* then not a valid rank, continue */ 
                continue;
            }

            peer_ctl_pointer = data_buffs[src].ctl_struct;
            if( !(*active_requests&(1<<j))) {
               /* then the bit hasn't been set, thus this peer 
                * hasn't been processed at this level 
                * I am putting the probe loop as the inner most loop to achieve
                * better temporal locality, this comes at a cost to asynchronicity 
                * but should get better cache performance 
                */
                matched = 0;
                for( probe = 0; probe < cm->num_to_probe && (0 == matched); probe++){
                    if(IS_PEER_READY(peer_ctl_pointer,ready_flag, sequence_number, BARRIER_RKING_FLAG, bcol_id)){
                        matched = 1;
                        /* set this request's bit */
                        *active_requests ^= (1<<j);
                    }
                }
            } 


        }
        if( max_requests == *active_requests ){
            /* bump the ready flag */
            ready_flag++;
            /*reset the active requests */
            *active_requests = 0;
        } else {
            /* cache the state and hop out
             * only the iteration needs to be tracked
             */
            *status = my_ctl_pointer->flags[BARRIER_RKING_FLAG][bcol_id];
            return BCOL_FN_STARTED;
        }
    }

    /* bump the flag one more time for the extra rank */
    ready_flag = flag_offset + 1 + pow_k + 2;

    /* finish off the last piece, send the data back to the extra  */
    if( 0 < exchange_node->n_extra_sources ) {
        /* simply announce my arrival */
        my_ctl_pointer->flags[BARRIER_RKING_FLAG][bcol_id] = ready_flag;

    }

FINISHED:


    my_ctl_pointer->starting_flag_value[bcol_id]++;
    return BCOL_FN_COMPLETE;
}


/* allgather progress function */

int bcol_basesmuma_k_nomial_barrier_progress(bcol_function_args_t *input_args,
                        struct mca_bcol_base_function_t *const_args)
{


    /* local variables */
    int flag_offset;
    volatile int8_t ready_flag;
    mca_bcol_basesmuma_module_t *bcol_module = (mca_bcol_basesmuma_module_t *) const_args->bcol_module;
    netpatterns_k_exchange_node_t *exchange_node = &bcol_module->knomial_allgather_tree;
    mca_bcol_basesmuma_component_t *cm = &mca_bcol_basesmuma_component;
    uint32_t buffer_index = input_args->buffer_index;
    int *active_requests =
        &(bcol_module->ml_mem.nb_coll_desc[buffer_index].active_requests);

    int *iteration = &bcol_module->ml_mem.nb_coll_desc[buffer_index].iteration;
    int *status = &bcol_module->ml_mem.nb_coll_desc[buffer_index].status;
    int *iter = iteration; /* double alias */
    int leading_dim, idx, buff_idx;

    int i, j, probe;
    int src;
    int max_requests = 0; /* critical to set this */
    int pow_k, tree_order;
    int bcol_id = (int) bcol_module->super.bcol_id;
    
    int matched = 0;
    int64_t sequence_number=input_args->sequence_num;
    int my_rank = bcol_module->super.sbgp_partner_module->my_index;

    volatile mca_bcol_basesmuma_payload_t *data_buffs;

    /* control structures */
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    volatile mca_bcol_basesmuma_header_t *peer_ctl_pointer;
#if 0 
    fprintf(stderr,"%d: entering sm allgather progress active requests %d iter %d ready_flag %d\n",my_rank,
            *active_requests,*iter,*status);
#endif
    buff_idx = buffer_index; 
    leading_dim=bcol_module->colls_no_user_data.size_of_group; 
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);
  
    data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
                bcol_module->colls_with_user_data.data_buffs+idx;
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;
    
    /* increment the starting flag by one and return */
    flag_offset = my_ctl_pointer->starting_flag_value[bcol_id];
    ready_flag = *status;
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

    if( EXTRA_NODE == exchange_node->node_type ) {

        /* If I'm in here, then I must be looking for data */
        ready_flag = flag_offset + 1 + pow_k + 2;

        src = exchange_node->rank_extra_sources_array[0];
        peer_ctl_pointer = data_buffs[src].ctl_struct;

        for( i = 0; i < cm->num_to_probe && (0 == matched); i++ ) {
            if(IS_PEER_READY(peer_ctl_pointer, ready_flag, sequence_number, BARRIER_RKING_FLAG, bcol_id)){
                matched = 1;
                
                goto FINISHED;
            } 

        }

        /* haven't found it, state is cached, bail out */
        return BCOL_FN_STARTED;

    }else if ( ( -1 == *iteration ) && (0 < exchange_node->n_extra_sources) ) {

        /* I am a proxy for someone */
        src = exchange_node->rank_extra_sources_array[0];
        peer_ctl_pointer = data_buffs[src].ctl_struct;
        
        /* probe for extra rank's arrival */
        for( i = 0; i < cm->num_to_probe && ( 0 == matched); i++) {
            if(IS_PEER_READY(peer_ctl_pointer,ready_flag, sequence_number, BARRIER_RKING_FLAG, bcol_id)){
                matched = 1;
                /* bump the flag */
                ready_flag++;
                *iteration = 0;
                goto MAIN_PHASE;
            } 
        }
        return BCOL_FN_STARTED;

    }

MAIN_PHASE:

    /* start the recursive k - ing phase */
    for( *iter=*iteration; *iter < pow_k; (*iter)++) {
        /* I am ready at this level */
        my_ctl_pointer->flags[BARRIER_RKING_FLAG][bcol_id] = ready_flag;
        if( 0 == *active_requests ) {
            /* flip some bits, if we don't have active requests from a previous visit */
            CALC_ACTIVE_REQUESTS(active_requests,exchange_node->rank_exchanges[*iter],tree_order);
        }
        for( j = 0; j < (tree_order - 1); j++ ) {
            
            /* recv phase */
            src = exchange_node->rank_exchanges[*iter][j];
            if( src < 0 ) {
                /* then not a valid rank, continue  
                 */
                continue;
            }

            peer_ctl_pointer = data_buffs[src].ctl_struct;
            if( !(*active_requests&(1<<j))){

                /* I am putting the probe loop as the inner most loop to achieve
                 * better temporal locality 
                 */
                matched = 0;
                for( probe = 0; probe < cm->num_to_probe && (0 == matched); probe++){
                    if(IS_PEER_READY(peer_ctl_pointer,ready_flag, sequence_number, BARRIER_RKING_FLAG, bcol_id)){
                        matched = 1;
                        /* flip the request's bit */
                        *active_requests ^= (1<<j);
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
            *status = my_ctl_pointer->flags[BARRIER_RKING_FLAG][bcol_id];
            return BCOL_FN_STARTED;
        }
    }
    /* bump the flag one more time for the extra rank */
    ready_flag = flag_offset + 1 + pow_k + 2;

    /* finish off the last piece, send the data back to the extra  */
    if( 0 < exchange_node->n_extra_sources ) {
        /* simply announce my arrival */
        my_ctl_pointer->flags[BARRIER_RKING_FLAG][bcol_id] = ready_flag;

    }

FINISHED:
  
    my_ctl_pointer->starting_flag_value[bcol_id]++;
    return BCOL_FN_COMPLETE;
}

/* Register k-nomial barrier functions to the BCOL function table,
 * so they can be selected
 */
int bcol_basesmuma_barrier_init(mca_bcol_base_module_t *super)
{
mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    comm_attribs.bcoll_type = BCOL_BARRIER;
    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1024 * 1024;
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000; /* range 1 */

    inv_attribs.datatype_bitmap = 0xffffffff;
    inv_attribs.op_types_bitmap = 0xffffffff;

    comm_attribs.data_src = DATA_SRC_KNOWN;

    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                bcol_basesmuma_k_nomial_barrier_init,
                bcol_basesmuma_k_nomial_barrier_progress);

    return OMPI_SUCCESS;
}

