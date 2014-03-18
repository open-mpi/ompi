/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
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
#include "ompi/mca/bcol/base/base.h"
#include "ompi/mca/bcol/basesmuma/bcol_basesmuma.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"

/* debug
 *   #include "opal/sys/timer.h"
 *
 *   extern uint64_t timers[7];
 *   end debug */

/* debug */
#include <unistd.h>
/* end debug */

/* non-blocking gather routines: init and progress functions */
int bcol_basesmuma_gather_init(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    comm_attribs.bcoll_type = BCOL_GATHER;
    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1048576;
    comm_attribs.data_src = DATA_SRC_KNOWN;
    comm_attribs.waiting_semantics = BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000;
    inv_attribs.datatype_bitmap = 0x11111111;
    inv_attribs.op_types_bitmap = 0x11111111;

    /* Set attributes for fanin fanout algorithm */
    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_basesmuma_k_nomial_gather_init,
                                 bcol_basesmuma_k_nomial_gather_progress);

    return OMPI_SUCCESS;
}

int bcol_basesmuma_k_nomial_gather_init(bcol_function_args_t *input_args,
                                        mca_bcol_base_function_t *c_input_args)
{
    /* local variables */
    int leading_dim, buff_idx, idx;
    int src, i, j, k_temp1, k_temp2;
    int pseudo_root, proxy_root, pseudo_base_adj;
    volatile int8_t ready_flag;
    int count=input_args->count;
    struct ompi_datatype_t* dtype=input_args->dtype;
    int root=input_args->root;
    int base_adj, base;
    int total_peers, my_pow_k=0;
    int64_t sequence_number=input_args->sequence_num;
    mca_bcol_basesmuma_module_t* bcol_module=
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
    int bcol_id = (int) bcol_module->super.bcol_id;
    int my_rank = bcol_module->super.sbgp_partner_module->my_index;
    netpatterns_k_exchange_node_t *exchange_node =
        &bcol_module->knomial_allgather_tree;
    uint32_t buffer_index = input_args->buffer_index;
    int *active_requests =
        &(bcol_module->ml_mem.nb_coll_desc[buffer_index].active_requests);

    int *iteration = &bcol_module->ml_mem.nb_coll_desc[buffer_index].iteration;
    int *status = &bcol_module->ml_mem.nb_coll_desc[buffer_index].status;

    int buff_offset = bcol_module->super.hier_scather_offset;

    /* "indirectors" */
    int *inv_map = exchange_node->inv_reindex_map;
    int *reindex_map = exchange_node->reindex_map;
    int stray = exchange_node->k_nomial_stray;

    /* tree radix */
    int tree_order = exchange_node->tree_order;
    /* tree depth */
    int pow_k =  exchange_node->log_tree_order;
    /* largest power of k less than or equal to np */
    int cnt = exchange_node->n_largest_pow_tree_order;

    /* payload structures */
    volatile mca_bcol_basesmuma_payload_t *data_buffs;

    /* control structures */
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;

    size_t pack_len = 0, dt_size;

#if 0
    fprintf(stderr,"Entering sm gather input_args->sbuf_offset %d \n",input_args->sbuf_offset);
    fflush(stderr);
#endif


    /* we will work only on packed data - so compute the length*/
    /* this is the size of my data, this is not gatherv so it's the same
     * for all ranks in the communicator.
     */
    ompi_datatype_type_size(dtype, &dt_size);
    pack_len=count*dt_size;
    /* now set the "real" offset */
    buff_offset = buff_offset*pack_len;

    buff_idx = input_args->src_desc->buffer_index;

    /* Get addressing information */
    my_rank = bcol_module->super.sbgp_partner_module->my_index;

    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);
    data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;

    /* Set pointer to current proc ctrl region */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;

    /* init the header */
    BASESMUMA_HEADER_INIT(my_ctl_pointer, ready_flag, sequence_number, bcol_id);

    /* init active requests, iteration, and status */
    *iteration = 0;
    *active_requests = 0;
    *status = -1;
    /* calculate the number of steps necessary for this collective */

    /* first thing we do is figure out where the root is in our new indexing */
    /* find root in new indexing */
    pseudo_root = inv_map[root];
    /* see if this is larger than the stray */
    if (pseudo_root >= stray) {
        /* then we need to define the proxy root, everyone can do this */
        proxy_root = pseudo_root - cnt;
    } else {
        proxy_root = pseudo_root;
    }

    /* do some figuring */
    if (EXCHANGE_NODE == exchange_node->node_type) {
        total_peers = 0;
        my_pow_k = pow_k;
        k_temp1 = tree_order;
        k_temp2 = 1;
        for( i = 0; i < pow_k; i++) {
            /* then find the base */
            FIND_BASE(base,exchange_node->reindex_myid,i+1,tree_order);
            /* now find the adjusted base */
            base_adj = base + (base + proxy_root)%k_temp1;
            /* ok, now find out WHO is occupying this slot */
            pseudo_base_adj = reindex_map[base_adj];

            if(my_rank == pseudo_base_adj ) {
                /* then go ahead and poll for children's data */
                for( j = 0; j < (tree_order - 1); j++ ) {
                    /* send phase
                     */
                    /* get communication partner */

                    src = exchange_node->rank_exchanges[i][j];
                    /* remember, if we have extra ranks, then we won't participate
                     * with a least one peer. Make a check
                     */
                    if( src < 0 ){
                        continue;
                    }else{

                        /* flip a bit to represent this request */
                        *active_requests ^= (1<<total_peers++);
                    }


                }
            } else {
                /* announce my arrival */
                my_pow_k = i;
                break;
            }

            k_temp1 = k_temp1*tree_order;
            k_temp2 = k_temp2*tree_order;
        }
    }

    *iteration = my_pow_k;

    if (EXTRA_NODE == exchange_node->node_type || 0 == exchange_node->n_extra_sources) {
        if (0 == my_pow_k || EXTRA_NODE == exchange_node->node_type) {
            opal_atomic_rmb ();

            my_ctl_pointer->flags[GATHER_FLAG][bcol_id] = ready_flag;
        }

        if ((EXTRA_NODE == exchange_node->node_type && root != my_rank) || 0 == my_pow_k) {
            /* nothing more to do */
            my_ctl_pointer->starting_flag_value[bcol_id]++;

            return BCOL_FN_COMPLETE;
        }
    }

    return BCOL_FN_STARTED;
}


int bcol_basesmuma_k_nomial_gather_progress(bcol_function_args_t *input_args,
                                            mca_bcol_base_function_t *c_input_args)
{
    /* local variables */
    int group_size;
    int flag_offset;
    int leading_dim, buff_idx, idx;
    int src, knt, i, j, k_temp1, k_temp2;
    volatile int8_t ready_flag;
    int count=input_args->count;
    struct ompi_datatype_t* dtype=input_args->dtype;
    int root=input_args->root;
    int probe;
    int matched;
    int64_t sequence_number=input_args->sequence_num;
    mca_bcol_basesmuma_module_t* bcol_module=
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
    int bcol_id = (int) bcol_module->super.bcol_id;
    int my_rank = bcol_module->super.sbgp_partner_module->my_index;
    mca_bcol_basesmuma_component_t *cm = &mca_bcol_basesmuma_component;
    netpatterns_k_exchange_node_t *exchange_node =
        &bcol_module->knomial_allgather_tree;
    uint32_t buffer_index = input_args->buffer_index;
    int *active_requests =
        &(bcol_module->ml_mem.nb_coll_desc[buffer_index].active_requests);
    int *iteration = &bcol_module->ml_mem.nb_coll_desc[buffer_index].iteration;
    int *status = &bcol_module->ml_mem.nb_coll_desc[buffer_index].status;
    int buff_offset = bcol_module->super.hier_scather_offset;
    /* "indirectors" */
    int *list_connected = bcol_module->super.list_n_connected;
    /* tree radix */
    int tree_order = exchange_node->tree_order;
    /* payload structures */
    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    volatile char *child_data_pointer;
    /* control structures */
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    volatile mca_bcol_basesmuma_header_t *child_ctl_pointer;
    /*volatile mca_bcol_basesmuma_ctl_struct_t* parent_ctl_pointer; */

    size_t pack_len = 0, dt_size;
    void *data_addr = (void *)((unsigned char *)input_args->src_desc->data_addr);


#if 0
    fprintf(stderr,"Entering sm gather input_args->sbuf_offset %d \n",input_args->sbuf_offset);
    fflush(stderr);
#endif


    /* we will work only on packed data - so compute the length*/
    /* this is the size of my data, this is not gatherv so it's the same
     * for all ranks in the communicator.
     */
    ompi_datatype_type_size(dtype, &dt_size);
    pack_len=count*dt_size;
    /* now set the "real" offset */
    buff_offset = buff_offset*pack_len;

    buff_idx = input_args->src_desc->buffer_index;

    /* Get addressing information */
    my_rank = bcol_module->super.sbgp_partner_module->my_index;

    group_size = bcol_module->colls_no_user_data.size_of_group;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);
    data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;

    /* Set pointer to current proc ctrl region */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;
    /* restart the ready_flag state */
    flag_offset = my_ctl_pointer->starting_flag_value[bcol_id];
    ready_flag = flag_offset + 1;

    /* calculate the number of steps necessary for this collective */

    /* first thing we do is figure out where the root is in our new indexing */
    /* find root in new indexing */
    if( EXTRA_NODE == exchange_node->node_type ) {

        /* poll for data from proxy */
        src = exchange_node->rank_extra_sources_array[0];
        /* get src data buffer */
        child_data_pointer = data_buffs[src].payload;
        child_ctl_pointer = data_buffs[src].ctl_struct;
        /* remember to bump your flag */
        ready_flag++;

        /* in this case, you must block */
        for (i = 0 ; i < cm->num_to_probe ; ++i) {
            if (IS_PEER_READY(child_ctl_pointer,ready_flag,sequence_number, GATHER_FLAG, bcol_id)){
                /* receive the data from the proxy, aka pseudo-root */
                memcpy((void *) ((unsigned char *) data_addr + buff_offset),
                       (void *) ((unsigned char *) child_data_pointer+buff_offset),
                       pack_len * group_size);

                goto FINISHED;
            }
        }

        return BCOL_FN_STARTED;
    }


    if (0 < exchange_node->n_extra_sources && (-1 == (*status))) {
        /* am a proxy, poll for pack_len data from extra */
        src = exchange_node->rank_extra_sources_array[0];
        /* get src data buffer */
        child_data_pointer = data_buffs[src].payload;
        child_ctl_pointer = data_buffs[src].ctl_struct;
        knt = 0;
        for( i = 0; i < src; i++){
            knt += list_connected[i];
        }
        /* must block here also */
        matched = 0;
        for (i = 0, matched = 0 ; i < cm->num_to_probe && (0 == matched) ; ++i) {
            if(IS_PEER_READY(child_ctl_pointer,ready_flag,sequence_number, GATHER_FLAG, bcol_id)){
                matched = 1;
                memcpy((void *) ((unsigned char *) data_addr + buff_offset + pack_len*knt),
                       (void *) ((unsigned char *) child_data_pointer + buff_offset +
                                 pack_len*knt), pack_len*list_connected[src]);
                *status = 0;
                if( 0 == *active_requests ){
                    goto LAST_STEP;
                }

                break;
            }
        }
        if( 0 == matched ){
            return BCOL_FN_STARTED;
        }
    }

    /* start the k-nomial gather phase */
    /* only "active ranks participate, once a rank has forwarded its data, it becomes inactive */
    for (probe = 0 ; probe < cm->num_to_probe ; ++probe) {
        k_temp1 = tree_order;
        k_temp2 = 1;
        for (i = 0 ; i < *(iteration) ; ++i) {

            /* then go ahead and poll for children's data */
            for (j = 0 ; j < (tree_order - 1) ; ++j) {
                /* send phase
                 */
                /* get communication partner */

                src = exchange_node->rank_exchanges[i][j];
                /* remember, if we have extra ranks, then we won't participate
                 * with a least one peer. Make a check
                 */
                /* if the bit that corresponds to this child has been set to zero,
                 * then it has already checked in and data received
                 */
                if (src < 0 || 1 != ((*active_requests >> ((tree_order - 1)*i + j))&1)){
                    continue;
                }
                child_data_pointer = data_buffs[src].payload;
                child_ctl_pointer = data_buffs[src].ctl_struct;

                if(IS_PEER_READY(child_ctl_pointer,ready_flag,sequence_number, GATHER_FLAG, bcol_id)){
                    /* copy the data */
                    memcpy((void *) ((unsigned char *) data_addr + buff_offset +
                                     exchange_node->payload_info[i][j].r_offset*pack_len),
                           (void *) ((unsigned char *) child_data_pointer + buff_offset +
                                     exchange_node->payload_info[i][j].r_offset*pack_len),
                           exchange_node->payload_info[i][j].r_len*pack_len);
                    /* flip the bit to zero */
                    *active_requests ^= (1<<((tree_order - 1)*i + j));
                    if(0 == (*active_requests)) {
                        goto LAST_STEP;
                    }
                }
            }
        }

        k_temp1 = k_temp1*tree_order;
        k_temp2 = k_temp2*tree_order;
    }


    return BCOL_FN_STARTED;

LAST_STEP:
    /* last step, proxies send full data back to the extra ranks */
    if( 0 < exchange_node->n_extra_sources &&
        root == exchange_node->rank_extra_sources_array[0]) {
        /* regardless, I will bump the ready flag and set it in case someone is watching */
        /* announce that data is ready */
        ready_flag++;
    }

    /* signal that data is ready */
    opal_atomic_wmb ();
    my_ctl_pointer->flags[GATHER_FLAG][bcol_id] = ready_flag;

FINISHED:


    my_ctl_pointer->starting_flag_value[bcol_id]++;

    return BCOL_FN_COMPLETE;
}


/* Blocking routines, used to prototype and test signaling,
 * as well as debug hierarchical algorithm
 */
#if 0
int bcol_basesmuma_gather_init(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    comm_attribs.bcoll_type = BCOL_GATHER;
    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 16;
    comm_attribs.data_src = DATA_SRC_KNOWN;
    comm_attribs.waiting_semantics = BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000;
    inv_attribs.datatype_bitmap = 0x11111111;
    inv_attribs.op_types_bitmap = 0x11111111;


    /* Set attributes for fanin fanout algorithm */
    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs, bcol_basesmuma_k_nomial_gather,
                                 bcol_basesmuma_k_nomial_gather);

    return OMPI_SUCCESS;
}
#endif


/* original, fully blocking, fully synchronous gather - should result in worst performance when used */
#if 0
int bcol_basesmuma_k_nomial_gather(bcol_function_args_t *input_args,
                                   mca_bcol_base_function_t *c_input_args)
{
    /* local variables */
    int group_size;
    int first_instance=0, flag_offset;
    int rc = OMPI_SUCCESS;
    int leading_dim, buff_idx, idx;
    int *group_list;
    int src, comm_src, knt, i, k, j, k_temp1, k_temp2;
    int pseudo_root, proxy_root, pseudo_base_adj;
    volatile int64_t ready_flag;
    int count=input_args->count;
    struct ompi_datatype_t* dtype=input_args->dtype;
    int root=input_args->root;
    int base_adj, base;
    int64_t sequence_number=input_args->sequence_num;
    mca_bcol_basesmuma_module_t* bcol_module=
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
    int my_rank = bcol_module->super.sbgp_partner_module->my_index;
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
    netpatterns_k_exchange_node_t *exchange_node =
        &bcol_module->knomial_allgather_tree;

    int buff_offset = bcol_module->super.hier_scather_offset;

    /* "indirectors" */
    int *list_connected = bcol_module->super.list_n_connected;
    int *inv_map = exchange_node->inv_reindex_map;
    int *reindex_map = exchange_node->reindex_map;
    /*int *reindex_map = exchange_node->reindex_map;*/
    /* stray rank == first rank in the extra set */
    int stray = exchange_node->k_nomial_stray;

    /* tree radix */
    int tree_order = exchange_node->tree_order;
    /* tree depth */
    int pow_k =  exchange_node->log_tree_order;
    /* largest power of k less than or equal to np */
    int cnt = exchange_node->n_largest_pow_tree_order;

    /*fprintf(stderr,"tree order %d pow_k %d stray %d root %d\n",tree_order, pow_k, stray, root);*/
    /* payload structures */
    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    volatile char *child_data_pointer;

    /* control structures */
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    volatile mca_bcol_basesmuma_header_t *child_ctl_pointer;
    /*volatile mca_bcol_basesmuma_ctl_struct_t* parent_ctl_pointer; */

    size_t pack_len = 0, dt_size;
    void *data_addr = (void *)((unsigned char *)input_args->src_desc->data_addr);

    /* active in the algorithm */
    bool active = true;

#if 0
    fprintf(stderr,"Entering sm gather input_args->sbuf_offset %d \n",input_args->sbuf_offset);
    fflush(stderr);
#endif


    /* we will work only on packed data - so compute the length*/
    /* this is the size of my data, this is not gatherv so it's the same
     * for all ranks in the communicator.
     */
    ompi_datatype_type_size(dtype, &dt_size);
    pack_len=count*dt_size;
    /* now set the "real" offset */
    buff_offset = buff_offset*pack_len;

    buff_idx = input_args->src_desc->buffer_index;

    /* Get addressing information */
    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    /* I have a feeling that I'll need this */
    group_list = bcol_module->super.sbgp_partner_module->group_list;

    group_size = bcol_module->colls_no_user_data.size_of_group;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);
    /*ctl_structs=(mca_bcol_basesmuma_ctl_struct_t **)
      bcol_module->colls_with_user_data.ctl_buffs+idx;
    */
    data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;

    /* Set pointer to current proc ctrl region */
    /*my_ctl_pointer = ctl_structs[my_rank]; */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;

    /* setup resource recycling */
    if( my_ctl_pointer->sequence_number < sequence_number ) {
        first_instance=1;
    }

    if( first_instance ) {
        /* Signal arrival */
        my_ctl_pointer->flag = -1;
        my_ctl_pointer->gflag = -1;
        my_ctl_pointer->index=1;
        /* this does not need to use any flag values , so only need to
         * set the value for subsequent values that may need this */
        my_ctl_pointer->starting_flag_value=0;
        flag_offset=0;

    } else {
        /* only one thread at a time will be making progress on this
         *   collective, so no need to make this atomic */
        my_ctl_pointer->index++;
    }


    /* increment the starting flag by one and return */
    flag_offset = my_ctl_pointer->starting_flag_value;
    ready_flag = flag_offset + sequence_number + 1;
    my_ctl_pointer->sequence_number = sequence_number;

/* debug
   fprintf(stderr," sequence_number %lld flag_offset %d starting flag val %d\n",sequence_number,flag_offset, my_ctl_pointer->starting_flag_value);
   fflush(stderr);
   end debug */


    /*
     * Fan out from root
     */
    /* don't need this either */
    /* root is the local leader */
    /* calculate the number of steps necessary for this collective */

    /* first thing we do is figure out where the root is in our new indexing */
    /* find root in new indexing */
    pseudo_root = inv_map[root];
    /* see if this is larger than the stray */
    if( pseudo_root >= stray ) {
        /* then we need to define the proxy root, everyone can do this */
        proxy_root = pseudo_root - cnt;
    }else {
        proxy_root = pseudo_root;
    }



    if( EXTRA_NODE == exchange_node->node_type ) {

        /* signal arrival */
        my_ctl_pointer->gflag = ready_flag;

        /* send is done */

        /* poll for data only if I am the root */
        /* bump the ready flag */
        ready_flag++;
        if( root == my_rank ){
            /* poll for data from proxy */
            src = exchange_node->rank_extra_sources_array[0];
            /* get src data buffer */
            child_data_pointer = data_buffs[src].payload;
            child_ctl_pointer = data_buffs[src].ctl_struct;
            while(!IS_GDATA_READY(child_ctl_pointer,ready_flag,sequence_number)){
                opal_progress();
            }
            /* receive the data from the proxy, aka pseudo-root */

            memcpy((void *) ((unsigned char *) data_addr + buff_offset),(void *) ((unsigned char *) child_data_pointer+buff_offset)
                   ,pack_len*group_size);
        }
        goto FINISHED;


    } else if( 0 < exchange_node->n_extra_sources ) {

        /* am a proxy, poll for pack_len data from extra */
        src = exchange_node->rank_extra_sources_array[0];
        /* get src data buffer */
        child_data_pointer = data_buffs[src].payload;
        child_ctl_pointer = data_buffs[src].ctl_struct;
        knt = 0;
        for( i = 0; i < src; i++){
            knt += list_connected[i];
        }
        while(!IS_GDATA_READY(child_ctl_pointer,ready_flag,sequence_number)){
            opal_progress();
        }
        memcpy((void *) ((unsigned char *) data_addr + buff_offset + pack_len*knt),
               (void *) ((unsigned char *) child_data_pointer + buff_offset +
                         pack_len*knt), pack_len*list_connected[src]);
        /*fprintf(stderr,"999 proxy received data from %d at offset %d of length %d\n",src,
          buff_offset+pack_len*knt,pack_len*list_connected[src]);
        */
    }

    /* start the k-nomial gather phase */
    /* only "active ranks participate, once a rank has forwarded its data, it becomes inactive */
    knt = 0;
    while(active){
        k_temp1 = tree_order;
        k_temp2 = 1;
        for( i = 0; i < pow_k; i++) {
            /* then find the base */
            /*FIND_BASE(base,my_rank,i+1,tree_order);*/
            FIND_BASE(base,exchange_node->reindex_myid,i+1,tree_order);
            /* now find the adjusted base */
            base_adj = base + (base + proxy_root)%k_temp1;
            /* ok, now find out WHO is occupying this slot */
            /*pseudo_base_adj = inv_map[base_adj];*/
            pseudo_base_adj = reindex_map[base_adj];

            if(my_rank == pseudo_base_adj ) {
                /* then go ahead and poll for children's data */
                for( j = 0; j < (tree_order - 1); j++ ) {
                    /* send phase
                     */
                    /* get communication partner */

                    src = exchange_node->rank_exchanges[i][j];
                    /*fprintf(stderr,"comm_src %d\n",comm_src);*/
                    /* remember, if we have extra ranks, then we won't participate
                     * with a least one peer. Make a check
                     */
                    if( src < 0 ){
                        continue;
                    }

                    /*fprintf(stderr,"src %d\n",src);*/
                    child_data_pointer = data_buffs[src].payload;
                    child_ctl_pointer = data_buffs[src].ctl_struct;
                    while(!IS_GDATA_READY(child_ctl_pointer,ready_flag,sequence_number)){
                        opal_progress();
                    }
                    memcpy((void *) ((unsigned char *) data_addr + buff_offset +
                                     exchange_node->payload_info[i][j].r_offset*pack_len),
                           (void *) ((unsigned char *) child_data_pointer + buff_offset +
                                     exchange_node->payload_info[i][j].r_offset*pack_len),
                           exchange_node->payload_info[i][j].r_len*pack_len);
                    /*
                      fprintf(stderr,"999 receiving data from %d at offset %d of length %d\n",
                      exchange_node->rank_exchanges[i][j], buff_offset + exchange_node->payload_info[i][j].r_offset,
                      exchange_node->payload_info[i][j].r_len*pack_len);
                    */
                    opal_atomic_wmb ();
                    knt++;
                    if(knt == exchange_node->n_actual_exchanges) {
                        /* this is the trick to break the root out,
                         * only the root should be able to satisfy this
                         */
                        /*
                          fprintf(stderr,"hello n_actual is %d \n",knt);
                          fprintf(stderr,"hello n_actual_exch is %d \n",
                          exchange_node->n_actual_exchanges);
                        */
                        goto LAST_STEP;
                    }
                }
            } else {
                /* announce my arrival */
                my_ctl_pointer->gflag = ready_flag;
                active = false;
                break;
            }

            k_temp1 = k_temp1*tree_order;
            k_temp2 = k_temp2*tree_order;
        }
    }
LAST_STEP:
    /* last step, proxies send full data back to the extra ranks */
    if( 0 < exchange_node->n_extra_sources &&
        root == exchange_node->rank_extra_sources_array[0]) {
        /* regardless, I will bump the ready flag and set it in case someone is watching */
        /* announce that data is ready */
        ready_flag++;
        my_ctl_pointer->gflag = ready_flag;
    }


FINISHED:

/* debug
   fprintf(stderr," my_ctl_pointer->index %d n of this type %d %u \n",
   my_ctl_pointer->index,c_input_args->n_of_this_type_in_collective,getpid());
   fflush(stderr);
   end debug */

    my_ctl_pointer->starting_flag_value+=1;

    return BCOL_FN_COMPLETE;
}

#endif


#if 0
/* blocking, asynchronous polling gather routine */
int bcol_basesmuma_k_nomial_gather(bcol_function_args_t *input_args,
                                   mca_bcol_base_function_t *c_input_args)
{
    /* local variables */
    int group_size;
    int first_instance=0, flag_offset;
    int rc = OMPI_SUCCESS;
    int leading_dim, buff_idx, idx;
    int *group_list;
    int src, comm_src, knt, i, k, j, k_temp1, k_temp2;
    int pseudo_root, proxy_root, pseudo_base_adj;
    volatile int64_t ready_flag;
    int count=input_args->count;
    struct ompi_datatype_t* dtype=input_args->dtype;
    int root=input_args->root;
    int base_adj, base;
    int total_peers, my_pow_k;
    int probe;
    int matched;
    int64_t sequence_number=input_args->sequence_num;
    mca_bcol_basesmuma_module_t* bcol_module=
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
    int my_rank = bcol_module->super.sbgp_partner_module->my_index;
    mca_bcol_basesmuma_component_t *cm = &mca_bcol_basesmuma_component;
    netpatterns_k_exchange_node_t *exchange_node =
        &bcol_module->knomial_allgather_tree;

    int buff_offset = bcol_module->super.hier_scather_offset;

    /* "indirectors" */
    int *list_connected = bcol_module->super.list_n_connected;
    int *inv_map = exchange_node->inv_reindex_map;
    int *reindex_map = exchange_node->reindex_map;
    /*int *reindex_map = exchange_node->reindex_map;*/
    /* stray rank == first rank in the extra set */
    int stray = exchange_node->k_nomial_stray;

    /* tree radix */
    int tree_order = exchange_node->tree_order;
    /* tree depth */
    int pow_k =  exchange_node->log_tree_order;
    /* largest power of k less than or equal to np */
    int cnt = exchange_node->n_largest_pow_tree_order;

    /*fprintf(stderr,"tree order %d pow_k %d stray %d root %d\n",tree_order, pow_k, stray, root);*/
    /* payload structures */
    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    volatile char *child_data_pointer;

    /* control structures */
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    volatile mca_bcol_basesmuma_header_t *child_ctl_pointer;
    /*volatile mca_bcol_basesmuma_ctl_struct_t* parent_ctl_pointer; */

    size_t pack_len = 0, dt_size;
    void *data_addr = (void *)((unsigned char *)input_args->src_desc->data_addr);

    /* active in the algorithm */
    bool active = true;

#if 0
    fprintf(stderr,"Entering sm gather root %d \n",root);
    fflush(stderr);
#endif


    /* we will work only on packed data - so compute the length*/
    /* this is the size of my data, this is not gatherv so it's the same
     * for all ranks in the communicator.
     */
    ompi_datatype_type_size(dtype, &dt_size);
    pack_len=count*dt_size;
    /* now set the "real" offset */
    buff_offset = buff_offset*pack_len;

    buff_idx = input_args->src_desc->buffer_index;

    /* Get addressing information */
    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    /* I have a feeling that I'll need this */
    group_list = bcol_module->super.sbgp_partner_module->group_list;

    group_size = bcol_module->colls_no_user_data.size_of_group;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);
    /*ctl_structs=(mca_bcol_basesmuma_ctl_struct_t **)
      bcol_module->colls_with_user_data.ctl_buffs+idx;
    */
    data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;

    /* Set pointer to current proc ctrl region */
    /*my_ctl_pointer = ctl_structs[my_rank]; */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;

    /* setup resource recycling */
    if( my_ctl_pointer->sequence_number < sequence_number ) {
        first_instance=1;
    }

    if( first_instance ) {
        /* Signal arrival */
        my_ctl_pointer->flag = -1;
        my_ctl_pointer->gflag = -1;
        my_ctl_pointer->index=1;
        /* this does not need to use any flag values , so only need to
         * set the value for subsequent values that may need this */
        my_ctl_pointer->starting_flag_value=0;
        flag_offset=0;

    } else {
        /* only one thread at a time will be making progress on this
         *   collective, so no need to make this atomic */
        my_ctl_pointer->index++;
    }


    /* increment the starting flag by one and return */
    flag_offset = my_ctl_pointer->starting_flag_value;
    ready_flag = flag_offset + sequence_number + 1;
    my_ctl_pointer->sequence_number = sequence_number;

/* debug
   fprintf(stderr," sequence_number %lld flag_offset %d starting flag val %d\n",sequence_number,flag_offset, my_ctl_pointer->starting_flag_value);
   fflush(stderr);
   end debug */


    /*
     * Fan out from root
     */
    /* don't need this either */
    /* root is the local leader */
    /* calculate the number of steps necessary for this collective */

    /* first thing we do is figure out where the root is in our new indexing */
    /* find root in new indexing */
    pseudo_root = inv_map[root];
    /* see if this is larger than the stray */
    if( pseudo_root >= stray ) {
        /* then we need to define the proxy root, everyone can do this */
        proxy_root = pseudo_root - cnt;
    }else {
        proxy_root = pseudo_root;
    }
    if( EXTRA_NODE == exchange_node->node_type ) {

        /* signal arrival */
        my_ctl_pointer->gflag = ready_flag;

        /* send is done */

        /* poll for data only if I am the root */
        /* bump the ready flag */
        ready_flag++;
        if( root == my_rank ){
            /* poll for data from proxy */
            src = exchange_node->rank_extra_sources_array[0];
            /* get src data buffer */
            child_data_pointer = data_buffs[src].payload;
            child_ctl_pointer = data_buffs[src].ctl_struct;
            /* in this case, you must block */
            while(!IS_GDATA_READY(child_ctl_pointer,ready_flag,sequence_number)){
                opal_progress();
            }
            /* receive the data from the proxy, aka pseudo-root */

            memcpy((void *) ((unsigned char *) data_addr + buff_offset),
                   (void *) ((unsigned char *) child_data_pointer+buff_offset)
                   ,pack_len*group_size);
        }
        goto FINISHED;


    } else if( 0 < exchange_node->n_extra_sources ) {

        /* am a proxy, poll for pack_len data from extra */
        src = exchange_node->rank_extra_sources_array[0];
        /* get src data buffer */
        child_data_pointer = data_buffs[src].payload;
        child_ctl_pointer = data_buffs[src].ctl_struct;
        knt = 0;
        for( i = 0; i < src; i++){
            knt += list_connected[i];
        }
        /* must block here also */
        while(!IS_GDATA_READY(child_ctl_pointer,ready_flag,sequence_number)){
            opal_progress();
        }
        memcpy((void *) ((unsigned char *) data_addr + buff_offset + pack_len*knt),
               (void *) ((unsigned char *) child_data_pointer + buff_offset +
                         pack_len*knt), pack_len*list_connected[src]);
        /*fprintf(stderr,"999 proxy received data from %d at offset %d of length %d\n",src,
          buff_offset+pack_len*knt,pack_len*list_connected[src]);
        */
    }
    /* do some figuring */

    total_peers = 0;
    my_pow_k = pow_k;
    k_temp1 = tree_order;
    k_temp2 = 1;
    for( i = 0; i < pow_k; i++) {
        /* then find the base */
        /*FIND_BASE(base,my_rank,i+1,tree_order);*/
        FIND_BASE(base,exchange_node->reindex_myid,i+1,tree_order);
        /* now find the adjusted base */
        base_adj = base + (base + proxy_root)%k_temp1;
        /* ok, now find out WHO is occupying this slot */
        /*pseudo_base_adj = inv_map[base_adj];*/
        pseudo_base_adj = reindex_map[base_adj];

        if(my_rank == pseudo_base_adj ) {
            /* then go ahead and poll for children's data */
            for( j = 0; j < (tree_order - 1); j++ ) {
                /* send phase
                 */
                /* get communication partner */

                src = exchange_node->rank_exchanges[i][j];
                /*fprintf(stderr,"comm_src %d\n",comm_src);*/
                /* remember, if we have extra ranks, then we won't participate
                 * with a least one peer. Make a check
                 */
                if( src < 0 ){
                    continue;
                }else{
                    total_peers++;
                }


            }
        } else {
            /* announce my arrival */
            my_pow_k = i;
            break;
        }

        k_temp1 = k_temp1*tree_order;
        k_temp2 = k_temp2*tree_order;
    }

    if( 0 == my_pow_k ){
        /* signal arrival */
        my_ctl_pointer->gflag = ready_flag;

        goto FINISHED;
    }



    /* start the k-nomial gather phase */
    /* only "active ranks participate, once a rank has forwarded its data, it becomes inactive */
    knt = 0;
    while(active){
        k_temp1 = tree_order;
        k_temp2 = 1;
        for( i = 0; i < my_pow_k; i++) {

            /* then go ahead and poll for children's data */
            for( j = 0; j < (tree_order - 1); j++ ) {
                matched = 0;
                /* send phase
                 */
                /* get communication partner */

                src = exchange_node->rank_exchanges[i][j];
                /*fprintf(stderr,"comm_src %d\n",comm_src);*/
                /* remember, if we have extra ranks, then we won't participate
                 * with a least one peer. Make a check
                 */
                if( src < 0 ){
                    continue;
                }

                /*fprintf(stderr,"src %d\n",src);*/
                child_data_pointer = data_buffs[src].payload;
                child_ctl_pointer = data_buffs[src].ctl_struct;

                /* if child has been marked, then skip */
                if( sequence_number == child_ctl_pointer->mark ){
                    continue;
                }


                for( probe = 0; probe < cm->num_to_probe && (0 == matched); probe++){
                    if(IS_GDATA_READY(child_ctl_pointer,ready_flag,sequence_number)){
                        /* mark the child's pointer */
                        child_ctl_pointer->mark = sequence_number;
                        /* copy the data */

                        memcpy((void *) ((unsigned char *) data_addr + buff_offset +
                                         exchange_node->payload_info[i][j].r_offset*pack_len),
                               (void *) ((unsigned char *) child_data_pointer + buff_offset +
                                         exchange_node->payload_info[i][j].r_offset*pack_len),
                               exchange_node->payload_info[i][j].r_len*pack_len);
                        /*
                          fprintf(stderr,"999 receiving data from %d at offset %d of length %d\n",
                          exchange_node->rank_exchanges[i][j], buff_offset + exchange_node->payload_info[i][j].r_offset,
                          exchange_node->payload_info[i][j].r_len*pack_len);
                        */
                        knt++;
                        if(knt == total_peers) {
                            /* this is the trick to break the root out,
                             * only the root should be able to satisfy this
                             */
                            /*
                              fprintf(stderr,"hello n_actual is %d \n",knt);
                              fprintf(stderr,"hello n_actual_exch is %d \n",
                              exchange_node->n_actual_exchanges);
                            */
                            opal_atomic_wmb ();
                            my_ctl_pointer->gflag = ready_flag;

                            goto LAST_STEP;
                        }
                        matched = 1;
                    }else{
                        opal_progress();
                    }
                }
            }
        }

        k_temp1 = k_temp1*tree_order;
        k_temp2 = k_temp2*tree_order;
    }
LAST_STEP:
    /* last step, proxies send full data back to the extra ranks */
    if( 0 < exchange_node->n_extra_sources &&
        root == exchange_node->rank_extra_sources_array[0]) {
        /* regardless, I will bump the ready flag and set it in case someone is watching */
        /* announce that data is ready */
        ready_flag++;
        my_ctl_pointer->gflag = ready_flag;
    }


FINISHED:

/* debug
   fprintf(stderr," my_ctl_pointer->index %d n of this type %d %u \n",
   my_ctl_pointer->index,c_input_args->n_of_this_type_in_collective,getpid());
   fflush(stderr);
   end debug */

    my_ctl_pointer->starting_flag_value+=1;

    return BCOL_FN_COMPLETE;
}
#endif
