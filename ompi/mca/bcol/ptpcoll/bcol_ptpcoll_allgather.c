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
#include "bcol_ptpcoll_allreduce.h"
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

int bcol_ptpcoll_k_nomial_allgather_init(bcol_function_args_t *input_args,
                struct mca_bcol_base_function_t *const_args)
{
    /* local variables */

    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *) const_args->bcol_module;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    netpatterns_k_exchange_node_t *exchange_node = &ptpcoll_module->knomial_allgather_tree;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int group_size = ptpcoll_module->group_size;
    int *list_connected = ptpcoll_module->super.list_n_connected; /* critical for hierarchical colls */

    int tag;
    int i, j;
    int knt;
    int comm_src, comm_dst, src, dst;
    int recv_offset, recv_len;
    int send_offset, send_len;

    uint32_t buffer_index = input_args->buffer_index;
    int pow_k, tree_order;
    int rc = OMPI_SUCCESS;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int completed = 0; /* initialized */
    void *data_buffer = (void*)(
            (unsigned char *) input_args->sbuf +
            (size_t) input_args->sbuf_offset);
    int pack_len = input_args->count * input_args->dtype->super.size;

#if 0
    fprintf(stderr,"entering p2p allgather pack_len %d. exchange node: %p\n",pack_len, exchange_node);
#endif
    /* initialize the iteration counter */
    int *iteration = &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration;
    *iteration = 0;

    /* reset active request counter */
    *active_requests = 0;

    /* keep tag within the limit supported by the pml */
    tag = (PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;

    /* k-nomial parameters */
    tree_order = exchange_node->tree_order;
    pow_k = exchange_node->log_tree_order;


    /* let's begin the collective, starting with extra ranks and their
     * respective proxies
     */
    if( EXTRA_NODE == exchange_node->node_type ) {

        /* then I will send to my proxy rank*/
        dst = exchange_node->rank_extra_sources_array[0];
        /* find rank in the communicator */
        comm_dst = group_list[dst];
        /* now I need to calculate my own offset */
        knt = 0;
        for (i = 0 ; i < my_group_index; i++){
            knt += list_connected[i];
        }

        /* send the data to my proxy */
        rc = MCA_PML_CALL(isend((void *) ( (unsigned char *) data_buffer +
                        knt*pack_len),
                        pack_len * list_connected[my_group_index],
                        MPI_BYTE,
                        comm_dst, tag,
                        MCA_PML_BASE_SEND_STANDARD, comm,
                        &(requests[*active_requests])));

        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10,("Failed to isend data"));
            return OMPI_ERROR;
        }
        ++(*active_requests);

        /* now I go ahead and post the receive from my proxy */
        comm_src = comm_dst;
        knt = 0;
        for( i =0; i < group_size; i++){
            knt += list_connected[i];
        }
        rc = MCA_PML_CALL(irecv(data_buffer,
                    knt * pack_len,
                    MPI_BYTE,
                    comm_src,
                    tag , comm, &(requests[*active_requests])));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to post ireceive "));
            return OMPI_ERROR;
        }

        ++(*active_requests);
        /* poll for completion */
        /* this polls internally */
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if(completed){
            /* go to buffer release */
            goto FINISHED;
        }else{
            /* save state and hop out
             * nothing to save here
             */
            return ((OMPI_SUCCESS != rc) ? OMPI_ERROR : BCOL_FN_STARTED);
        }
    }else if ( 0 < exchange_node->n_extra_sources ) {

        /* I am a proxy for someone */
        src = exchange_node->rank_extra_sources_array[0];
        /* find the rank in the communicator */
        comm_src = group_list[src];
        knt = 0;
        for(i = 0; i < src; i++){
            knt += list_connected[i];
        }
        /* post the receive */
        rc = MCA_PML_CALL(irecv((void *) ( (unsigned char *) data_buffer
                        + knt*pack_len),
                        pack_len * list_connected[src],
                        MPI_BYTE,
                        comm_src,
                        tag , comm, &(requests[*active_requests])));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to post ireceive "));
            return OMPI_ERROR;
        }

        ++(*active_requests);
        /* poll for completion */
        /* this routine polls internally */
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if(!completed){
            /* save state and hop out
             * We really do need to block here so set
             * the iteration to -1 indicating we need to
             *  finish this part first
             */
            *iteration = -1;
            return ((OMPI_SUCCESS != rc )? OMPI_ERROR : BCOL_FN_STARTED);
        }

    }

    /* we start the recursive k - ing phase */
    /* fprintf(stderr,"tree order %d pow_k %d \n",tree_order,pow_k);*/
    for( i = 0; i < pow_k; i++) {
        for(j = 0; j < (tree_order - 1); j++) {

            /* send phase */
            dst = exchange_node->rank_exchanges[i][j];
            if( dst < 0 ){
                continue;
            }
            comm_dst = group_list[dst];
            send_offset = exchange_node->payload_info[i][j].s_offset * pack_len;
            send_len = exchange_node->payload_info[i][j].s_len * pack_len;
            /* debug print */
            /* fprintf(stderr,"sending %d bytes to rank %d at offset %d\n",send_len, */
            /*         comm_dst,send_offset); */
            rc = MCA_PML_CALL(isend((void*)((unsigned char *) data_buffer +
                            send_offset),
                            send_len,
                            MPI_BYTE,
                            comm_dst, tag,
                            MCA_PML_BASE_SEND_STANDARD, comm,
                            &(requests[*active_requests])));

            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10,("Failed to isend data"));
                return OMPI_ERROR;
            }
            ++(*active_requests);

            /* sends are posted */
        }

        /* Now post the recv's */
        for( j = 0; j < (tree_order - 1); j++ ) {

            /* recv phase */
            src = exchange_node->rank_exchanges[i][j];
            if( src < 0 ) {
                continue;
            }
            comm_src = group_list[src];
            recv_offset = exchange_node->payload_info[i][j].r_offset * pack_len;
            recv_len = exchange_node->payload_info[i][j].r_len * pack_len;
            /* debug print */
            /* fprintf(stderr,"recving %d bytes to rank %d at offset %d\n",recv_len, */
            /*         comm_src,recv_offset); */
            /* post the receive */
            rc = MCA_PML_CALL(irecv((void *) ((unsigned char *) data_buffer +
                            recv_offset),
                            recv_len,
                            MPI_BYTE,
                            comm_src,
                            tag, comm, &(requests[*active_requests])));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to post ireceive "));
                return OMPI_ERROR;
            }

            ++(*active_requests);
        }
        /* finished all send/recv's now poll for completion before
         * continuing to next iteration
         */
        completed = 0;
        /* polling internally on 2*(k - 1) requests */
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);

        if(!completed){
            /* save state and hop out
             * only the iteration needs to be tracked
             */
            *iteration = i; /* need to pick up here */

            return ((OMPI_SUCCESS != rc) ? OMPI_ERROR : BCOL_FN_STARTED);
        }
    }

    /* finish off the last piece, send the data back to the extra  */
    if( 0 < exchange_node->n_extra_sources ) {
        dst = exchange_node->rank_extra_sources_array[0];
        comm_dst = group_list[dst];
        knt = 0;
        for( i = 0; i < group_size; i++){
            knt += list_connected[i];
        }
        /* debug print */
        /*
        fprintf(stderr,"sending %d bytes to extra %d \n",pack_len*knt,comm_dst);
        */
        rc = MCA_PML_CALL(isend(data_buffer,
                    pack_len * knt,
                    MPI_BYTE,
                    comm_dst, tag,
                    MCA_PML_BASE_SEND_STANDARD, comm,
                    &(requests[*active_requests])));

        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10,("Failed to isend data"));
            return OMPI_ERROR;
        }
        ++(*active_requests);

        /* probe for send completion */
        completed = 0;
        /* polling internally */
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if(!completed){
            /* save state and hop out
             * We really do need to block here so set
             * the iteration to pow_k +1 indicating we need to
             *  finish progressing the last part
             */
            *iteration = pow_k + 1;

            return (OMPI_SUCCESS != rc ? OMPI_ERROR : BCOL_FN_STARTED);
        }
    }

FINISHED:
    /* recycle buffer if need be */
    return BCOL_FN_COMPLETE;
}

/* allgather progress function */

int bcol_ptpcoll_k_nomial_allgather_progress(bcol_function_args_t *input_args,
                        struct mca_bcol_base_function_t *const_args)
{


    /* local variables */

    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *) const_args->bcol_module;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    netpatterns_k_exchange_node_t *exchange_node = &ptpcoll_module->knomial_allgather_tree;
    int group_size = ptpcoll_module->group_size;
    int *list_connected = ptpcoll_module->super.list_n_connected; /* critical for hierarchical colls */


    int tag;
    int i, j;
    int knt;
    int comm_src, comm_dst, src, dst;
    int recv_offset, recv_len;
    int send_offset, send_len;
    uint32_t buffer_index = input_args->buffer_index;

    int pow_k, tree_order;
    int rc = OMPI_SUCCESS;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int completed = 0; /* initialized */
    void *data_buffer = (void*)(
            (unsigned char *) input_args->sbuf +
            (size_t) input_args->sbuf_offset);
    int pack_len = input_args->count * input_args->dtype->super.size;
    /* initialize the counter */
    int *iteration = &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration;


#if 0
    fprintf(stderr,"%d: entering p2p allgather progress AR: %d iter: %d\n",my_group_index,*active_requests,
            *iteration);
#endif
    /* keep tag within the limit supported by the pml */
    tag = (PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;

    /* k-nomial tree parameters */
    tree_order = exchange_node->tree_order;
    pow_k = exchange_node->log_tree_order;

    /* let's begin the collective, starting with extra ranks and their
     * respective proxies
     */
    if( EXTRA_NODE == exchange_node->node_type ) {

        /* debug print */
        /*fprintf(stderr,"666 \n");*/
        /* simply poll for completion */
        completed = 0;
        /* polling internally */
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if(completed){
            /* go to buffer release */
            goto FINISHED;
        }else{
            /* save state and hop out
             * nothing to save here
             */
            return ((OMPI_SUCCESS != rc) ? OMPI_ERROR : BCOL_FN_STARTED);
        }
    }else if ( 0 < exchange_node->n_extra_sources && (-1 == *iteration)) {

        /* I am a proxy for someone */
        /* Simply poll for completion */
        completed = 0;
        /* polling internally */
        assert( 1 == *active_requests);
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if(!completed){
            /* save state and hop out
             * We really do need to block here so set
             * the iteration to -1 indicating we need to
             *  finish this part first
             */
            (*iteration) = -1;
            return ((OMPI_SUCCESS != rc) ? OMPI_ERROR : BCOL_FN_STARTED);
        }
        /* I may now proceed to the recursive k - ing phase */
        *iteration = 0;
    }


    /* the ordering here between the extra rank and progress active requests
     * is critical
     */
    /* extra rank */
    if( (pow_k + 1) == *iteration ){
        /* finish off the last one */
        goto PROGRESS_EXTRA;
    }

    /* active requests must be completed before continuing on to
     * recursive k -ing step
     * CAREFUL HERE, IT THIS REALLY WHAT YOU WANT??
     */
    if( 0 < (*active_requests) ) {
        /* then we have something to progress from last step */
        /* debug print */
        /*
        fprintf(stderr,"%d: entering progress AR: %d iter: %d\n",my_group_index,*active_requests,
            *iteration);
        */
        completed = 0;
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if(!completed){
            /* save state and hop out
             * state hasn't changed
             */

            return ((MPI_SUCCESS != rc) ? OMPI_ERROR : BCOL_FN_STARTED);
        }
        ++(*iteration);
    }



    /* we start the recursive k - ing phase */
    for( i = *iteration; i < pow_k; i++) {
        /* nothing changes here */
        for(j = 0; j < (tree_order - 1); j++) {

            /* send phase */
            dst = exchange_node->rank_exchanges[i][j];
            if( dst < 0 ){
                continue;
            }
            comm_dst = group_list[dst];
            send_offset = exchange_node->payload_info[i][j].s_offset * pack_len;
            send_len = exchange_node->payload_info[i][j].s_len * pack_len;
            rc = MCA_PML_CALL(isend((void*)((unsigned char *) data_buffer +
                            send_offset),
                            send_len,
                            MPI_BYTE,
                            comm_dst, tag,
                            MCA_PML_BASE_SEND_STANDARD, comm,
                            &(requests[*active_requests])));

            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10,("Failed to isend data"));
                return OMPI_ERROR;
            }
            ++(*active_requests);

            /* sends are posted */
        }

        /* Now post the recv's */
        for( j = 0; j < (tree_order - 1); j++ ) {

            /* recv phase */
            src = exchange_node->rank_exchanges[i][j];
            if( src < 0 ) {
                continue;
            }
            comm_src = group_list[src];
            recv_offset = exchange_node->payload_info[i][j].r_offset * pack_len;
            recv_len = exchange_node->payload_info[i][j].r_len * pack_len;
            /* post the receive */
            rc = MCA_PML_CALL(irecv((void *) ((unsigned char *) data_buffer +
                            recv_offset),
                            recv_len,
                            MPI_BYTE,
                            comm_src,
                            tag, comm, &(requests[*active_requests])));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to post ireceive "));
                return OMPI_ERROR;
            }

            ++(*active_requests);
        }
        /* finished all send/recv's now poll for completion before
         * continuing to next iteration
         */
        completed = 0;
        /* make this non-blocking */
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if(!completed){
            /* save state and hop out
             * We really do need to block here so set
             * the iteration to -1 indicating we need to
             *  finish this part first
             */
            *iteration = i; /* need to pick up here */

            return ((OMPI_SUCCESS != rc) ? OMPI_ERROR : BCOL_FN_STARTED);
        }
    }

    /* finish off the last piece, send the data back to the extra  */
    if( 0 < exchange_node->n_extra_sources ) {
        dst = exchange_node->rank_extra_sources_array[0];
        comm_dst = group_list[dst];
        knt = 0;
        for( i = 0; i < group_size; i++){
            knt += list_connected[i];
        }
        rc = MCA_PML_CALL(isend(data_buffer,
                    pack_len * knt,
                    MPI_BYTE,
                    comm_dst, tag,
                    MCA_PML_BASE_SEND_STANDARD, comm,
                    &(requests[*active_requests])));

        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10,("Failed to isend data"));
            return OMPI_ERROR;
        }
        ++(*active_requests);

        /* probe for send completion */
        completed = 0;
        /* make this non-blocking */
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if(!completed){
            /* save state and hop out
             * We really do need to block here so set
             * the iteration to pow_k +1 indicating we need to
             *  finish progressing the last part
             */
            *iteration = pow_k + 1;

            return ((OMPI_SUCCESS != rc) ? OMPI_ERROR :  BCOL_FN_STARTED);
        }
    }
    /* folks need to skip this unless they really are the proxy
     * reentering with the intent of progressing the final send
     */
    goto FINISHED;

PROGRESS_EXTRA:

    /* probe for send completion */
    completed = 0;
    /* make this non-blocking */
    completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
    if(!completed){
        /* save state and hop out
         * We really do need to block here so set
         * the iteration to pow_k +1 indicating we need to
         *  finish progressing the last part
         */

        return ((OMPI_SUCCESS != rc) ? OMPI_ERROR : BCOL_FN_STARTED);
    }

FINISHED:
    /* recycle buffer if need be */
    return BCOL_FN_COMPLETE;
}

/*
 * Register allreduce functions to the BCOL function table,
 * so they can be selected
 */
int bcol_ptpcoll_allgather_init(mca_bcol_base_module_t *super)
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
                bcol_ptpcoll_k_nomial_allgather_init,
                bcol_ptpcoll_k_nomial_allgather_progress);


    comm_attribs.data_src = DATA_SRC_KNOWN;
    inv_attribs.bcol_msg_min = 10000000;
    inv_attribs.bcol_msg_max = 10485760; /* range 4 */

    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                bcol_ptpcoll_k_nomial_allgather_init,
                bcol_ptpcoll_k_nomial_allgather_progress);

    return OMPI_SUCCESS;
}
