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

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <inttypes.h>

#include "bcol_iboffload.h"
#include "bcol_iboffload_alltoall.h"
#include "bcol_iboffload_bcast.h"
#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_task.h"
#include "bcol_iboffload_collreq.h"
#include "bcol_iboffload_collfrag.h"
#include "bcol_iboffload_endpoint.h"

#include "opal/include/opal/types.h"

static int mca_bcol_iboffload_allgather_init(
                               bcol_function_args_t *fn_arguments,
                               mca_bcol_iboffload_module_t *iboffload_module,
                               mca_bcol_iboffload_collreq_t **coll_request,
                               bool if_bcol_last, int mq_credits,
                               collective_message_progress_function progress_fn)
{
    int rc;

    ompi_free_list_item_t *item;
    mca_bcol_iboffload_collfrag_t *coll_fragment;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    OMPI_FREE_LIST_WAIT(&cm->collreqs_free, item, rc);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("Wait for free list failed.\n"));
        return rc;
    }
    /* setup call request */
    (*coll_request) = (mca_bcol_iboffload_collreq_t *) item;

    (*coll_request)->n_fragments  = 0;
    (*coll_request)->n_frags_sent = 0;
    (*coll_request)->n_frag_mpi_complete = 0;
    (*coll_request)->n_frag_net_complete = 0;
    (*coll_request)->if_bcol_last = if_bcol_last;
    (*coll_request)->ml_buffer_index = fn_arguments->buffer_index;
    (*coll_request)->completion_cb_fn = NULL;
    (*coll_request)->buffer_info[SBUF].buf = (void *) (
            (unsigned char *)fn_arguments->sbuf +
            fn_arguments->sbuf_offset);
    (*coll_request)->buffer_info[RBUF].buf = (void *) (
        (unsigned char *)fn_arguments->rbuf +
        fn_arguments->rbuf_offset);
    (*coll_request)->buffer_info[SBUF].offset = fn_arguments->sbuf_offset;
    (*coll_request)->buffer_info[RBUF].offset = fn_arguments->rbuf_offset;
    /* seems like we should initialize the memory registration pointer to NULL here */
    (*coll_request)->buffer_info[SBUF].iboffload_reg = NULL;
    (*coll_request)->buffer_info[RBUF].iboffload_reg = NULL;
    (*coll_request)->dtype = fn_arguments->dtype;
    (*coll_request)->count = fn_arguments->count;
    (*coll_request)->module = iboffload_module;
    /* TODO Pasha: we need it for pending quque. Set it later. */
    (*coll_request)->progress_fn = progress_fn;
    /* TODO Pasha: fix it  later */
    (*coll_request)->qp_index = MCA_BCOL_IBOFFLOAD_QP_BARRIER;

    (*coll_request)->order_info = &fn_arguments->order_info;

    coll_fragment = &((*coll_request)->first_collfrag);
    mca_bcol_iboffload_collfrag_init(coll_fragment);

    /** Vasily ????? */
    /* mq_credits = (*coll_request)->total_tasks_num; */
    coll_fragment->mq_credits = mq_credits;
    coll_fragment->mq_index = COLL_MQ;
    /* pasha: just set it to zero */
    coll_fragment->last_wait_num = 0;
    coll_fragment->alg = -2; /* used only for debug */
    /*
    if (my_rank == algthm_ptr->root) {
        coll_fragment->last_wait_num = 0;
    } else {
        coll_fragment->last_wait_num = algth_lst->last_wait_num;
    }
    */
    /* Pasha: we have nothing to unpack */
    coll_fragment->unpack_size = 0;
    /* coll_fragment->unpack_size = pack_len; */
    /* coll_fragment->alg = RECURSIVE_DOUBLING_TREE_BCAST; */

    /* set pointers for (coll frag) <-> (coll full request) */
    (*coll_request)->user_handle_freed = false;

    fn_arguments->bcol_opaque_data = (void *) (*coll_request);
    /*  We don't have root..
    if (true == fn_arguments->root_flag) {
        (*coll_request)->root = my_group_index;
    } else {
        (*coll_request)->root = fn_arguments->root_route->rank;
    }
    */

    MCA_BCOL_IBOFFLOAD_SET_COLL_REQ_LINKS((*coll_request), coll_fragment);
    return OMPI_SUCCESS;
}

#if 1
static inline void bcol_iboffload_setup_allgather_endpoints_connection(mca_bcol_iboffload_module_t *iboffload)
{
    int i, j;
    /*Seems that we don't require this*/
    netpatterns_k_exchange_node_t *exchange_node = &iboffload->knomial_allgather_tree;

    mca_bcol_iboffload_endpoint_t *ep;

    IBOFFLOAD_VERBOSE(10, ("Open connections.\n"));
#if 0
    fprintf(stderr,"Entering Open Connections\n");
#endif

    /* start with extras and proxy connections */
    if(exchange_node->n_extra_sources > 0) {
        /* connect to endpoint */
        /*ep = iboffload->endpoints[comm_to_ibnet[exchange_node->rank_extra_sources_array[0]]];*/
        ep = iboffload->endpoints[exchange_node->rank_extra_sources_array[0]];
         while (OMPI_SUCCESS !=
                check_endpoint_state(ep, NULL, NULL)) {
            opal_progress();
        }
    }
    /* now move through the recursive k-ing exchanges */
    if(NULL != exchange_node->rank_exchanges) {
        for( i = 0; i < exchange_node->log_tree_order; i++) {
            for( j = 0; j < ( exchange_node->tree_order - 1 ); j++) {
                if( exchange_node->rank_exchanges[i][j] < 0 ){
                    continue;
                }
                /* connect to endpoint */
                /*ep = iboffload->endpoints[comm_to_ibnet[exchange_node->rank_exchanges[i][j]]];*/
                ep = iboffload->endpoints[exchange_node->rank_exchanges[i][j]];
                if (iboffload->ibnet->super.my_index < ep->index) {
                    while(0 == (ep)->remote_zero_rdma_addr.addr) {
                        opal_progress();
                    }
                } else {
                    IBOFFLOAD_VERBOSE(10, ("Trying to connect - %d", ep->index));
                    while (OMPI_SUCCESS !=
                            check_endpoint_state(ep, NULL, NULL)) {
                        opal_progress();
                    }
                }

            }
        }
    }

    /* set the connection status to connected */
    iboffload->connection_status[ALLGATHER_KNOMIAL_ALG] = true;
}
#endif


static inline void bcol_iboffload_setup_allgather_ring_endpoints_connection(mca_bcol_iboffload_module_t *iboffload)
{
    int i;
    const int group_size = iboffload->ibnet->super.group_size;
    mca_bcol_iboffload_endpoint_t *ep;

    IBOFFLOAD_VERBOSE(10, ("Open connections.\n"));

    /* this is algorithm specific - need to move through the algorithm here basically to set up connections, should be
     *
     */

     /* I'm going to leave this alone for now, because I'm
      *  not sure how these endpoints map back to ibnet. Is it mapped to ibnet ids or to communicator ids?
      */
    for (i = 0; i < group_size; i++) {
        ep = iboffload->endpoints[i];
        while (OMPI_SUCCESS !=
                check_endpoint_state(ep, NULL, NULL)) {
            opal_progress();
        }
    }

    /* set the connection status to connected */

    /*JSL - change this macro */
    iboffload->connection_status[ALLGATHER_NEIGHBOR_ALG] = true;
}

#if 0
/* allgather neighbor exchange algorithm N/2 communication steps, 2 connections */
static int mca_bcol_iboffload_neighbor_allgather_userbuffer_exec(mca_bcol_iboffload_module_t *iboffload_module,
                                                   mca_bcol_iboffload_collreq_t *coll_request)
{
    int rc,
        src, dst;

    uint32_t pack_len;
    int my_group_index = iboffload_module->super.sbgp_partner_module->my_index;
    int group_size = iboffload_module->group_size;
    int step, roffset, soffset;
    int neighbor[2], offset_at_step[2], recv_data_from[2], send_data_from;
    int even_rank;
    int parity;

    struct mqe_task *last_send = NULL,
                    *last_wait = NULL;
    mca_bcol_iboffload_collfrag_t *coll_fragment = &coll_request->first_collfrag;

#if 0
    fprintf(stderr,"entering large msg neighbor exchange allgather\n");
#endif
    IBOFFLOAD_VERBOSE(10,("Entering large msg iboffload allgather"));
    if (OPAL_UNLIKELY(!iboffload_module->connection_status[ALLGATHER_NEIGHBOR_ALG])) {
        IBOFFLOAD_VERBOSE(10,("Allgather open new connection "));
        bcol_iboffload_setup_allgather_ring_endpoints_connection(iboffload_module);
    }

    pack_len = coll_request->count * coll_request->dtype->super.size;
    IBOFFLOAD_VERBOSE(10,("My packet length %d pack_len frag_count %d dtype size %d ",
                pack_len,
                coll_request->count,
                coll_request->dtype->super.size));

    /* register send and receive sides */
    /* send side, only sending pack_len data */

    /* I think that probably I will only register the rbuf */
    /* on receive side I need to register pack_len*group_size data */
    rc = mca_bcol_iboffload_prepare_buffer(coll_request->buffer_info[RBUF].buf, pack_len * group_size,
            &coll_request->buffer_info[RBUF].iboffload_reg, iboffload_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("Cannot register memory: "
                         "addr - %p, %d bytes.\n",
                          coll_request->buffer_info[RBUF].buf, pack_len));
        return OMPI_ERROR;
    }
    coll_request->buffer_info[RBUF].lkey = coll_request->buffer_info[RBUF].iboffload_reg->mr->lkey;

    /* it is estimated mq consumption... */
    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                    iboffload_module, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));
        goto out_of_resources;
    }

    coll_fragment->tail_next = &coll_fragment->to_post;


    /* start the neighbor exchange */

    even_rank = !(my_group_index % 2);
    if (even_rank) {
        neighbor[0] = (my_group_index + 1) % group_size;
        neighbor[1] = (my_group_index - 1 + group_size) % group_size;
        recv_data_from[0] = my_group_index;
        recv_data_from[1] = my_group_index;
        offset_at_step[0] = (+2);
        offset_at_step[1] = (-2);
    } else {
        neighbor[0] = (my_group_index - 1 + group_size) % group_size;
        neighbor[1] = (my_group_index + 1) % group_size;
        recv_data_from[0] = neighbor[0];
        recv_data_from[1] = neighbor[0];
        offset_at_step[0] = (-2);
        offset_at_step[1] = (+2);
    }

    /* first step is special step, only send one block */
    roffset = neighbor[0]*pack_len;
    soffset = my_group_index*pack_len;
    /* send receive this */

    dst = neighbor[0];
    src = neighbor[0];

    rc = mca_bcol_iboffload_send_rtr_setup(&last_send,
            src, iboffload_module,
            coll_fragment);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to mca_bcol_iboffload_send_rtr_setup"));
        if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
            goto out_of_resources;
        }
        return OMPI_ERROR;
    }


    rc = mca_bcol_iboffload_recv_rtr_setup(
            &last_wait, dst, iboffload_module, coll_fragment);
    /* send the data */
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to"
                    "mca_bcol_iboffload_recv_rtr_setup"));
        if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
            goto out_of_resources;
        }
        return OMPI_ERROR;
    }

    rc = mca_bcol_iboffload_send_large_buff_setup(
            &last_send, RBUF,
            coll_request->buffer_info[RBUF].offset +
            soffset/* offset calc */ ,
            pack_len, dst,
            iboffload_module, coll_fragment);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to"
                    "mca_bcol_iboffload_send_large_buff_setup"));
        if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
            goto out_of_resources;
        }
        return OMPI_ERROR;
    }
    /* send is done */



    rc = mca_bcol_iboffload_recv_large_buff_setup(&last_wait, RBUF,
            coll_request->buffer_info[RBUF].offset +
            roffset,
            pack_len, src,
            iboffload_module, coll_fragment);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to mca_bcol_iboffload_recv_large_buff_setup"));
        if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
            goto out_of_resources;
        }
        return OMPI_ERROR;
    }

    /* now for the actual neighbor exchange algorithm */


    /* determine initial send location */
    if(even_rank) {
        send_data_from = my_group_index;
    }else {
        send_data_from = recv_data_from[0];
    }
    for( step = 1; step < (group_size/2); step++) {

        parity = step % 2;
        recv_data_from[parity] =
            (recv_data_from[parity] + offset_at_step[parity] + group_size) % group_size;
        src = neighbor[parity];
        dst = src;

        roffset = recv_data_from[parity] * pack_len;
        soffset = send_data_from * pack_len;

        /* post send rtr and recev rtr together */
        if( 1 == step ){
            rc = mca_bcol_iboffload_send_rtr_setup(&last_send,
                    src, iboffload_module,
                    coll_fragment);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to mca_bcol_iboffload_send_rtr_setup"));
                if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                    goto out_of_resources;
                }
                return OMPI_ERROR;
            }

            rc = mca_bcol_iboffload_recv_rtr_setup(
                    &last_wait, dst, iboffload_module, coll_fragment);
            /* send the data */
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to"
                            "mca_bcol_iboffload_recv_rtr_setup"));
                if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                    goto out_of_resources;
                }
                return OMPI_ERROR;
            }
        }


        /* I'm using the hierarchy offset used in the k-nomial allgather */
        /* this won't work...*/
        rc = mca_bcol_iboffload_send_large_buff_setup(
                &last_send, RBUF,
                coll_request->buffer_info[RBUF].offset +
                soffset/* offset calc */ ,
                2 * pack_len, dst,
                iboffload_module, coll_fragment);

        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        "mca_bcol_iboffload_send_large_buff_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }
        /* send is done */


        rc = mca_bcol_iboffload_recv_large_buff_setup(&last_wait, RBUF,
                coll_request->buffer_info[RBUF].offset +
                roffset,
                2 * pack_len, src,
                iboffload_module, coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to mca_bcol_iboffload_recv_large_buff_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }
        send_data_from = recv_data_from[parity];

    }

    /* end of list */
    *coll_fragment->tail_next = NULL;

    /* finish initializing full message descriptor */
    (coll_request)->n_fragments  = 1;
    (coll_request)->n_frags_sent = 1;

    assert(NULL != last_wait);
    last_wait->flags |= MQE_WR_FLAG_SIGNAL;
    coll_fragment->signal_task_wr_id = last_wait->wr_id;
    last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;

    assert(MCA_COLL_ML_NO_BUFFER == coll_request->ml_buffer_index);
    /* post the mwr */
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload_module, coll_fragment->to_post);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload_module->super, coll_request->order_info);

    IBOFFLOAD_VERBOSE(10, ("Return success.\n"));
    return BCOL_FN_STARTED;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Allgather, adding collfrag to collfrag_pending.\n"));
    rc =
        mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload_module);
    return (OMPI_SUCCESS != rc) ? BCOL_FN_NOT_STARTED : BCOL_FN_STARTED;
}
#endif

#if 0
/* debug connection routine */
static inline void bcol_iboffload_setup_allgather_endpoints_connection(mca_bcol_iboffload_module_t *iboffload)
{
    int i;
    const int group_size = iboffload->ibnet->super.group_size;
    mca_bcol_iboffload_endpoint_t *ep;

    IBOFFLOAD_VERBOSE(10, ("Open connections.\n"));

    /* this is algorithm specific - need to move through the algorithm here basically to set up connections, should be
     *
     */

     /* I'm going to leave this alone for now, because I'm
      *  not sure how these endpoints map back to ibnet. Is it mapped to ibnet ids or to communicator ids?
      */
    for (i = 0; i < group_size; i++) {
        ep = iboffload->endpoints[i];
        while (OMPI_SUCCESS !=
                check_endpoint_state(ep, NULL, NULL)) {
            opal_progress();
        }
    }

    /* set the connection status to connected */

    /*JSL - change this macro */
    iboffload->connection_status[ALLGATHER_KNOMIAL_ALG] = true;
}
#endif

static int mca_bcol_iboffload_k_nomial_allgather_userbuffer_exec(mca_bcol_iboffload_module_t *iboffload_module,
                                                   mca_bcol_iboffload_collreq_t *coll_request)
{
    int rc,
        src, dst, comm_dst, comm_src;
    int tree_order, pow_k, i, j;

    uint32_t pack_len;
    int my_group_index = iboffload_module->super.sbgp_partner_module->my_index;
    int group_size = iboffload_module->group_size;
    int *group_list = iboffload_module->super.sbgp_partner_module->group_list;
    int my_comm_index = group_list[my_group_index];

    netpatterns_k_exchange_node_t *exchange_node = &iboffload_module->knomial_allgather_tree;

    struct mqe_task *last_send = NULL,
                    *last_wait = NULL;
    mca_bcol_iboffload_collfrag_t *coll_fragment = &coll_request->first_collfrag;

#if 0
    fprintf(stderr,"entering large msg allgather\n");
#endif
    IBOFFLOAD_VERBOSE(10,("Entering large msg iboffload allgather"));
    if (OPAL_UNLIKELY(!iboffload_module->connection_status[ALLGATHER_KNOMIAL_ALG])) {
        IBOFFLOAD_VERBOSE(10,("Allgather open new connection "));
        bcol_iboffload_setup_allgather_endpoints_connection(iboffload_module);
    }

    pack_len = coll_request->count * coll_request->dtype->super.size;
    IBOFFLOAD_VERBOSE(10,("My packet length %d pack_len frag_count %d dtype size %d ",
                pack_len,
                coll_request->count,
                coll_request->dtype->super.size));

    /* register send and receive sides */
    /* send side, only sending pack_len data */

    /* I think that probably I will only register the rbuf */
    /* on receive side I need to register pack_len*group_size data */

    rc = mca_bcol_iboffload_prepare_buffer(coll_request->buffer_info[RBUF].buf, pack_len * group_size,
            &coll_request->buffer_info[RBUF].iboffload_reg, iboffload_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("Cannot register memory: "
                         "addr - %p, %d bytes.\n",
                          coll_request->buffer_info[RBUF].buf, pack_len));
        return OMPI_ERROR;
    }
    coll_request->buffer_info[RBUF].lkey = coll_request->buffer_info[RBUF].iboffload_reg->mr->lkey;

    /* it is estimated mq consumption... */
    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                    iboffload_module, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));
        goto out_of_resources;
    }

    coll_fragment->tail_next = &coll_fragment->to_post;

    /* start with the extra / proxy phase */
    if( EXTRA_NODE == exchange_node->node_type ) {


        /* send pack_len data to proxy */
        comm_dst = exchange_node->rank_extra_sources_array[0];
        /* get ib subnet id */
        dst = comm_dst; /* comm_to_ibnet[comm_dst];*/
        /* post ready-to-receive receive on sender's side */
        rc = mca_bcol_iboffload_recv_rtr_setup(
                &last_wait, dst, iboffload_module, coll_fragment);

        /* send the data */
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        "mca_bcol_iboffload_recv_rtr_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }

        rc = mca_bcol_iboffload_send_large_buff_setup(
                &last_send, RBUF, coll_request->buffer_info[RBUF].offset + my_comm_index*pack_len,
                pack_len, dst,
                iboffload_module, coll_fragment);

        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        "mca_bcol_iboffload_send_large_buff_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }
        /* send is done */

        /* post the receive */
        comm_src = comm_dst;
        src = dst;
        /* Sending this results in a race condition where if the rtr send bypasses
           the large msg receive on proxy's side, then it triggers the start of the
           recurssive k-ing phase prematurely causing random data corruption.
          */
       /*
        rc = mca_bcol_iboffload_send_rtr_setup(&last_send,
                                    src, iboffload_module,
                                    coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to mca_bcol_iboffload_send_rtr_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }
        */
        rc = mca_bcol_iboffload_recv_large_buff_setup(&last_wait,
                RBUF, coll_request->buffer_info[RBUF].offset,
                pack_len*group_size, src,
                iboffload_module, coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to mca_bcol_iboffload_recv_large_buff_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }

        goto FINISHED;


    } else if( 0 < exchange_node->n_extra_sources ) {

        /* am a proxy, receive pack_len data from extra */
        comm_src = exchange_node->rank_extra_sources_array[0];
        /* get ib subnet */
        src =  comm_src; /*comm_to_ibnet[comm_src];*/

        rc = mca_bcol_iboffload_send_rtr_setup(&last_send,
                                    src, iboffload_module,
                                    coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to mca_bcol_iboffload_send_rtr_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }


        rc = mca_bcol_iboffload_recv_large_buff_setup(&last_wait,
                RBUF, coll_request->buffer_info[RBUF].offset + pack_len*comm_src,
                pack_len, src,
                iboffload_module, coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to mca_bcol_iboffload_recv_large_buff_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }

    }

    /* start recursive k - ing */
    tree_order = exchange_node->tree_order;
    pow_k =  exchange_node->log_tree_order;
    for( i = 0; i < pow_k; i++) {


        /* Post ready-to-recv messages - I am here */
        for( j = 0; j <( tree_order - 1); j++) {
            comm_src = exchange_node->rank_exchanges[i][j];
            if( comm_src < 0 ){
                continue;
            }
            /* get ib subnet */
            src = comm_src; /*comm_to_ibnet[comm_src];*/

            rc = mca_bcol_iboffload_send_rtr_setup(&last_send,
                    src, iboffload_module,
                    coll_fragment);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to mca_bcol_iboffload_send_rtr_setup"));
                if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                    goto out_of_resources;
                }
                return OMPI_ERROR;
            }
        }

        /* Post receive ready-to-recev message - I can send to you */
        for( j = 0; j < (tree_order - 1); j++) {
            /* recev ready-to-receive message */
            comm_dst = exchange_node->rank_exchanges[i][j];
            /* remember, if we have extra ranks, then we won't participate
             * with a least one peer. Make a check:
             */
            if( comm_dst < 0 ){
                continue;
            }

            /* get ib subnet id */
            dst = comm_dst; /*comm_to_ibnet[comm_dst];*/
            /* post ready-to-receive receive on sender's side */
            rc = mca_bcol_iboffload_recv_rtr_setup(
                    &last_wait, dst, iboffload_module, coll_fragment);
            /* send the data */
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to"
                            "mca_bcol_iboffload_recv_rtr_setup"));
                if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                    goto out_of_resources;
                }
                return OMPI_ERROR;
            }
        }


        /* (k-1) sends */
        for( j = 0; j < (tree_order - 1); j++ ) {

            /* send phase
             */
            comm_dst = exchange_node->rank_exchanges[i][j];
            /* remember, if we have extra ranks, then we won't participate
             * with a least one peer. Make a check
             */
            if( comm_dst < 0 ){
                continue;
            }

            /* get ib subnet id */
            dst = comm_dst; /*comm_to_ibnet[comm_dst];*/
            rc = mca_bcol_iboffload_send_large_buff_setup(
                    &last_send, RBUF,
                    coll_request->buffer_info[RBUF].offset + pack_len*exchange_node->payload_info[i][j].s_offset/* offset calc */ ,
                    exchange_node->payload_info[i][j].s_len*pack_len, dst,
                    iboffload_module, coll_fragment);

            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to"
                            "mca_bcol_iboffload_send_large_buff_setup"));
                if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                    goto out_of_resources;
                }
                return OMPI_ERROR;
            }
            /* send is done */

        }

        /* we post receives after all sends in order to achieve concurrent
         * sends as well as assuring blocking until completely receiving
         * all data at level k before starting level k+1 sends
         */
        /* (k-1) receives - these are blocking */
        for( j = 0; j < (tree_order - 1); j++) {
            /*recv phase */
            comm_src = exchange_node->rank_exchanges[i][j];
            if( comm_src < 0 ){
                continue;
            }
            /* get ib subnet */
            src = comm_src; /*comm_to_ibnet[comm_src];*/

            rc = mca_bcol_iboffload_recv_large_buff_setup(&last_wait, RBUF,
                    coll_request->buffer_info[RBUF].offset + pack_len*exchange_node->payload_info[i][j].r_offset,
                    exchange_node->payload_info[i][j].r_len*pack_len, src,
                    iboffload_module, coll_fragment);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to mca_bcol_iboffload_recv_large_buff_setup"));
                if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                    goto out_of_resources;
                }
                return OMPI_ERROR;
            }



        }


    }

    /* last step, just send it back to the extra if I have one */
    if( 0 < exchange_node->n_extra_sources ) {

        comm_dst = exchange_node->rank_extra_sources_array[0];

        /* get ib subnet id */
        dst = comm_dst; /*comm_to_ibnet[comm_dst];*/
        /*
        rc = mca_bcol_iboffload_recv_rtr_setup(
                &last_wait, dst, iboffload_module, coll_fragment);

        // send the data
         we are already guaranteed that extra rank is waiting
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        "mca_bcol_iboffload_recv_rtr_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }
        */

        rc = mca_bcol_iboffload_send_large_buff_setup(
                &last_send, RBUF, coll_request->buffer_info[RBUF].offset,
                pack_len*group_size, dst,
                iboffload_module, coll_fragment);

        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        "mca_bcol_iboffload_send_large_buff_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }
        /* send is done */

    }

FINISHED:

    /* end of list */
    *coll_fragment->tail_next = NULL;

    /* finish initializing full message descriptor */
    (coll_request)->n_fragments  = 1;
    (coll_request)->n_frags_sent = 1;

    assert(NULL != last_wait);
    last_wait->flags |= MQE_WR_FLAG_SIGNAL;
    coll_fragment->signal_task_wr_id = last_wait->wr_id;
    last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;

    assert(MCA_COLL_ML_NO_BUFFER == coll_request->ml_buffer_index);
    /* post the mwr */
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload_module, coll_fragment->to_post);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload_module->super, coll_request->order_info);

    IBOFFLOAD_VERBOSE(10, ("Return success.\n"));
    return BCOL_FN_STARTED;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Allgather, adding collfrag to collfrag_pending.\n"));
    rc =
        mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload_module);
    return (OMPI_SUCCESS != rc) ? BCOL_FN_NOT_STARTED : BCOL_FN_STARTED;
}

static int mca_bcol_iboffload_k_nomial_allgather_mlbuffer_exec(mca_bcol_iboffload_module_t *iboffload_module,
                                                   mca_bcol_iboffload_collreq_t *coll_request)
{
    int rc,
        src, dst, comm_dst, comm_src, i, j;
    int tree_order, pow_k, knt;
    uint32_t pack_len;
    int my_group_index = iboffload_module->super.sbgp_partner_module->my_index;
    int group_size = iboffload_module->group_size;
    netpatterns_k_exchange_node_t *exchange_node =
                                    &iboffload_module->knomial_allgather_tree;

    struct mqe_task *last_send = NULL,
                    *last_wait = NULL;
    mca_bcol_iboffload_collfrag_t *coll_fragment = &coll_request->first_collfrag;
    int *list_connected = iboffload_module->super.list_n_connected;

    /* test test */
    int buff_offset = iboffload_module->super.hier_scather_offset;

    IBOFFLOAD_VERBOSE(10,("Entering small msg iboffload bcast"));


    if (OPAL_UNLIKELY(!iboffload_module->connection_status[ALLGATHER_KNOMIAL_ALG])) {
        IBOFFLOAD_VERBOSE(10,("Bcast open new connection "));
        bcol_iboffload_setup_allgather_endpoints_connection(iboffload_module);
    }

    pack_len = coll_request->count * coll_request->dtype->super.size;
    IBOFFLOAD_VERBOSE(10,("My packet length %d pack_len frag_count %d dtype size %d ",
                            pack_len,
                            coll_request->count,
                            coll_request->dtype->super.size));

    /* now we calculate the actual buff_offset */
    buff_offset = buff_offset*pack_len;

    /* it is estimated mq consumption... */
    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                 iboffload_module, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));
        goto out_of_resources;
    }

    coll_fragment->tail_next = &coll_fragment->to_post;
    /* we put this in to propagate the lkey into this local data structure */
    coll_request->buffer_info[SBUF].lkey = iboffload_module->rdma_block.ib_info.lkey;
    /* end hack */
    if( EXTRA_NODE == exchange_node->node_type ) {
        /* setup the rdma "send" pack_len data to proxy rank */
        comm_dst = exchange_node->rank_extra_sources_array[0];
        /* get ib subnet id */
        dst = comm_dst;
        /* now I need to calculate my own offset info */
        knt = 0;
        for( i = 0; i < my_group_index; i++){
            knt += list_connected[i];
        }

        rc = mca_bcol_iboffload_rdma_write_imm_small_buff_setup(
                &last_send, pack_len*list_connected[my_group_index],  pack_len*knt /* source offset */,
                pack_len*knt /* destination offset */, dst,
                iboffload_module, coll_fragment);
#if 0
        rc = mca_bcol_iboffload_rdma_write_imm_small_buff_setup(
                &last_send, pack_len,  pack_len*group_list[my_group_index] /* source offset */,
                pack_len*group_list[my_group_index] /* destination offset */, dst,
                iboffload_module, coll_fragment);
#endif
        /* old flow with ml offset */
#if 0
        rc = mca_bcol_iboffload_rdma_write_imm_small_buff_setup(
                &last_send, pack_len,  pack_len*group_list[my_group_index] /* source offset */,
                coll_request->buffer_info[RBUF].offset + pack_len*group_list[my_group_index] /* destination offset */, dst,
                iboffload_module, coll_fragment);
#endif
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        " mca_bcol_iboffload_send_small_buff_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }
        /* send is done */

        /* setup the rdma "receive" from proxy */
        comm_src = comm_dst;
        src = dst;
        /* more general is the number connected */
        knt = 0;
        for( i = 0; i < group_size; i++) {
            knt += list_connected[i];
        }


        rc = mca_bcol_iboffload_recv_small_buff_setup(&last_wait,
                                    pack_len*knt, src,
                                    iboffload_module, coll_fragment);

       /*
        rc = mca_bcol_iboffload_recv_small_buff_setup(&last_wait,
                                    pack_len*group_size, src,
                                    iboffload_module, coll_fragment);
        */
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to setup data receive"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }

        goto FINISHED;
    } else if( 0 < exchange_node->n_extra_sources ) {

        /* am a proxy, receive pack_len data from extra */
        comm_src = exchange_node->rank_extra_sources_array[0];
        /* get ib subnet */
        src = comm_src;
        rc = mca_bcol_iboffload_recv_small_buff_setup(&last_wait,
                                    pack_len*list_connected[src], src,
                                    iboffload_module, coll_fragment);
        /*
        rc = mca_bcol_iboffload_recv_small_buff_setup(&last_wait,
                                    pack_len, src,
                                    iboffload_module, coll_fragment);
        */
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to setup data receive"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
            }
            return OMPI_ERROR;
        }


    }

    /* start recursive k - ing */
    tree_order = exchange_node->tree_order;
    pow_k =  exchange_node->log_tree_order;
    /*fprintf(stderr,"tree order %d pow_k %d\n",tree_order,pow_k);*/
    for( i = 0; i < pow_k; i++) {
        for( j = 0; j < (tree_order - 1); j++ ) {
            /* send phase
             */
            comm_dst = exchange_node->rank_exchanges[i][j];
            /* remember, if we have extra ranks, then we won't participate
             * with a least one peer. Make a check
             */
            /*fprintf(stderr,"AAA my index %d comm_dst %d\n",my_group_index,comm_dst);*/
            if( comm_dst < 0 ){
                continue;
            }

            /* get ib subnet id */
            /* again, don't think we need this */
            /*dst = ibnet_map[comm_dst];*/
            dst = comm_dst;
            /*
            fprintf(stderr,"BBB my index %d dst %d pack len %d s_len %d src offset %d r_len %d \n",my_group_index,dst,
                    pack_len,exchange_node->payload_info[i][j].s_len,exchange_node->payload_info[i][j].s_offset,
                    exchange_node->payload_info[i][j].r_len);
            */
            /* rdma "send" setup */


            rc = mca_bcol_iboffload_rdma_write_imm_small_buff_setup(
                    &last_send, exchange_node->payload_info[i][j].s_len * pack_len,
                    exchange_node->payload_info[i][j].s_offset * pack_len /* source offset */,
                    exchange_node->payload_info[i][j].s_offset * pack_len /* destination offset */, dst,
                    iboffload_module, coll_fragment);

#if 0
            rc = mca_bcol_iboffload_rdma_write_imm_small_buff_setup(
                    &last_send, exchange_node->payload_info[i][j].s_len * pack_len,
                    exchange_node->payload_info[i][j].s_offset * exchange_node->payload_info[i][j].s_len*pack_len /* source offset */,
                    exchange_node->payload_info[i][j].s_offset * exchange_node->payload_info[i][j].s_len*pack_len /* destination offset */, dst,
                    iboffload_module, coll_fragment);
#endif

#if 0
            rc = mca_bcol_iboffload_rdma_write_imm_small_buff_setup(
                    &last_send, exchange_node->payload_info[i][j].s_len * pack_len,
                    exchange_node->payload_info[i][j].s_offset * pack_len /* source offset */,
                    exchange_node->payload_info[i][j].s_offset * pack_len /* destination offset */, dst,
                    iboffload_module, coll_fragment);
#endif
#if 0
            rc = mca_bcol_iboffload_rdma_write_imm_small_buff_setup(
                    &last_send, exchange_node->payload_info[i][j].s_len * pack_len,
                    coll_request->buffer_info[SBUF].offset + exchange_node->payload_info[i][j].s_offset * pack_len /* source offset */,
                    coll_request->buffer_info[SBUF].offset + exchange_node->payload_info[i][j].s_offset * pack_len /* destination offset */, dst,
                    iboffload_module, coll_fragment);
#endif
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to"
                            " mca_bcol_iboffload_send_small_buff_setup"));
                if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                    goto out_of_resources;
                }
                return OMPI_ERROR;
            }

            /* send is done */
        }

       for( j = 0; j < (tree_order - 1); j++) {

            /* rdma "recv" phase */
           comm_src = exchange_node->rank_exchanges[i][j];
           /* remember, if we have extra ranks, then we won't participate
            * with a least one peer. Make a check
            */
           if( comm_src < 0 ){
               continue;
           }

           /* get ib subnet id */
           /* shouldn't need this */
           src = comm_src;

           rc = mca_bcol_iboffload_recv_small_buff_setup(&last_wait,
                    exchange_node->payload_info[i][j].r_len * pack_len, src,
                    iboffload_module, coll_fragment);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to setup data receive"));
                if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                    goto out_of_resources;
                }
                return OMPI_ERROR;
            }

        }
    }

    /* last step, proxies send full data back to the extra ranks */
    if( 0 < exchange_node->n_extra_sources ) {
        /* send pack_len data to proxy */
        comm_dst = exchange_node->rank_extra_sources_array[0];
        /* get ibnet id */
        dst = comm_dst;

        knt = 0;
        for( i = 0; i < group_size; i++){
            knt += list_connected[i];
        }

        rc = mca_bcol_iboffload_rdma_write_imm_small_buff_setup(
                &last_send, pack_len*knt, 0 /* source offset */,
                0 /* destination offset */, dst,
                iboffload_module, coll_fragment);
#if 0
        rc = mca_bcol_iboffload_rdma_write_imm_small_buff_setup(
                &last_send, pack_len*group_size, 0 /* source offset */,
                0 /* destination offset */, dst,
                iboffload_module, coll_fragment);
#endif
#if 0
        rc = mca_bcol_iboffload_rdma_write_imm_small_buff_setup(
                &last_send, pack_len*group_size, coll_request->buffer_info[RBUF].offset /* source offset */,
                coll_request->buffer_info[SBUF].offset /* destination offset */, dst,
                iboffload_module, coll_fragment);
#endif
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        " mca_bcol_iboffload_send_small_buff_setup"));
            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc){
                goto out_of_resources;
                fprintf(stderr,"I'm out of resources \n");
            }
            return OMPI_ERROR;
        }
        /* send is done */

    }

FINISHED:

    /* end of list */
    *coll_fragment->tail_next = NULL;

    /* finish initializing full message descriptor */
    (coll_request)->n_fragments  = 1;
    (coll_request)->n_frags_sent = 1;

    assert(NULL != last_wait);
    last_wait->flags |= MQE_WR_FLAG_SIGNAL;
    coll_fragment->signal_task_wr_id = last_wait->wr_id;
    last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;

    assert(MCA_COLL_ML_NO_BUFFER != coll_request->ml_buffer_index);
    /* post the mwr */
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload_module, coll_fragment->to_post);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload_module->super, coll_request->order_info);

    IBOFFLOAD_VERBOSE(10, ("Return success.\n"));
    return BCOL_FN_STARTED;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Allgather, adding collfrag to collfrag_pending.\n"));
    rc =
        mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload_module);
    return (OMPI_SUCCESS != rc) ? BCOL_FN_NOT_STARTED : BCOL_FN_STARTED;
}

#if 0
static int mca_bcol_iboffload_neighbor_allgather_userbuffer_intra(
                                            bcol_function_args_t *fn_arguments,
                                            struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_iboffload_module_t *iboffload_module =
        (mca_bcol_iboffload_module_t *)const_args->bcol_module;

    int rc;
    int mq_credits = iboffload_module->group_size * 2 * 2; /* large message protocol consumes
                                                            * twice as many mq credits
                                                            */

    bool if_bcol_last = BCOL_IBOFFLOAD_IS_LAST_CALL(const_args);
    mca_bcol_iboffload_collreq_t *coll_request;

    MCA_BCOL_CHECK_ORDER(const_args->bcol_module, fn_arguments);

    rc = mca_bcol_iboffload_allgather_init(fn_arguments, iboffload_module,
            &coll_request, if_bcol_last, mq_credits,
            mca_bcol_iboffload_neighbor_allgather_userbuffer_exec);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = coll_request->progress_fn(iboffload_module, coll_request);

    IBOFFLOAD_VERBOSE(10, ("mca_bcol_iboffload_k_nomial_allgather_userbuffer_intra was started [%d]\n", rc));
    return rc;
}
#endif

#if 1
static int mca_bcol_iboffload_k_nomial_allgather_userbuffer_intra(bcol_function_args_t *fn_arguments,
                                                   struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_iboffload_module_t *iboffload_module =
        (mca_bcol_iboffload_module_t *)const_args->bcol_module;

    int rc;
    int mq_credits = ((iboffload_module->knomial_allgather_tree.tree_order - 1)*
                       iboffload_module->knomial_allgather_tree.log_tree_order + 1) * 2 * 2; /* large message protocol
                                                                                              * consumes twice as much
                                                                                              */

    bool if_bcol_last = BCOL_IBOFFLOAD_IS_LAST_CALL(const_args);
    mca_bcol_iboffload_collreq_t *coll_request;

    MCA_BCOL_CHECK_ORDER(const_args->bcol_module, fn_arguments);

    rc = mca_bcol_iboffload_allgather_init(fn_arguments, iboffload_module,
            &coll_request, if_bcol_last, mq_credits,
            mca_bcol_iboffload_k_nomial_allgather_userbuffer_exec);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = coll_request->progress_fn(iboffload_module, coll_request);

    IBOFFLOAD_VERBOSE(10, ("mca_bcol_iboffload_k_nomial_allgather_userbuffer_intra was started [%d]\n", rc));
    return rc;
}
#endif

static int mca_bcol_iboffload_k_nomial_allgather_mlbuffer_intra(bcol_function_args_t *fn_arguments,
                                                   struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_iboffload_module_t *iboffload_module =
        (mca_bcol_iboffload_module_t *)const_args->bcol_module;

    int rc;

    /* I'll add one for everyone, since nobody wants to feel left out */
    int mq_credits = ((iboffload_module->knomial_allgather_tree.tree_order - 1)*
                       iboffload_module->knomial_allgather_tree.log_tree_order + 1) * 2 ;
    bool if_bcol_last = BCOL_IBOFFLOAD_IS_LAST_CALL(const_args);
    mca_bcol_iboffload_collreq_t *coll_request;

    MCA_BCOL_CHECK_ORDER(const_args->bcol_module, fn_arguments);

    rc = mca_bcol_iboffload_allgather_init(fn_arguments, iboffload_module,
            &coll_request, if_bcol_last, mq_credits,
            mca_bcol_iboffload_k_nomial_allgather_mlbuffer_exec);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = coll_request->progress_fn(iboffload_module, coll_request);

    IBOFFLOAD_VERBOSE(10, ("mca_bcol_iboffload_small_msg_bcast_intra was started [%d]\n", rc));
    return rc;
}


/* these progress engines are shared between alltoall and allgather and exist in both files,
 * should be moved to a common .h file
 */
static int mca_bcol_iboffload_collreq_mlbuffer_progress(
            bcol_function_args_t *input_args,
            struct mca_bcol_base_function_t *const_args)
{
    int i;
    mca_bcol_iboffload_collreq_t *coll_request =
         (mca_bcol_iboffload_collreq_t *)
                   input_args->bcol_opaque_data;
    IBOFFLOAD_VERBOSE(10, ("Run progress (ml buffer).\n"));
    for (i = 0; i < mca_bcol_iboffload_component.max_progress_pull; i++) {
    if (BCOL_IS_COMPLETED(coll_request)) {

        coll_request->user_handle_freed = true;

        if (COLLREQ_IS_DONE(coll_request)) {
        IBOFFLOAD_VERBOSE(10, ("Coll request already done.\n"));
        RELEASE_COLLREQ(coll_request);
        }
        IBOFFLOAD_VERBOSE(10, ("Collective finished (ml buffer).\n"));

        return BCOL_FN_COMPLETE;
    }
    }
    IBOFFLOAD_VERBOSE(10, ("Collective not finished (ml buffer).\n"));
    return BCOL_FN_STARTED;
}


static int mca_bcol_iboffload_collreq_userbuffer_progress(
                        bcol_function_args_t *input_args,
                        struct mca_bcol_base_function_t *const_args)
{
    int i;
    mca_bcol_iboffload_collreq_t *coll_request =
                 (mca_bcol_iboffload_collreq_t *)
                                   input_args->bcol_opaque_data;

    IBOFFLOAD_VERBOSE(10, ("Run progress (user buffer)\n"));

    /* Complete the allgather - progress releases full request descriptors */

    for (i = 0; i < mca_bcol_iboffload_component.max_progress_pull; i++) {
        if (coll_request->n_frag_mpi_complete == coll_request->n_fragments &&
            coll_request->n_frag_net_complete == coll_request->n_fragments) {

            IBOFFLOAD_VERBOSE(10, ("Deregister user buff.\n"));

            if (NULL != coll_request->buffer_info[SBUF].iboffload_reg) {
                coll_request->module->device->mpool->mpool_deregister(
                        coll_request->module->device->mpool,
                        (mca_mpool_base_registration_t *) coll_request->buffer_info[SBUF].iboffload_reg);
                coll_request->buffer_info[SBUF].iboffload_reg = NULL;
            }


            if (NULL != coll_request->buffer_info[RBUF].iboffload_reg) {
                coll_request->module->device->mpool->mpool_deregister(
                        coll_request->module->device->mpool,
                        (mca_mpool_base_registration_t *) coll_request->buffer_info[RBUF].iboffload_reg);
                coll_request->buffer_info[RBUF].iboffload_reg = NULL;
            }

            RELEASE_COLLREQ(coll_request);
            IBOFFLOAD_VERBOSE(10, ("New bcast done !!!"));
            return BCOL_FN_COMPLETE;
        }
    }

    IBOFFLOAD_VERBOSE(10, ("Collective finished (user buffer).\n"));

    /* We are not done */
    return BCOL_FN_STARTED;
}

int mca_bcol_iboffload_allgather_register(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    IBOFFLOAD_VERBOSE(10, ("Register iboffload Allgather.\n"));
    comm_attribs.bcoll_type = BCOL_ALLGATHER;

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
            mca_bcol_iboffload_k_nomial_allgather_mlbuffer_intra,
            mca_bcol_iboffload_collreq_mlbuffer_progress);

    inv_attribs.bcol_msg_min = 10000000;
    inv_attribs.bcol_msg_max = 10485760; /* range 4 */


    /* zero-copy k-nomial algorithm */
#if 1
    mca_bcol_base_set_attributes(super,
            &comm_attribs, &inv_attribs,
            mca_bcol_iboffload_k_nomial_allgather_userbuffer_intra,
            mca_bcol_iboffload_collreq_userbuffer_progress);
#endif
    /* zero-copy neighbor exchange algorithm */
#if 0
    mca_bcol_base_set_attributes(super,
            &comm_attribs, &inv_attribs,
            mca_bcol_iboffload_neighbor_allgather_userbuffer_intra,
            mca_bcol_iboffload_collreq_userbuffer_progress);
#endif
    return OMPI_SUCCESS;
}
