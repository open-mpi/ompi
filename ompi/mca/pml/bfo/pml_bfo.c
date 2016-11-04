/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2006-2008 University of Houston.  All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "opal/class/opal_bitmap.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/pmix/pmix.h"

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/runtime/ompi_cr.h"

#include "pml_bfo.h"
#include "pml_bfo_component.h"
#include "pml_bfo_comm.h"
#include "pml_bfo_hdr.h"
#include "pml_bfo_recvfrag.h"
#include "pml_bfo_sendreq.h"
#include "pml_bfo_recvreq.h"
#include "pml_bfo_rdmafrag.h"
#if PML_BFO
#include "pml_bfo_failover.h"
#endif /* PML_BFO */

mca_pml_bfo_t mca_pml_bfo = {
    {
        mca_pml_bfo_add_procs,
        mca_pml_bfo_del_procs,
        mca_pml_bfo_enable,
        mca_pml_bfo_progress,
        mca_pml_bfo_add_comm,
        mca_pml_bfo_del_comm,
        mca_pml_bfo_irecv_init,
        mca_pml_bfo_irecv,
        mca_pml_bfo_recv,
        mca_pml_bfo_isend_init,
        mca_pml_bfo_isend,
        mca_pml_bfo_send,
        mca_pml_bfo_iprobe,
        mca_pml_bfo_probe,
        mca_pml_bfo_start,
        mca_pml_bfo_improbe,
        mca_pml_bfo_mprobe,
        mca_pml_bfo_imrecv,
        mca_pml_bfo_mrecv,
        mca_pml_bfo_dump,
        mca_pml_bfo_ft_event,
        65535,
        INT_MAX
    }
};


void mca_pml_bfo_error_handler( struct mca_btl_base_module_t* btl,
                                int32_t flags, ompi_proc_t* errproc,
                                char* btlinfo );

int mca_pml_bfo_enable(bool enable)
{
    if( false == enable ) {
        return OMPI_SUCCESS;
    }

    OBJ_CONSTRUCT(&mca_pml_bfo.lock, opal_mutex_t);

    /* fragments */
    OBJ_CONSTRUCT(&mca_pml_bfo.rdma_frags, opal_free_list_t);
    opal_free_list_init( &mca_pml_bfo.rdma_frags,
                         sizeof(mca_pml_bfo_rdma_frag_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_bfo_rdma_frag_t),
                         0,opal_cache_line_size,
                         mca_pml_bfo.free_list_num,
                         mca_pml_bfo.free_list_max,
                         mca_pml_bfo.free_list_inc,
                         NULL, 0, NULL, NULL, NULL );

    OBJ_CONSTRUCT(&mca_pml_bfo.recv_frags, opal_free_list_t);
    opal_free_list_init( &mca_pml_bfo.recv_frags,
                         sizeof(mca_pml_bfo_recv_frag_t) + mca_pml_bfo.unexpected_limit,
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_bfo_recv_frag_t),
                         0,opal_cache_line_size,
                         mca_pml_bfo.free_list_num,
                         mca_pml_bfo.free_list_max,
                         mca_pml_bfo.free_list_inc,
                         NULL, 0, NULL, NULL, NULL );

    OBJ_CONSTRUCT(&mca_pml_bfo.pending_pckts, opal_free_list_t);
    opal_free_list_init( &mca_pml_bfo.pending_pckts,
                         sizeof(mca_pml_bfo_pckt_pending_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_bfo_pckt_pending_t),
                         0,opal_cache_line_size,
                         mca_pml_bfo.free_list_num,
                         mca_pml_bfo.free_list_max,
                         mca_pml_bfo.free_list_inc,
                         NULL, 0, NULL, NULL, NULL );

    OBJ_CONSTRUCT(&mca_pml_bfo.buffers, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_bfo.send_ranges, opal_free_list_t);
    opal_free_list_init( &mca_pml_bfo.send_ranges,
                         sizeof(mca_pml_bfo_send_range_t) +
                         (mca_pml_bfo.max_send_per_range - 1) * sizeof(mca_pml_bfo_com_btl_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_bfo_send_range_t),
                         0,opal_cache_line_size,
                         mca_pml_bfo.free_list_num,
                         mca_pml_bfo.free_list_max,
                         mca_pml_bfo.free_list_inc,
                         NULL, 0, NULL, NULL, NULL );

    /* pending operations */
    OBJ_CONSTRUCT(&mca_pml_bfo.send_pending, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_bfo.recv_pending, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_bfo.pckt_pending, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_bfo.rdma_pending, opal_list_t);
    /* missing communicator pending list */
    OBJ_CONSTRUCT(&mca_pml_bfo.non_existing_communicator_pending, opal_list_t);

    /**
     * If we get here this is the PML who get selected for the run. We
     * should get ownership for the send and receive requests list, and
     * initialize them with the size of our own requests.
     */
    opal_free_list_init( &mca_pml_base_send_requests,
                         sizeof(mca_pml_bfo_send_request_t) +
                         (mca_pml_bfo.max_rdma_per_request - 1) *
                         sizeof(mca_pml_bfo_com_btl_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_bfo_send_request_t),
                         0,opal_cache_line_size,
                         mca_pml_bfo.free_list_num,
                         mca_pml_bfo.free_list_max,
                         mca_pml_bfo.free_list_inc,
                         NULL, 0, NULL, NULL, NULL );

    opal_free_list_init( &mca_pml_base_recv_requests,
                         sizeof(mca_pml_bfo_recv_request_t) +
                         (mca_pml_bfo.max_rdma_per_request - 1) *
                         sizeof(mca_pml_bfo_com_btl_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_bfo_recv_request_t),
                         0,opal_cache_line_size,
                         mca_pml_bfo.free_list_num,
                         mca_pml_bfo.free_list_max,
                         mca_pml_bfo.free_list_inc,
                         NULL, 0, NULL, NULL, NULL );

    mca_pml_bfo.enabled = true;
    return OMPI_SUCCESS;
}

int mca_pml_bfo_add_comm(ompi_communicator_t* comm)
{
    /* allocate pml specific comm data */
    mca_pml_bfo_comm_t* pml_comm = OBJ_NEW(mca_pml_bfo_comm_t);
    opal_list_item_t *item, *next_item;
    mca_pml_bfo_recv_frag_t* frag;
    mca_pml_bfo_comm_proc_t* pml_proc;
    mca_pml_bfo_match_hdr_t* hdr;
    int i;

    if (NULL == pml_comm) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* should never happen, but it was, so check */
    if (comm->c_contextid > mca_pml_bfo.super.pml_max_contextid) {
        OBJ_RELEASE(pml_comm);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    mca_pml_bfo_comm_init_size(pml_comm, comm->c_remote_group->grp_proc_count);
    comm->c_pml_comm = pml_comm;

    for( i = 0; i < comm->c_remote_group->grp_proc_count; i++ ) {
        pml_comm->procs[i].ompi_proc = ompi_group_peer_lookup(comm->c_remote_group,i);
        OBJ_RETAIN(pml_comm->procs[i].ompi_proc);
    }
    /* Grab all related messages from the non_existing_communicator pending queue */
    for( item = opal_list_get_first(&mca_pml_bfo.non_existing_communicator_pending);
         item != opal_list_get_end(&mca_pml_bfo.non_existing_communicator_pending);
         item = next_item ) {
        frag = (mca_pml_bfo_recv_frag_t*)item;
        next_item = opal_list_get_next(item);
        hdr = &frag->hdr.hdr_match;

        /* Is this fragment for the current communicator ? */
        if( frag->hdr.hdr_match.hdr_ctx != comm->c_contextid )
            continue;

        /* As we now know we work on a fragment for this communicator
         * we should remove it from the
         * non_existing_communicator_pending list. */
        opal_list_remove_item( &mca_pml_bfo.non_existing_communicator_pending,
                               item );

      add_fragment_to_unexpected:

        /* We generate the MSG_ARRIVED event as soon as the PML is aware
         * of a matching fragment arrival. Independing if it is received
         * on the correct order or not. This will allow the tools to
         * figure out if the messages are not received in the correct
         * order (if multiple network interfaces).
         */
        PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_MSG_ARRIVED, comm,
                               hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);

        /* There is no matching to be done, and no lock to be held on the communicator as
         * we know at this point that the communicator has not yet been returned to the user.
         * The only required protection is around the non_existing_communicator_pending queue.
         * We just have to push the fragment into the unexpected list of the corresponding
         * proc, or into the out-of-order (cant_match) list.
         */
        pml_proc = &(pml_comm->procs[hdr->hdr_src]);

        if( ((uint16_t)hdr->hdr_seq) == ((uint16_t)pml_proc->expected_sequence) ) {
            /* We're now expecting the next sequence number. */
            pml_proc->expected_sequence++;
            opal_list_append( &pml_proc->unexpected_frags, (opal_list_item_t*)frag );
            PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_MSG_INSERT_IN_UNEX_Q, comm,
                                   hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);
            /* And now the ugly part. As some fragments can be inserted in the cant_match list,
             * every time we succesfully add a fragment in the unexpected list we have to make
             * sure the next one is not in the cant_match. Otherwise, we will endup in a deadlock
             * situation as the cant_match is only checked when a new fragment is received from
             * the network.
             */
           for(frag = (mca_pml_bfo_recv_frag_t *)opal_list_get_first(&pml_proc->frags_cant_match);
               frag != (mca_pml_bfo_recv_frag_t *)opal_list_get_end(&pml_proc->frags_cant_match);
               frag = (mca_pml_bfo_recv_frag_t *)opal_list_get_next(frag)) {
               hdr = &frag->hdr.hdr_match;
               /* If the message has the next expected seq from that proc...  */
               if(hdr->hdr_seq != pml_proc->expected_sequence)
                   continue;

               opal_list_remove_item(&pml_proc->frags_cant_match, (opal_list_item_t*)frag);
               goto add_fragment_to_unexpected;
           }
        } else {
            opal_list_append( &pml_proc->frags_cant_match, (opal_list_item_t*)frag );
        }
    }
    return OMPI_SUCCESS;
}

int mca_pml_bfo_del_comm(ompi_communicator_t* comm)
{
    mca_pml_bfo_comm_t* pml_comm = comm->c_pml_comm;
    int i;

    for( i = 0; i < comm->c_remote_group->grp_proc_count; i++ ) {
        OBJ_RELEASE(pml_comm->procs[i].ompi_proc);
    }
    OBJ_RELEASE(comm->c_pml_comm);
    comm->c_pml_comm = NULL;
    return OMPI_SUCCESS;
}


/*
 *   For each proc setup a datastructure that indicates the BTLs
 *   that can be used to reach the destination.
 *
 */

int mca_pml_bfo_add_procs(ompi_proc_t** procs, size_t nprocs)
{
    opal_bitmap_t reachable;
    int rc;
    opal_list_item_t *item;

    if(nprocs == 0)
        return OMPI_SUCCESS;

    OBJ_CONSTRUCT(&reachable, opal_bitmap_t);
    rc = opal_bitmap_init(&reachable, (int)nprocs);
    if(OMPI_SUCCESS != rc)
        return rc;

    /*
     * JJH: Disable this in FT enabled builds since
     * we use a wrapper PML. It will cause this check to
     * return failure as all processes will return the wrapper PML
     * component in use instead of the wrapped PML component underneath.
     */
#if OPAL_ENABLE_FT_CR == 0
    /* make sure remote procs are using the same PML as us */
    if (OMPI_SUCCESS != (rc = mca_pml_base_pml_check_selected("bfo",
                                                              procs,
                                                              nprocs))) {
        return rc;
    }
#endif

    rc = mca_bml.bml_add_procs( nprocs,
                                procs,
                                &reachable );
    if(OMPI_SUCCESS != rc)
        goto cleanup_and_return;

    /* Check that values supplied by all initialized btls will work
       for us.  Note that this is the list of all initialized BTLs,
       not the ones used for the just added procs.  This is a little
       overkill and inaccurate, as we may end up not using the BTL in
       question and all add_procs calls after the first one are
       duplicating an already completed check.  But the final
       initialization of the PML occurs before the final
       initialization of the BTLs, and iterating through the in-use
       BTLs requires iterating over the procs, as the BML does not
       expose all currently in use btls. */

    for (item = opal_list_get_first(&mca_btl_base_modules_initialized) ;
         item != opal_list_get_end(&mca_btl_base_modules_initialized) ;
         item = opal_list_get_next(item)) {
        mca_btl_base_selected_module_t *sm =
            (mca_btl_base_selected_module_t*) item;
        if (sm->btl_module->btl_eager_limit < sizeof(mca_pml_bfo_hdr_t)) {
	    opal_show_help("help-mpi-pml-bfo.txt", "eager_limit_too_small",
			   true,
			   sm->btl_component->btl_version.mca_component_name,
			   ompi_process_info.nodename,
			   sm->btl_component->btl_version.mca_component_name,
			   sm->btl_module->btl_eager_limit,
			   sm->btl_component->btl_version.mca_component_name,
			   sizeof(mca_pml_bfo_hdr_t),
			   sm->btl_component->btl_version.mca_component_name);
            rc = OMPI_ERR_BAD_PARAM;
            goto cleanup_and_return;
        }
    }


    /* TODO: Move these callback registration to another place */
    rc = mca_bml.bml_register( MCA_PML_BFO_HDR_TYPE_MATCH,
                               mca_pml_bfo_recv_frag_callback_match,
                               NULL );
    if(OMPI_SUCCESS != rc)
        goto cleanup_and_return;

    rc = mca_bml.bml_register( MCA_PML_BFO_HDR_TYPE_RNDV,
                               mca_pml_bfo_recv_frag_callback_rndv,
                               NULL );
    if(OMPI_SUCCESS != rc)
        goto cleanup_and_return;

    rc = mca_bml.bml_register( MCA_PML_BFO_HDR_TYPE_RGET,
                               mca_pml_bfo_recv_frag_callback_rget,
                               NULL );
    if(OMPI_SUCCESS != rc)
        goto cleanup_and_return;

    rc = mca_bml.bml_register( MCA_PML_BFO_HDR_TYPE_ACK,
                               mca_pml_bfo_recv_frag_callback_ack,
                               NULL );
    if(OMPI_SUCCESS != rc)
        goto cleanup_and_return;

    rc = mca_bml.bml_register( MCA_PML_BFO_HDR_TYPE_FRAG,
                               mca_pml_bfo_recv_frag_callback_frag,
                               NULL );
    if(OMPI_SUCCESS != rc)
        goto cleanup_and_return;

    rc = mca_bml.bml_register( MCA_PML_BFO_HDR_TYPE_PUT,
                               mca_pml_bfo_recv_frag_callback_put,
                               NULL );
    if(OMPI_SUCCESS != rc)
        goto cleanup_and_return;

    rc = mca_bml.bml_register( MCA_PML_BFO_HDR_TYPE_FIN,
                               mca_pml_bfo_recv_frag_callback_fin,
                               NULL );
    if(OMPI_SUCCESS != rc)
        goto cleanup_and_return;

#if PML_BFO
    rc = mca_pml_bfo_register_callbacks();
    if(OMPI_SUCCESS != rc)
        goto cleanup_and_return;
#endif /* PML_BFO */
    /* register error handlers */
    rc = mca_bml.bml_register_error((mca_btl_base_module_error_cb_fn_t)mca_pml_bfo_error_handler);
    if(OMPI_SUCCESS != rc)
        goto cleanup_and_return;

  cleanup_and_return:
    OBJ_DESTRUCT(&reachable);

    return rc;
}

/*
 * iterate through each proc and notify any PTLs associated
 * with the proc that it is/has gone away
 */

int mca_pml_bfo_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    return mca_bml.bml_del_procs(nprocs, procs);
}

/*
 * diagnostics
 */

int mca_pml_bfo_dump(struct ompi_communicator_t* comm, int verbose)
{
    struct mca_pml_comm_t* pml_comm = comm->c_pml_comm;
    int i;

    /* iterate through all procs on communicator */
    for( i = 0; i < (int)pml_comm->num_procs; i++ ) {
        mca_pml_bfo_comm_proc_t* proc = &pml_comm->procs[i];
        mca_bml_base_endpoint_t* ep = (mca_bml_base_endpoint_t*)proc->ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];
        size_t n;

        opal_output(0, "[Rank %d]\n", i);
        /* dump all receive queues */

        /* dump all btls */
        for(n=0; n<ep->btl_eager.arr_size; n++) {
            mca_bml_base_btl_t* bml_btl = &ep->btl_eager.bml_btls[n];
            bml_btl->btl->btl_dump(bml_btl->btl, bml_btl->btl_endpoint, verbose);
        }
    }
    return OMPI_SUCCESS;
}

static void mca_pml_bfo_fin_completion( mca_btl_base_module_t* btl,
                                        struct mca_btl_base_endpoint_t* ep,
                                        struct mca_btl_base_descriptor_t* des,
                                        int status )
{

    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context;

#if PML_BFO
    if( OPAL_UNLIKELY(OMPI_SUCCESS != status) ) {
        mca_pml_bfo_repost_fin(des);
        return;
    }
    MCA_PML_BFO_CHECK_EAGER_BML_BTL_ON_FIN_COMPLETION(bml_btl, btl, des);
#endif /* PML_BFO */
    /* check for pending requests */
    MCA_PML_BFO_PROGRESS_PENDING(bml_btl);
}

/**
 * Send an FIN to the peer. If we fail to send this ack (no more available
 * fragments or the send failed) this function automatically add the FIN
 * to the list of pending FIN, Which guarantee that the FIN will be sent
 * later.
 */
int mca_pml_bfo_send_fin( ompi_proc_t* proc,
                          mca_bml_base_btl_t* bml_btl,
                          opal_ptr_t hdr_des,
                          uint8_t order,
#if PML_BFO
                          uint32_t status,
                          uint16_t seq,
                          uint8_t restartseq,
                          uint16_t ctx, uint32_t src)
#else /* PML_BFO */
                          uint32_t status )
#endif /* PML_BFO */
{
    mca_btl_base_descriptor_t* fin;
    mca_pml_bfo_fin_hdr_t* hdr;
    int rc;

    mca_bml_base_alloc(bml_btl, &fin, order, sizeof(mca_pml_bfo_fin_hdr_t),
                       MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

    if(NULL == fin) {
        MCA_PML_BFO_ADD_FIN_TO_PENDING(proc, hdr_des, bml_btl, order, status);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    fin->des_cbfunc = mca_pml_bfo_fin_completion;
    fin->des_cbdata = NULL;

    /* fill in header */
    hdr = (mca_pml_bfo_fin_hdr_t*)fin->des_local->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_FIN;
    hdr->hdr_des = hdr_des;
    hdr->hdr_fail = status;
#if PML_BFO
    fin->des_cbdata = proc;
    hdr->hdr_match.hdr_seq = seq;
    hdr->hdr_match.hdr_ctx = ctx;
    hdr->hdr_match.hdr_src = src;
    hdr->hdr_match.hdr_common.hdr_flags = restartseq;  /* use unused hdr_flags field */
#endif /* PML_BFO */

    bfo_hdr_hton(hdr, MCA_PML_BFO_HDR_TYPE_FIN, proc);

    /* queue request */
    rc = mca_bml_base_send( bml_btl,
                            fin,
                            MCA_PML_BFO_HDR_TYPE_FIN );
    if( OPAL_LIKELY( rc >= 0 ) ) {
        if( OPAL_LIKELY( 1 == rc ) ) {
            MCA_PML_BFO_PROGRESS_PENDING(bml_btl);
        }
        return OMPI_SUCCESS;
    }
    mca_bml_base_free(bml_btl, fin);
    MCA_PML_BFO_ADD_FIN_TO_PENDING(proc, hdr_des, bml_btl, order, status);
    return OMPI_ERR_OUT_OF_RESOURCE;
}

void mca_pml_bfo_process_pending_packets(mca_bml_base_btl_t* bml_btl)
{
    mca_pml_bfo_pckt_pending_t *pckt;
    int32_t i, rc, s = (int32_t)opal_list_get_size(&mca_pml_bfo.pckt_pending);

    for(i = 0; i < s; i++) {
        mca_bml_base_btl_t *send_dst = NULL;
        OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
        pckt = (mca_pml_bfo_pckt_pending_t*)
            opal_list_remove_first(&mca_pml_bfo.pckt_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
        if(NULL == pckt)
            break;
        if(pckt->bml_btl != NULL &&
                pckt->bml_btl->btl == bml_btl->btl) {
            send_dst = pckt->bml_btl;
        } else {
            mca_bml_base_endpoint_t* endpoint =
                (mca_bml_base_endpoint_t*) pckt->proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];
            send_dst = mca_bml_base_btl_array_find(
                    &endpoint->btl_eager, bml_btl->btl);
        }
        if(NULL == send_dst) {
            OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
            opal_list_append(&mca_pml_bfo.pckt_pending,
                             (opal_list_item_t*)pckt);
            OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
            continue;
        }

        switch(pckt->hdr.hdr_common.hdr_type) {
            case MCA_PML_BFO_HDR_TYPE_ACK:
                rc = mca_pml_bfo_recv_request_ack_send_btl(pckt->proc,
                        send_dst,
                        pckt->hdr.hdr_ack.hdr_src_req.lval,
                        pckt->hdr.hdr_ack.hdr_dst_req.pval,
                        pckt->hdr.hdr_ack.hdr_send_offset,
                        pckt->hdr.hdr_common.hdr_flags & MCA_PML_BFO_HDR_FLAGS_NORDMA);
                if( OPAL_UNLIKELY(OMPI_ERR_OUT_OF_RESOURCE == rc) ) {
                    OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
                    opal_list_append(&mca_pml_bfo.pckt_pending,
                                     (opal_list_item_t*)pckt);
                    OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
                    return;
                }
                break;
            case MCA_PML_BFO_HDR_TYPE_FIN:
                rc = mca_pml_bfo_send_fin(pckt->proc, send_dst,
                                          pckt->hdr.hdr_fin.hdr_des,
                                          pckt->order,
#if PML_BFO
                                          pckt->hdr.hdr_fin.hdr_fail,
                                          pckt->hdr.hdr_fin.hdr_match.hdr_seq,
                                          pckt->hdr.hdr_fin.hdr_match.hdr_common.hdr_flags,
                                          pckt->hdr.hdr_fin.hdr_match.hdr_ctx,
                                          pckt->hdr.hdr_fin.hdr_match.hdr_src);
#else /* PML_BFO */
                                          pckt->hdr.hdr_fin.hdr_fail);
#endif /* PML_BFO */
                if( OPAL_UNLIKELY(OMPI_ERR_OUT_OF_RESOURCE == rc) ) {
                    return;
                }
                break;
            default:
                opal_output(0, "[%s:%d] wrong header type\n",
                            __FILE__, __LINE__);
                break;
        }
        /* We're done with this packet, return it back to the free list */
        MCA_PML_BFO_PCKT_PENDING_RETURN(pckt);
    }
}

void mca_pml_bfo_process_pending_rdma(void)
{
    mca_pml_bfo_rdma_frag_t* frag;
    int32_t i, rc, s = (int32_t)opal_list_get_size(&mca_pml_bfo.rdma_pending);

    for(i = 0; i < s; i++) {
        OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
        frag = (mca_pml_bfo_rdma_frag_t*)
            opal_list_remove_first(&mca_pml_bfo.rdma_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
        if(NULL == frag)
            break;
        if(frag->rdma_state == MCA_PML_BFO_RDMA_PUT) {
            frag->retries++;
            rc = mca_pml_bfo_send_request_put_frag(frag);
        } else {
            rc = mca_pml_bfo_recv_request_get_frag(frag);
        }
        if(OMPI_ERR_OUT_OF_RESOURCE == rc)
            break;
    }
}


void mca_pml_bfo_error_handler(
        struct mca_btl_base_module_t* btl, int32_t flags,
        ompi_proc_t* errproc, char* btlinfo ) {
#if PML_BFO
    if (flags & MCA_BTL_ERROR_FLAGS_NONFATAL) {
        mca_pml_bfo_failover_error_handler(btl, flags, errproc, btlinfo);
        return;
    }
#endif /* PML_BFO */
    ompi_rte_abort(-1, NULL);
}

#if OPAL_ENABLE_FT_CR    == 0
int mca_pml_bfo_ft_event( int state ) {
    return OMPI_SUCCESS;
}
#else
int mca_pml_bfo_ft_event( int state )
{
    static bool first_continue_pass = false;
    ompi_proc_t** procs = NULL;
    size_t num_procs;
    int ret, p;

    if(OPAL_CRS_CHECKPOINT == state) {
        if( opal_cr_timing_barrier_enabled ) {
            OPAL_CR_SET_TIMER(OPAL_CR_TIMER_CRCPBR1);
            opal_pmix.fence(NULL, 0);
        }

        OPAL_CR_SET_TIMER(OPAL_CR_TIMER_P2P0);
    }
    else if(OPAL_CRS_CONTINUE == state) {
        first_continue_pass = !first_continue_pass;

        if( !first_continue_pass ) {
            if( opal_cr_timing_barrier_enabled ) {
                OPAL_CR_SET_TIMER(OPAL_CR_TIMER_COREBR0);
                opal_pmix.fence(NULL, 0);
            }
            OPAL_CR_SET_TIMER(OPAL_CR_TIMER_P2P2);
        }

        if (opal_cr_continue_like_restart && !first_continue_pass) {
            /*
             * Get a list of processes
             */
            procs = ompi_proc_all(&num_procs);
            if(NULL == procs) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            /*
             * Refresh the proc structure, and publish our proc info in the modex.
             * NOTE: Do *not* call ompi_proc_finalize as there are many places in
             *       the code that point to indv. procs in this strucutre. For our
             *       needs here we only need to fix up the modex, bml and pml
             *       references.
             */
            if (OMPI_SUCCESS != (ret = ompi_proc_refresh())) {
                opal_output(0,
                            "pml:bfo: ft_event(Restart): proc_refresh Failed %d",
                            ret);
                for(p = 0; p < (int)num_procs; ++p) {
                    OBJ_RELEASE(procs[p]);
                }
                free (procs);
                return ret;
            }
        }
    }
    else if(OPAL_CRS_RESTART_PRE == state ) {
        /* Nothing here */
    }
    else if(OPAL_CRS_RESTART == state ) {
        /*
         * Get a list of processes
         */
        procs = ompi_proc_all(&num_procs);
        if(NULL == procs) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Clean out the modex information since it is invalid now.
         *    ompi_rte_purge_proc_attrs();
         * This happens at the ORTE level, so doing it again here will cause
         * some issues with socket caching.
         */


        /*
         * Refresh the proc structure, and publish our proc info in the modex.
         * NOTE: Do *not* call ompi_proc_finalize as there are many places in
         *       the code that point to indv. procs in this strucutre. For our
         *       needs here we only need to fix up the modex, bml and pml
         *       references.
         */
        if (OMPI_SUCCESS != (ret = ompi_proc_refresh())) {
            opal_output(0,
                        "pml:bfo: ft_event(Restart): proc_refresh Failed %d",
                        ret);
            for(p = 0; p < (int)num_procs; ++p) {
                OBJ_RELEASE(procs[p]);
            }
            free (procs);
            return ret;
        }
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    /* Call the BML
     * BML is expected to call ft_event in
     * - BTL(s)
     * - MPool(s)
     */
    if( OMPI_SUCCESS != (ret = mca_bml.bml_ft_event(state))) {
        opal_output(0, "pml:base: ft_event: BML ft_event function failed: %d\n",
                    ret);
    }

    if(OPAL_CRS_CHECKPOINT == state) {
        OPAL_CR_SET_TIMER(OPAL_CR_TIMER_P2P1);

        if( opal_cr_timing_barrier_enabled ) {
            OPAL_CR_SET_TIMER(OPAL_CR_TIMER_P2PBR0);
            /* JJH Cannot barrier here due to progress engine -- ompi_rte_barrier();*/
        }
    }
    else if(OPAL_CRS_CONTINUE == state) {
        if( !first_continue_pass ) {
            if( opal_cr_timing_barrier_enabled ) {
                OPAL_CR_SET_TIMER(OPAL_CR_TIMER_P2PBR1);
                opal_pmix.fence(NULL, 0);
            }
            OPAL_CR_SET_TIMER(OPAL_CR_TIMER_P2P3);
        }

        if (opal_cr_continue_like_restart && !first_continue_pass) {
            /*
             * Exchange the modex information once again.
             * BTLs will have republished their modex information.
             */
            opal_pmix.fence(NULL, 0);

            /*
             * Startup the PML stack now that the modex is running again
             * Add the new procs (BTLs redo modex recv's)
             */
            if( OMPI_SUCCESS != (ret = mca_pml_bfo_add_procs(procs, num_procs) ) ) {
                opal_output(0, "pml:bfo: ft_event(Restart): Failed in add_procs (%d)", ret);
                return ret;
            }

            /* Is this barrier necessary ? JJH */
            opal_pmix.fence(NULL, 0);

            if( NULL != procs ) {
                for(p = 0; p < (int)num_procs; ++p) {
                    OBJ_RELEASE(procs[p]);
                }
                free(procs);
                procs = NULL;
            }
        }
        if( !first_continue_pass ) {
            if( opal_cr_timing_barrier_enabled ) {
                OPAL_CR_SET_TIMER(OPAL_CR_TIMER_P2PBR2);
                opal_pmix.fence(NULL, 0);
            }
            OPAL_CR_SET_TIMER(OPAL_CR_TIMER_CRCP1);
        }
    }
    else if(OPAL_CRS_RESTART_PRE == state ) {
        /* Nothing here */
    }
    else if(OPAL_CRS_RESTART == state  ) {
        /*
         * Exchange the modex information once again.
         * BTLs will have republished their modex information.
         */
        opal_pmix.fence(NULL, 0);

        /*
         * Startup the PML stack now that the modex is running again
         * Add the new procs (BTLs redo modex recv's)
         */
        if( OMPI_SUCCESS != (ret = mca_pml_bfo_add_procs(procs, num_procs) ) ) {
            opal_output(0, "pml:bfo: ft_event(Restart): Failed in add_procs (%d)", ret);
            return ret;
        }

        /* Is this barrier necessary ? JJH */
        opal_pmix.fence(NULL, 0);

        if( NULL != procs ) {
            for(p = 0; p < (int)num_procs; ++p) {
                OBJ_RELEASE(procs[p]);
            }
            free(procs);
            procs = NULL;
        }
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
#endif /* OPAL_ENABLE_FT_CR */

int mca_pml_bfo_com_btl_comp(const void *v1, const void *v2)
{
    const mca_pml_bfo_com_btl_t *b1 = (const mca_pml_bfo_com_btl_t *) v1;
    const mca_pml_bfo_com_btl_t *b2 = (const mca_pml_bfo_com_btl_t *) v2;

    if(b1->bml_btl->btl_weight < b2->bml_btl->btl_weight)
        return 1;
    if(b1->bml_btl->btl_weight > b2->bml_btl->btl_weight)
        return -1;

    return 0;
}

