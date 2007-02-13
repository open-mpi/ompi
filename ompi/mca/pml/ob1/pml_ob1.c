/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
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

#include "ompi/class/ompi_bitmap.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/btl/base/base.h"
#include "pml_ob1.h"
#include "pml_ob1_component.h"
#include "pml_ob1_comm.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_recvfrag.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_rdmafrag.h"
#include "ompi/mca/bml/base/base.h"
#include "orte/mca/errmgr/errmgr.h"

mca_pml_ob1_t mca_pml_ob1 = {
    {
    mca_pml_ob1_add_procs,
    mca_pml_ob1_del_procs,
    mca_pml_ob1_enable,
    mca_pml_ob1_progress,
    mca_pml_ob1_add_comm,
    mca_pml_ob1_del_comm,
    mca_pml_ob1_irecv_init,
    mca_pml_ob1_irecv,
    mca_pml_ob1_recv,
    mca_pml_ob1_isend_init,
    mca_pml_ob1_isend,
    mca_pml_ob1_send,
    mca_pml_ob1_iprobe,
    mca_pml_ob1_probe,
    mca_pml_ob1_start,
    mca_pml_ob1_dump,
    32768,
    INT_MAX
    }
};


void mca_pml_ob1_error_handler(
        struct mca_btl_base_module_t* btl,
        int32_t flags);

int mca_pml_ob1_enable(bool enable)
{
    if( false == enable ) return OMPI_SUCCESS;
    mca_pml_ob1.enabled = true;
    return OMPI_SUCCESS;
}

int mca_pml_ob1_add_comm(ompi_communicator_t* comm)
{
    /* allocate pml specific comm data */
    mca_pml_ob1_comm_t* pml_comm = OBJ_NEW(mca_pml_ob1_comm_t);
    int i;

    if (NULL == pml_comm) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    mca_pml_ob1_comm_init_size(pml_comm, comm->c_remote_group->grp_proc_count);
    comm->c_pml_comm = pml_comm;

    for( i = 0; i < comm->c_remote_group->grp_proc_count; i++ ) {
        pml_comm->procs[i].ompi_proc = comm->c_remote_group->grp_proc_pointers[i];
    }
    return OMPI_SUCCESS;
}

int mca_pml_ob1_del_comm(ompi_communicator_t* comm)
{
    OBJ_RELEASE(comm->c_pml_comm);
    comm->c_pml_comm = NULL;
    return OMPI_SUCCESS;
}


/*
 *   For each proc setup a datastructure that indicates the PTLs
 *   that can be used to reach the destination.
 *
 */

int mca_pml_ob1_add_procs(ompi_proc_t** procs, size_t nprocs)
{
    ompi_bitmap_t reachable;
    struct mca_bml_base_endpoint_t ** bml_endpoints = NULL;
    int rc;
    size_t i;

    if(nprocs == 0)
        return OMPI_SUCCESS;

    OBJ_CONSTRUCT(&reachable, ompi_bitmap_t);
    rc = ompi_bitmap_init(&reachable, nprocs);
    if(OMPI_SUCCESS != rc)
        return rc;

    /* make sure remote procs are using the same PML as us */
    if (OMPI_SUCCESS != (rc = mca_pml_base_pml_check_selected("ob1",
                                                              procs,
                                                              nprocs))) {
        return rc;
    }

    bml_endpoints = (struct mca_bml_base_endpoint_t **) malloc ( nprocs *
		     sizeof(struct mca_bml_base_endpoint_t*));
    if ( NULL == bml_endpoints ) {
	return OMPI_ERR_OUT_OF_RESOURCE;
    }
   
    rc = mca_bml.bml_add_procs(
                               nprocs,
                               procs,
                               bml_endpoints,
                               &reachable
                               );
    if(OMPI_SUCCESS != rc)
        return rc;

    rc = mca_bml.bml_register(
                              MCA_BTL_TAG_PML,
                              mca_pml_ob1_recv_frag_callback,
                              NULL);
    
    /* register error handlers */
    rc = mca_bml.bml_register_error(mca_pml_ob1_error_handler);
    
    /* initialize free list of receive buffers */
    ompi_free_list_init(
                        &mca_pml_ob1.buffers,
                        sizeof(mca_pml_ob1_buffer_t) + mca_pml_ob1.eager_limit,
                        OBJ_CLASS(mca_pml_ob1_buffer_t),
                        0,
                        mca_pml_ob1.free_list_max,
                        mca_pml_ob1.free_list_inc,
                        NULL);

    /* we don't have any endpoint data we need to cache on the
       ompi_proc_t, so set proc_pml to NULL */
    for (i = 0 ; i < nprocs ; ++i) {
        procs[i]->proc_pml = NULL;
    }

    if ( NULL != bml_endpoints ) {
	free ( bml_endpoints) ;
    }
    OBJ_DESTRUCT(&reachable);
    return rc;
}

/*
 * iterate through each proc and notify any PTLs associated
 * with the proc that it is/has gone away
 */

int mca_pml_ob1_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    return mca_bml.bml_del_procs(nprocs, procs);
}

int mca_pml_ob1_component_fini(void)
{
    /* FIX */
    return OMPI_SUCCESS;
}

/*
 * diagnostics
 */

int mca_pml_ob1_dump(struct ompi_communicator_t* comm, int verbose)
{
    struct mca_pml_comm_t* pml_comm = comm->c_pml_comm;
    size_t i;

    /* iterate through all procs on communicator */
    for(i=0; i<pml_comm->num_procs; i++) {
        mca_pml_ob1_comm_proc_t* proc = &pml_comm->procs[i];
        mca_bml_base_endpoint_t* ep = (mca_bml_base_endpoint_t*)proc->ompi_proc->proc_bml;
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

static void mca_pml_ob1_fin_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context; 
    
    MCA_BML_BASE_BTL_DES_RETURN(bml_btl, des);

    /* check for pending requests */
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

int mca_pml_ob1_send_fin_btl(
        ompi_proc_t* proc,
        mca_bml_base_btl_t* bml_btl,
        void *hdr_des
        )
{
    mca_btl_base_descriptor_t* fin;
    mca_pml_ob1_fin_hdr_t* hdr;
    int rc;

    MCA_PML_OB1_DES_ALLOC(bml_btl, fin, sizeof(mca_pml_ob1_fin_hdr_t));
    if(NULL == fin) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    fin->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    fin->des_cbfunc = mca_pml_ob1_fin_completion;
    fin->des_cbdata = NULL;

    /* fill in header */
    hdr = (mca_pml_ob1_fin_hdr_t*)fin->des_src->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_FIN;
    hdr->hdr_des.pval = hdr_des;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
#ifdef WORDS_BIGENDIAN
    hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else
    /* if we are little endian and the remote side is big endian,
       we're responsible for making sure the data is in network byte
       order */
    if (proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
        hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
        MCA_PML_OB1_FIN_HDR_HTON(*hdr);
    }
#endif
#endif

    /* queue request */
    rc = mca_bml_base_send(
                           bml_btl,
                           fin,
                           MCA_BTL_TAG_PML
                           );
    if(OMPI_SUCCESS != rc) {
        MCA_BML_BASE_BTL_DES_RETURN(bml_btl, fin);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    

    return OMPI_SUCCESS;
}

void mca_pml_ob1_process_pending_packets(mca_bml_base_btl_t* bml_btl)
{
    mca_pml_ob1_pckt_pending_t *pckt;
    int i, rc, s = opal_list_get_size(&mca_pml_ob1.pckt_pending);

    for(i = 0; i < s; i++) {
        mca_bml_base_btl_t *send_dst = NULL;
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
        pckt = (mca_pml_ob1_pckt_pending_t*)
            opal_list_remove_first(&mca_pml_ob1.pckt_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        if(NULL == pckt)
            break;
        if(pckt->bml_btl != NULL && 
                pckt->bml_btl->btl == bml_btl->btl) {
            send_dst = pckt->bml_btl;
        } else {
            send_dst = mca_bml_base_btl_array_find(
                    &pckt->proc->proc_bml->btl_eager, bml_btl->btl);
        }
        if(NULL == send_dst) {
            OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
            opal_list_append(&mca_pml_ob1.pckt_pending,
                    (opal_list_item_t*)pckt);
            OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
            continue;
        }

        switch(pckt->hdr.hdr_common.hdr_type) {
            case MCA_PML_OB1_HDR_TYPE_ACK:
                rc = mca_pml_ob1_recv_request_ack_send_btl(pckt->proc,
                        send_dst,
                        pckt->hdr.hdr_ack.hdr_src_req.lval,
                        pckt->hdr.hdr_ack.hdr_dst_req.pval,
                        pckt->hdr.hdr_ack.hdr_rdma_offset);
                MCA_PML_OB1_PCKT_PENDING_RETURN(pckt);
                if(OMPI_ERR_OUT_OF_RESOURCE == rc) {
                    MCA_PML_OB1_ADD_ACK_TO_PENDING(pckt->proc,
                            pckt->hdr.hdr_ack.hdr_src_req.lval,
                            pckt->hdr.hdr_ack.hdr_dst_req.pval,
                            pckt->hdr.hdr_ack.hdr_rdma_offset);
                    return;
                }
                break;
            case MCA_PML_OB1_HDR_TYPE_FIN:
                rc = mca_pml_ob1_send_fin_btl(pckt->proc, send_dst,
                        pckt->hdr.hdr_fin.hdr_des.pval);
                MCA_PML_OB1_PCKT_PENDING_RETURN(pckt);
                if(OMPI_ERR_OUT_OF_RESOURCE == rc) {
                     MCA_PML_OB1_ADD_FIN_TO_PENDING(pckt->proc,
                             pckt->hdr.hdr_fin.hdr_des.pval, pckt->bml_btl);
                     return;
                }
                break;
            default:
                opal_output(0, "[%s:%d] wrong header type\n",
                        __FILE__, __LINE__);
                break;
        }
    }
}

void mca_pml_ob1_process_pending_rdma(void)
{
    mca_pml_ob1_rdma_frag_t* frag;
    int i, rc, s = opal_list_get_size(&mca_pml_ob1.rdma_pending);

    for(i = 0; i < s; i++) {
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
        frag = (mca_pml_ob1_rdma_frag_t*)
            opal_list_remove_first(&mca_pml_ob1.rdma_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        if(NULL == frag)
            break;
        if(frag->rdma_state == MCA_PML_OB1_RDMA_PUT) {
            rc = mca_pml_ob1_send_request_put_frag(frag);
        } else {
            rc = mca_pml_ob1_recv_request_get_frag(frag);
        }
        if(OMPI_ERR_OUT_OF_RESOURCE == rc)
            break;
    }
}


void mca_pml_ob1_error_handler(
        struct mca_btl_base_module_t* btl,
        int32_t flags) { 
    orte_errmgr.abort();
}
