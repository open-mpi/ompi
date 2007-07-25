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
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <sys/time.h>
#include <time.h>
#include "ompi/types.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss.h"
#include "btl_openib.h"
#include "btl_openib_endpoint.h" 
#include "btl_openib_proc.h"
#include "btl_openib_frag.h"
#include "ompi/class/ompi_free_list.h" 
#include <errno.h> 
#include <string.h> 

#define ENDPOINT_CONNECT_REQ 0
#define ENDPOINT_CONNECT_RSP 1
#define ENDPOINT_CONNECT_ACK 2

static void mca_btl_openib_endpoint_construct(mca_btl_base_endpoint_t* endpoint);
static void mca_btl_openib_endpoint_destruct(mca_btl_base_endpoint_t* endpoint);

int mca_btl_openib_endpoint_create_qp(
                                      mca_btl_openib_module_t* openib_btl, 
                                      struct ibv_pd* pd, 
                                      struct ibv_cq* cq, 
                                      struct ibv_srq* srq, 
                                      struct ibv_qp_attr* qp_attr,                                      
                                      struct ibv_qp** qp,
                                      int qp_idx
                                      ); 



int mca_btl_openib_endpoint_qp_init_query(
                                          mca_btl_openib_module_t* openib_btl, 
                                          struct ibv_qp* qp, 
                                          struct ibv_qp_attr* attr,
                                          uint32_t lcl_psn, 
                                          uint32_t rem_qp_num, 
                                          uint32_t rem_psn,  
                                          uint16_t rem_lid, 
                                          uint32_t rem_mtu,
                                          uint32_t port_num 
                                          ); 

static int post_send(mca_btl_openib_module_t *openib_btl,
        mca_btl_openib_endpoint_t *endpoint, mca_btl_openib_frag_t *frag,
        const int qp, const int do_rdma)
{
    struct ibv_send_wr *bad_wr; 

    assert(!do_rdma || BTL_OPENIB_EAGER_RDMA_QP(qp));

    frag->sg_entry.length = frag->segment.seg_len +
        sizeof(mca_btl_openib_header_t) +
        (do_rdma ? sizeof(mca_btl_openib_footer_t) : 0);

    if(frag->sg_entry.length <= openib_btl->ib_inline_max) { 
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED|IBV_SEND_INLINE;
    } else { 
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED; 
    }

    if(endpoint->nbo)
        BTL_OPENIB_HEADER_HTON((*(frag->hdr)));

    if(do_rdma) {
        mca_btl_openib_footer_t* ftr =
            (mca_btl_openib_footer_t*)(((char*)frag->segment.seg_addr.pval) +
                                       frag->segment.seg_len);
        frag->wr_desc.sr_desc.opcode = IBV_WR_RDMA_WRITE;
        MCA_BTL_OPENIB_RDMA_FRAG_SET_SIZE(ftr, frag->sg_entry.length);
        MCA_BTL_OPENIB_RDMA_MAKE_LOCAL(ftr);
#if OMPI_ENABLE_DEBUG
        ((mca_btl_openib_footer_t*)(((char*)frag->segment.seg_addr.pval) +
                                    frag->segment.seg_len))->seq = 
            endpoint->eager_rdma_remote.seq++;
#endif
        if(endpoint->nbo)
            BTL_OPENIB_FOOTER_HTON((*ftr));

        frag->wr_desc.sr_desc.wr.rdma.rkey = endpoint->eager_rdma_remote.rkey;
        frag->wr_desc.sr_desc.wr.rdma.remote_addr =
            endpoint->eager_rdma_remote.base.lval +
            endpoint->eager_rdma_remote.head *
            openib_btl->eager_rdma_frag_size +
            sizeof(mca_btl_openib_header_t) +
            mca_btl_openib_component.eager_limit +
            sizeof(mca_btl_openib_footer_t);
        frag->wr_desc.sr_desc.wr.rdma.remote_addr -= frag->sg_entry.length;
        MCA_BTL_OPENIB_RDMA_NEXT_INDEX(endpoint->eager_rdma_remote.head);
    } else {
        if(MCA_BTL_OPENIB_SRQ_QP == endpoint->qps[qp].qp_type) { 
            frag->wr_desc.sr_desc.opcode = IBV_WR_SEND_WITH_IMM;
            frag->wr_desc.sr_desc.imm_data = endpoint->rem_info.rem_index;
        } else {
            frag->wr_desc.sr_desc.opcode = IBV_WR_SEND;
        }
    }
            
    frag->base.order = qp;
    return ibv_post_send(endpoint->qps[qp].lcl_qp, &frag->wr_desc.sr_desc, &bad_wr);
 }

/* 
 * post a send to the work queue 
 */ 
static int btl_openib_acquire_send_resources(
        mca_btl_openib_module_t *openib_btl,
        mca_btl_openib_endpoint_t *endpoint,
        mca_btl_openib_frag_t *frag, int *qp, int *do_rdma)
{
    if(*do_rdma) {
        if(OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens, -1) < 0) {
            OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens, 1);
            *do_rdma = 0;
        } else {
            *qp = mca_btl_openib_component.eager_rdma_qp;
        }
    }

    if(OPAL_THREAD_ADD32(&endpoint->qps[*qp].sd_wqe, -1) < 0) {
        OPAL_THREAD_ADD32(&endpoint->qps[*qp].sd_wqe, 1);
        if(*do_rdma)
            OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens, 1);
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        opal_list_append(&endpoint->qps[*qp].pending_frags,
                (opal_list_item_t *)frag);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if(*do_rdma)
        return OMPI_SUCCESS;

    if(MCA_BTL_OPENIB_PP_QP == endpoint->qps[*qp].qp_type) { 
        if(OPAL_THREAD_ADD32(&endpoint->qps[*qp].u.pp_qp.sd_credits, -1) < 0) {
            OPAL_THREAD_ADD32(&endpoint->qps[*qp].u.pp_qp.sd_credits, 1);
            OPAL_THREAD_ADD32(&endpoint->qps[*qp].sd_wqe, 1);
            OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
            opal_list_append(&endpoint->qps[*qp].pending_frags,
                    (opal_list_item_t *)frag);
            OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    } else {
        if(OPAL_THREAD_ADD32(&openib_btl->qps[*qp].u.srq_qp.sd_credits, -1) < 0) {
            OPAL_THREAD_ADD32(&openib_btl->qps[*qp].u.srq_qp.sd_credits, 1);
            OPAL_THREAD_ADD32(&endpoint->qps[*qp].sd_wqe, 1);
            OPAL_THREAD_LOCK(&openib_btl->ib_lock);
            opal_list_append(&openib_btl->qps[*qp].u.srq_qp.pending_frags,
                             (opal_list_item_t *)frag);
            OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }
        
    return OMPI_SUCCESS;
}

/* this function os called with endpoint->endpoint_lock held */
static inline int mca_btl_openib_endpoint_post_send(mca_btl_openib_module_t* openib_btl, 
                                                    mca_btl_openib_endpoint_t * endpoint, 
                                                    mca_btl_openib_frag_t * frag)
{ 
    int do_rdma = 0, qp, ib_rc;
    
    frag->sg_entry.addr = (unsigned long) frag->hdr;
   
    if(frag->base.order != MCA_BTL_NO_ORDER) {
        qp = frag->base.order; /* if order is provided use it */
    } else {
        qp = frag->qp_idx;

        if(frag->segment.seg_len <= mca_btl_openib_component.eager_limit &&
                (frag->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY))
            do_rdma = 1; /* High priority frag. Try to send over eager RDMA */
    }

    if(btl_openib_acquire_send_resources(openib_btl, endpoint, frag, &qp,
                &do_rdma) == OMPI_ERR_OUT_OF_RESOURCE)
        return MPI_SUCCESS;

    if(BTL_OPENIB_EAGER_RDMA_QP(qp) && endpoint->eager_rdma_local.credits > 0) {
        frag->hdr->credits = endpoint->eager_rdma_local.credits;
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits,
                          -frag->hdr->credits);
        frag->hdr->credits |= BTL_OPENIB_RDMA_CREDITS_FLAG;
    } else if(MCA_BTL_OPENIB_PP_QP == endpoint->qps[qp].qp_type &&
            endpoint->qps[qp].u.pp_qp.rd_credits > 0) {
            frag->hdr->credits = endpoint->qps[qp].u.pp_qp.rd_credits;
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.rd_credits, -frag->hdr->credits);
    } else {
        frag->hdr->credits = 0;
    }

    if(endpoint->qps[qp].u.pp_qp.cm_return) {
        frag->hdr->cm_seen = endpoint->qps[qp].u.pp_qp.cm_return;
        OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_return,
                -frag->hdr->cm_seen);
    } else {
        frag->hdr->cm_seen = 0;
    }
    
    ib_rc = post_send(openib_btl, endpoint, frag, qp, do_rdma); 
    if(ib_rc) {
        if(endpoint->nbo) { 
            BTL_OPENIB_HEADER_NTOH((*(frag->hdr)));
        }
        if(BTL_OPENIB_IS_RDMA_CREDITS(frag->hdr->credits)) {
            OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits,
                    BTL_OPENIB_CREDITS(frag->hdr->credits));
        }
        OPAL_THREAD_ADD32(&endpoint->qps[qp].sd_wqe, 1);
        if(do_rdma) {
            OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens, 1);
        } else {
            if(MCA_BTL_OPENIB_PP_QP == endpoint->qps[qp].qp_type) {
                OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.rd_credits, frag->hdr->credits);
                OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.sd_credits, 1);
            } else {
                OPAL_THREAD_ADD32(&openib_btl->qps[qp].u.srq_qp.sd_credits, 1);
            }
        }
        BTL_ERROR(("error posting send request error %d: %s\n", 
                   ib_rc, strerror(ib_rc))); 
        return OMPI_ERROR; 
    }
#if 0
    mca_btl_openib_post_srr_all(openib_btl, 1);
    mca_btl_openib_endpoint_post_rr_all(endpoint, 1);
#endif
    return OMPI_SUCCESS;
}



OBJ_CLASS_INSTANCE(mca_btl_openib_endpoint_t, 
                   opal_list_item_t, mca_btl_openib_endpoint_construct, 
                   mca_btl_openib_endpoint_destruct);

/*
 * Initialize state of the endpoint instance.
 *
 */
static void mca_btl_openib_endpoint_construct_qp(mca_btl_base_endpoint_t *endpoint, int qp)
{
 
    bool pp = (MCA_BTL_OPENIB_PP_QP == 
               mca_btl_openib_component.qp_infos[qp].type);
    endpoint->qps[qp].lcl_qp_attr =
        (struct ibv_qp_attr *)malloc(sizeof(struct ibv_qp_attr));
    memset(endpoint->qps[qp].lcl_qp_attr, 0, sizeof(struct ibv_qp_attr));
    endpoint->qps[qp].qp_type = mca_btl_openib_component.qp_infos[qp].type;
    endpoint->qps[qp].lcl_qp = NULL;
    endpoint->qps[qp].rd_pending_credit_chks = 0;
    /* setup rem_info */
    endpoint->rem_info.rem_qps[qp].rem_qp_num = 0; 
    endpoint->rem_info.rem_qps[qp].rem_psn = 0;

    OBJ_CONSTRUCT(&endpoint->qps[qp].pending_frags, opal_list_t);
    if(pp) { 
        /* local credits are set here such that on initial posting
         * of the receive buffers we end up with zero credits to return
         * to our peer. The peer initializes his sd_credits to reflect this 
         * below. Note that this may be a problem for iWARP as the sender 
         * now has credits even if the receive buffers are not yet posted 
         */
        endpoint->qps[qp].u.pp_qp.rd_credits =
            -mca_btl_openib_component.qp_infos[qp].rd_num;
        
        endpoint->qps[qp].u.pp_qp.rd_posted = 0;
        endpoint->qps[qp].u.pp_qp.cm_sent = 0;
        endpoint->qps[qp].u.pp_qp.cm_return = 
            -mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv;
        endpoint->qps[qp].u.pp_qp.cm_received =
            mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv;

        /* initialize the local view of credits */
        endpoint->qps[qp].u.pp_qp.sd_credits = 
            mca_btl_openib_component.qp_infos[qp].rd_num;
        
        /* number of available send wqes */
        endpoint->qps[qp].sd_wqe = mca_btl_openib_component.qp_infos[qp].rd_num;
    
    } else { 
        /* number of available send wqes */
        endpoint->qps[qp].sd_wqe = mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
    } 
    
}

static void mca_btl_openib_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{

    int qp;
    
    /* setup qp structures */
    if( mca_btl_openib_component.num_qps > 0 ) { 
        endpoint->qps = 
            (mca_btl_openib_endpoint_qp_t*) 
            malloc(sizeof(mca_btl_openib_endpoint_qp_t) * 
                   mca_btl_openib_component.num_qps);
        endpoint->rem_info.rem_qps = 
            (mca_btl_openib_rem_qp_info_t*)
            malloc(sizeof(mca_btl_openib_rem_qp_info_t) *
                   mca_btl_openib_component.num_qps);
    }

    endpoint->endpoint_btl = 0;
    endpoint->endpoint_proc = 0;
    endpoint->endpoint_tstamp = 0.0;
    endpoint->endpoint_state = MCA_BTL_IB_CLOSED;
    endpoint->endpoint_retries = 0;
    OBJ_CONSTRUCT(&endpoint->endpoint_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&endpoint->pending_lazy_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->pending_get_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->pending_put_frags, opal_list_t);
    
    endpoint->get_tokens = mca_btl_openib_component.ib_qp_ous_rd_atom;

    /* initialize RDMA eager related parts */
    endpoint->eager_recv_count = 0;
    memset(&endpoint->eager_rdma_remote, 0,
           sizeof(mca_btl_openib_eager_rdma_remote_t));
    memset(&endpoint->eager_rdma_local, 0,
           sizeof(mca_btl_openib_eager_rdma_local_t));
    OBJ_CONSTRUCT(&endpoint->eager_rdma_local.lock, opal_mutex_t);

    endpoint->rem_info.rem_lid = 0; 
    endpoint->rem_info.rem_subnet_id = 0; 
    endpoint->rem_info.rem_mtu = 0;
    endpoint->nbo = false;
    endpoint->use_eager_rdma = false;
    endpoint->eager_rdma_remote.tokens = 0;
    endpoint->eager_rdma_local.credits = 0;
    
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
        mca_btl_openib_endpoint_construct_qp(endpoint, qp);
    }
}

/*
 * Destroy a endpoint
 *
 */

static void mca_btl_openib_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
    bool pval_clean = false;
    int qp;
    /* Release memory resources */
    do {
        /* Make sure that mca_btl_openib_endpoint_connect_eager_rdma ()
         * was not in "connect" or "bad" flow (failed to allocate memory)
         * and changed the pointer back to NULL 
         */
        if(!opal_atomic_cmpset_ptr(&endpoint->eager_rdma_local.base.pval, NULL,
                    (void*)1)) {
            if ((void*)1 != endpoint->eager_rdma_local.base.pval && 
                    NULL != endpoint->eager_rdma_local.base.pval) {
                endpoint->endpoint_btl->super.btl_mpool->mpool_free(endpoint->endpoint_btl->super.btl_mpool,
                        endpoint->eager_rdma_local.base.pval, 
                        (mca_mpool_base_registration_t*)endpoint->eager_rdma_local.reg);
                pval_clean=true;
            }
        } else {
            pval_clean=true;
        }
    } while (!pval_clean);

    /* Close opened QPs if we have them*/
    if(MCA_BTL_IB_CLOSED != endpoint->endpoint_state) {
        
        for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
            OBJ_DESTRUCT(&endpoint->qps[qp].pending_frags);
            MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(endpoint->endpoint_btl,
                    &endpoint->qps[qp].pending_frags);
            if(ibv_destroy_qp(endpoint->qps[qp].lcl_qp)) {
                BTL_ERROR(("Failed to destroy QP:%d\n", qp));
            }
            free(endpoint->qps[qp].lcl_qp_attr);
        }
        /* free the qps */
        free(endpoint->qps);
    }
    OBJ_DESTRUCT(&endpoint->endpoint_lock);
    /* Clean pending lists */
    MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(endpoint->endpoint_btl,
            &endpoint->pending_lazy_frags);
    OBJ_DESTRUCT(&endpoint->pending_lazy_frags);
    
    MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(endpoint->endpoint_btl,
            &endpoint->pending_get_frags);
    OBJ_DESTRUCT(&endpoint->pending_get_frags);
    
    MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(endpoint->endpoint_btl,
                                       &endpoint->pending_put_frags);
    OBJ_DESTRUCT(&endpoint->pending_put_frags);
}

/*
 * Send connection information to remote endpoint using OOB
 *
 */

static void mca_btl_openib_endpoint_send_cb(
    int status,
    orte_process_name_t* endpoint, 
    orte_buffer_t* buffer,
    orte_rml_tag_t tag, 
    void* cbdata)
{
    OBJ_RELEASE(buffer);
}

static int mca_btl_openib_endpoint_send_connect_data(mca_btl_base_endpoint_t* endpoint, uint8_t message_type)
{
    orte_buffer_t* buffer = OBJ_NEW(orte_buffer_t);
    int rc;
    
    if(NULL == buffer) {
         ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
         return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* pack the info in the send buffer */ 
    opal_output(mca_btl_base_output, "packing %d of %d\n", 1, ORTE_UINT8);
    rc = orte_dss.pack(buffer, &message_type, 1, ORTE_UINT8);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    opal_output(mca_btl_base_output, "packing %d of %d\n", 1, ORTE_UINT64);
    rc = orte_dss.pack(buffer, &endpoint->subnet_id, 1, ORTE_UINT64);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /*GMS left off here */
    if(message_type != ENDPOINT_CONNECT_REQ) {
        /* send the QP connect request info we respond to */
        opal_output(mca_btl_base_output, "packing %d of %d\n", 1, ORTE_UINT32);
        rc = orte_dss.pack(buffer,
                           &endpoint->rem_info.rem_qps[0].rem_qp_num, 1,
                           ORTE_UINT32);
        if(rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        opal_output(mca_btl_base_output, "packing %d of %d\n", 1, ORTE_UINT16);
        rc = orte_dss.pack(buffer, &endpoint->rem_info.rem_lid, 1, ORTE_UINT16);
        if(rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    if(message_type != ENDPOINT_CONNECT_ACK) {
        int qp;
        /* stuff all the QP info into the buffer */
        for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
            opal_output(mca_btl_base_output, "packing %d of %d\n", 1, ORTE_UINT32);
            rc = orte_dss.pack(buffer, &endpoint->qps[qp].lcl_qp->qp_num,
                               1, ORTE_UINT32);
            if(rc != ORTE_SUCCESS) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            opal_output(mca_btl_base_output, "packing %d of %d\n", 1, ORTE_UINT32);
            rc = orte_dss.pack(buffer, &endpoint->qps[qp].lcl_psn, 1,
                               ORTE_UINT32); 
            if(rc != ORTE_SUCCESS) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        opal_output(mca_btl_base_output, "packing %d of %d\n", 1, ORTE_UINT16);
        rc = orte_dss.pack(buffer, &endpoint->endpoint_btl->lid, 1, ORTE_UINT16);
        if(rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        opal_output(mca_btl_base_output, "packing %d of %d\n", 1, ORTE_UINT32);
        rc = orte_dss.pack(buffer, &endpoint->endpoint_btl->hca->mtu, 1,
                ORTE_UINT32);
        if(rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        opal_output(mca_btl_base_output, "packing %d of %d\n", 1, ORTE_UINT32);
        rc = orte_dss.pack(buffer, &endpoint->index, 1, ORTE_UINT32);
        if(rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* send to endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, 
                                 buffer, ORTE_RML_TAG_DYNAMIC-1, 0,
                                 mca_btl_openib_endpoint_send_cb, NULL);
    
    
    BTL_VERBOSE(("Sent QP Info, LID = %d, SUBNET = %016x\n",
                 endpoint->endpoint_btl->lid, 
                 endpoint->subnet_id));
    
    

    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 * Set remote connection info
 *   (from OOB connection) 
 *
 */
static int mca_btl_openib_endpoint_set_remote_info(mca_btl_base_endpoint_t* endpoint,
                                                   mca_btl_openib_rem_info_t* rem_info)
{
    
    /* copy the rem_info stuff */
    memcpy(&((mca_btl_openib_endpoint_t*) endpoint)->rem_info, 
           rem_info, sizeof(mca_btl_openib_rem_info_t)); 
    
    /* copy over the rem qp info */
    memcpy(endpoint->rem_info.rem_qps,
           rem_info->rem_qps, sizeof(mca_btl_openib_rem_qp_info_t) * 
           mca_btl_openib_component.num_qps);
    
    BTL_VERBOSE(("Setting QP info,  LID = %d", endpoint->rem_info.rem_lid));

    return ORTE_SUCCESS;

}


static int mca_btl_openib_endpoint_qp_create_one(
         mca_btl_base_endpoint_t* endpoint, int prio, int qp)
{
    int rc;
    mca_btl_openib_module_t *openib_btl =
        (mca_btl_openib_module_t*)endpoint->endpoint_btl;
    struct ibv_srq *srq = 
        (MCA_BTL_OPENIB_PP_QP == openib_btl->qps[qp].type) ? NULL : 
        openib_btl->qps[qp].u.srq_qp.srq;
    
    /* Create the  Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_create_qp(openib_btl,
                    openib_btl->hca->ib_pd,
                    openib_btl->ib_cq[prio],
                    srq,
                    endpoint->qps[qp].lcl_qp_attr,
                    &endpoint->qps[qp].lcl_qp, 
                    qp))) {
        BTL_ERROR(("error creating queue pair, error code %d", rc)); 
        return rc;
    }
    endpoint->qps[qp].lcl_psn = lrand48() & 0xffffff;
    endpoint->qps[qp].credit_frag = NULL;
    openib_btl->cq_users[prio]++;

    if(MCA_BTL_OPENIB_SRQ_QP == endpoint->qps[qp].qp_type) { 
        mca_btl_openib_post_srr(openib_btl, 1, qp);
    } else { 
        mca_btl_openib_endpoint_post_rr(endpoint, 1, qp);
    }
    
    return OMPI_SUCCESS;
 }

static int mca_btl_openib_endpoint_qp_create_all(
        mca_btl_base_endpoint_t* endpoint)
{
    int qp, rc;

    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
        if( mca_btl_openib_component.qp_infos[qp].size <=
                mca_btl_openib_component.eager_limit) { 
            rc = mca_btl_openib_endpoint_qp_create_one(endpoint,
                    BTL_OPENIB_HP_CQ, qp);
            if(rc != OMPI_SUCCESS)
                return rc;
        } else { 
            rc = mca_btl_openib_endpoint_qp_create_one(endpoint,
                    BTL_OPENIB_LP_CQ, qp);
            if(rc != OMPI_SUCCESS)
                return rc;
        }
    }

    return OMPI_SUCCESS;
}
/*
 * Start to connect to the endpoint. We send our Queue Pair
 * information over the TCP OOB communication mechanism.

 * On completion of our send, a send completion handler 
 * is called
 *
 */

static int mca_btl_openib_endpoint_start_connect(mca_btl_base_endpoint_t* endpoint)
{
    int rc;
    
    BTL_VERBOSE(("Initialized QP ,  LID = %d",
                 ((mca_btl_openib_module_t*)endpoint->endpoint_btl)->lid)); 
    
    rc = mca_btl_openib_endpoint_qp_create_all(endpoint);

    if(rc != OMPI_SUCCESS)
        return rc;

    /* Send connection info over to remote endpoint */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;

    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_send_connect_data(endpoint,
                                                                       ENDPOINT_CONNECT_REQ))) {
        BTL_ERROR(("error sending connect request, error code %d", rc)); 
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 * Reply to a `start - connect' message
 *
 */
static int mca_btl_openib_endpoint_reply_start_connect(
        mca_btl_openib_endpoint_t *endpoint,
        mca_btl_openib_rem_info_t *rem_info)
{
    int rc;

    BTL_VERBOSE(("Initialized QPs, LID = %d",
                 ((mca_btl_openib_module_t*)endpoint->endpoint_btl)->lid));

    rc = mca_btl_openib_endpoint_qp_create_all(endpoint);

    if(rc != OMPI_SUCCESS)
        return rc;

    /* Set the remote side info */
    mca_btl_openib_endpoint_set_remote_info(endpoint, rem_info);
    
    /* Connect to endpoint */

    rc = mca_btl_openib_endpoint_connect(endpoint);
    if(rc != OMPI_SUCCESS) {
        BTL_ERROR(("error in endpoint connect error code is %d", rc)); 
        return rc;
    }

    /* Send connection info over to remote endpoint */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECT_ACK;
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_send_connect_data(endpoint,
                    ENDPOINT_CONNECT_RSP))) {
        BTL_ERROR(("error in endpoint send connect request error code is %d",
                    rc));
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 * called when the openib has completed setup via the 
 *  OOB channel 
 */

static void mca_btl_openib_endpoint_connected(mca_btl_openib_endpoint_t *endpoint)
{
    opal_list_item_t *frag_item;
    mca_btl_openib_frag_t *frag;
    mca_btl_openib_module_t* openib_btl; 
   
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTED;
    endpoint->endpoint_btl->poll_cq = true; 
    
    /**
     * The connection is correctly setup. Now we can decrease the event trigger.
     */
    opal_progress_event_users_decrement();

    /* While there are frags in the list,
     * process them */

    while(!opal_list_is_empty(&(endpoint->pending_lazy_frags))) {
        frag_item = opal_list_remove_first(&(endpoint->pending_lazy_frags));
        frag = (mca_btl_openib_frag_t *) frag_item;
        openib_btl = endpoint->endpoint_btl;
        /* We need to post this one */
        
        if(OMPI_SUCCESS != mca_btl_openib_endpoint_post_send(openib_btl, endpoint, frag))
            BTL_ERROR(("Error posting send")); 
    }
}

/*
 * Non blocking OOB recv callback.
 * Read incoming QP and other info, and if this endpoint
 * is trying to connect, reply with our QP info, 
 * otherwise try to modify QP's and establish
 * reliable connection
 *
 */

static void mca_btl_openib_endpoint_recv(
    int status,
    orte_process_name_t* process_name, 
    orte_buffer_t* buffer,
    orte_rml_tag_t tag, 
    void* cbdata)
{
    mca_btl_openib_proc_t *ib_proc;
    mca_btl_openib_endpoint_t *ib_endpoint = NULL;
    int endpoint_state;
    int rc;
    uint32_t i, lcl_qp = 0;
    uint16_t lcl_lid = 0;
    int32_t cnt = 1;
    mca_btl_openib_rem_info_t rem_info;
    uint8_t message_type;
    bool master;
    
    /* start by unpacking data first so we know who is knocking at 
       our door */ 
    
    opal_output(mca_btl_base_output, "unpacking %d of %d\n", cnt, ORTE_UINT8);
    rc = orte_dss.unpack(buffer, &message_type, &cnt, ORTE_UINT8);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    opal_output(mca_btl_base_output, "unpacking %d of %d\n", cnt, ORTE_UINT64);
    rc = orte_dss.unpack(buffer, &rem_info.rem_subnet_id, &cnt, ORTE_UINT64);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    

    if(message_type != ENDPOINT_CONNECT_REQ) {
        opal_output(mca_btl_base_output, "unpacking %d of %d\n", cnt, ORTE_UINT32);
        rc = orte_dss.unpack(buffer, &lcl_qp, &cnt, ORTE_UINT32);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        opal_output(mca_btl_base_output, "unpacking %d of %d\n", cnt, ORTE_UINT16);
        rc = orte_dss.unpack(buffer, &lcl_lid, &cnt, ORTE_UINT16);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return;
        }
    }
    if(message_type != ENDPOINT_CONNECT_ACK) {
        int qp; 
        /* get ready for the data */
        rem_info.rem_qps = 
            (mca_btl_openib_rem_qp_info_t*) malloc(sizeof(mca_btl_openib_rem_qp_info_t) * 
                                                   mca_btl_openib_component.num_qps);
        
        /* unpack all the qp info */
        for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
            opal_output(mca_btl_base_output, "unpacking %d of %d\n", cnt, ORTE_UINT32);
            rc = orte_dss.unpack(buffer, &rem_info.rem_qps[qp].rem_qp_num, &cnt,
                                 ORTE_UINT32);
            if(ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return;
            }
            opal_output(mca_btl_base_output, "unpacking %d of %d\n", cnt, ORTE_UINT32);
            rc = orte_dss.unpack(buffer, &rem_info.rem_qps[qp].rem_psn, &cnt,
                                 ORTE_UINT32);
            if(ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return;
            }
        }
        
        opal_output(mca_btl_base_output, "unpacking %d of %d\n", cnt, ORTE_UINT16);
        rc = orte_dss.unpack(buffer, &rem_info.rem_lid, &cnt, ORTE_UINT16);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        opal_output(mca_btl_base_output, "unpacking %d of %d\n", cnt, ORTE_UINT32);
        rc = orte_dss.unpack(buffer, &rem_info.rem_mtu, &cnt, ORTE_UINT32);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        opal_output(mca_btl_base_output, "unpacking %d of %d\n", cnt, ORTE_UINT32);
        rc = orte_dss.unpack(buffer, &rem_info.rem_index, &cnt, ORTE_UINT32);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return;
        }
    }
    
    BTL_VERBOSE(("Received QP Info,  LID = %d, SUBNET = %016x\n",
                 rem_info.rem_lid, 
                 rem_info.rem_subnet_id));
    
    master = orte_ns.compare_fields(ORTE_NS_CMP_ALL, orte_process_info.my_name,
                                    process_name) > 0 ? true : false;
    
    for(ib_proc = (mca_btl_openib_proc_t*)
            opal_list_get_first(&mca_btl_openib_component.ib_procs);
        ib_proc != (mca_btl_openib_proc_t*)
            opal_list_get_end(&mca_btl_openib_component.ib_procs);
        ib_proc  = (mca_btl_openib_proc_t*)opal_list_get_next(ib_proc)) {
        bool found = false;
        
        if(orte_ns.compare_fields(ORTE_NS_CMP_ALL,
                                  &ib_proc->proc_guid, process_name) != ORTE_EQUAL)
            continue;
        
        if(message_type != ENDPOINT_CONNECT_REQ) {
            /* This is a reply message. Try to get the endpoint instance the
             * reply belongs to */
            for(i = 0; i < ib_proc->proc_endpoint_count; i++) { 
                ib_endpoint = ib_proc->proc_endpoints[i];
                if(ib_endpoint->qps[0].lcl_qp != NULL &&
                   lcl_lid == ib_endpoint->endpoint_btl->lid &&
                   lcl_qp == ib_endpoint->qps[0].lcl_qp->qp_num &&
                   rem_info.rem_subnet_id == ib_endpoint->subnet_id) {
                    found = true;
                    break;
                }
            }
        } else {
            /* This is new connection request. If this is master try to find
             * endpoint in a connecting state. If this is slave try to find
             * endpoint in closed state and initiate connection back */
            mca_btl_openib_endpoint_t *ib_endpoint_found = NULL;
            for(i = 0; i < ib_proc->proc_endpoint_count; i++) { 
                ib_endpoint = ib_proc->proc_endpoints[i];
                if(ib_endpoint->subnet_id != rem_info.rem_subnet_id ||
                   (ib_endpoint->endpoint_state != MCA_BTL_IB_CONNECTING
                    && ib_endpoint->endpoint_state != MCA_BTL_IB_CLOSED))
                    continue;
                found = true;
                ib_endpoint_found = ib_endpoint;
                if((master &&
                    ib_endpoint->endpoint_state == MCA_BTL_IB_CONNECTING) ||
                   (!master &&
                    ib_endpoint->endpoint_state == MCA_BTL_IB_CLOSED))
                    break; /* Found one. No point to continue */
            }
            ib_endpoint = ib_endpoint_found;
            
            /* if this is slave and there is no endpoints in closed state
             * then all connection are already in progress so just ignore
             * this connection request */
            if(found && !master &&
                    ib_endpoint->endpoint_state != MCA_BTL_IB_CLOSED) {
                return;
            }
        }
        
        if(!found) {
            BTL_ERROR(("can't find suitable endpoint for this peer\n")); 
            return; 
        }
        
        endpoint_state = ib_endpoint->endpoint_state;
        
        /* Update status */
        switch(endpoint_state) {
        case MCA_BTL_IB_CLOSED :
            /* We had this connection closed before.
             * The endpoint is trying to connect. Move the
             * status of this connection to CONNECTING,
             * and then reply with our QP information */
            
            if(master) {
                rc = mca_btl_openib_endpoint_reply_start_connect(ib_endpoint, &rem_info);
            } else {
                rc = mca_btl_openib_endpoint_start_connect(ib_endpoint);
            }
            
            if(OMPI_SUCCESS != rc) {
                BTL_ERROR(("error in endpoint reply start connect"));
                break;
            }
            
            /** As long as we expect a message from the peer (in order to setup the connection)
             * let the event engine pool the OOB events. Note: we increment it once peer active
             * connection.
             */
            opal_progress_event_users_increment();
            break;
             
        case MCA_BTL_IB_CONNECTING :
            mca_btl_openib_endpoint_set_remote_info(ib_endpoint, &rem_info);
            if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_connect(ib_endpoint))) {
                BTL_ERROR(("endpoint connect error: %d", rc)); 
                break;
            }
           
            if(master) {
                ib_endpoint->endpoint_state = MCA_BTL_IB_WAITING_ACK;

                /* Send him an ack */
                mca_btl_openib_endpoint_send_connect_data(ib_endpoint,
                        ENDPOINT_CONNECT_RSP);
            } else {
                mca_btl_openib_endpoint_send_connect_data(ib_endpoint,
                        ENDPOINT_CONNECT_ACK);
                mca_btl_openib_endpoint_connected(ib_endpoint);
             }
            break;
            
        case MCA_BTL_IB_WAITING_ACK:
            mca_btl_openib_endpoint_connected(ib_endpoint);
            break;
            
         case MCA_BTL_IB_CONNECT_ACK:
            mca_btl_openib_endpoint_send_connect_data(ib_endpoint,
                                                      ENDPOINT_CONNECT_ACK);
            mca_btl_openib_endpoint_connected(ib_endpoint);
            break;

       case MCA_BTL_IB_CONNECTED :
            break;
        default :
            BTL_ERROR(("Invalid endpoint state %d", endpoint_state));
        }
        OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
        break;
    }
}

/* 
 *  Post the OOB recv (for receiving the peers information)
 */ 
void mca_btl_openib_post_recv()
{
    
    orte_rml.recv_buffer_nb(
        ORTE_NAME_WILDCARD, 
        ORTE_RML_TAG_DYNAMIC-1, 
        ORTE_RML_PERSISTENT,
        mca_btl_openib_endpoint_recv,
        NULL);
}


/*
 * Attempt to send a fragment using a given endpoint. If the endpoint is not
 * connected, queue the fragment and start the connection as required.
 */

int mca_btl_openib_endpoint_send(
                             mca_btl_base_endpoint_t* endpoint,
                             mca_btl_openib_frag_t* frag
                             )
{
    int rc;
    bool call_progress = false;
    mca_btl_openib_module_t *openib_btl; 
    
    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
    switch(endpoint->endpoint_state) {
        case MCA_BTL_IB_CONNECTING:

            BTL_VERBOSE(("Queing because state is connecting"));
            
            opal_list_append(&endpoint->pending_lazy_frags,
                    (opal_list_item_t *)frag);
            call_progress = true;
            rc = OMPI_SUCCESS;
            break;

        case MCA_BTL_IB_CONNECT_ACK:
        case MCA_BTL_IB_WAITING_ACK:
            BTL_VERBOSE(("Queuing because waiting for ack"));

            opal_list_append(&endpoint->pending_lazy_frags,
                    (opal_list_item_t *)frag);
            call_progress = true;
            rc = OMPI_SUCCESS;
            break;

        case MCA_BTL_IB_CLOSED:

            BTL_VERBOSE(("Connection to endpoint closed ... connecting ..."));
            opal_list_append(&endpoint->pending_lazy_frags,
                    (opal_list_item_t *)frag);
            rc = mca_btl_openib_endpoint_start_connect(endpoint);
            /**
             * As long as we expect a message from the peer (in order to setup the connection)
             * let the event engine pool the OOB events. Note: we increment it once peer active
             * connection.
             */
            opal_progress_event_users_increment();
            call_progress = true;
            break;

        case MCA_BTL_IB_FAILED:

            rc = OMPI_ERR_UNREACH;
            break;

        case MCA_BTL_IB_CONNECTED:
            {
                openib_btl = endpoint->endpoint_btl;
                BTL_VERBOSE(("Send to : %d, len : %lu, frag : %p", 
                              endpoint->endpoint_proc->proc_guid.vpid,
                              frag->sg_entry.length,
                              frag));
                rc = mca_btl_openib_endpoint_post_send(openib_btl, endpoint, frag); 
                break; 
            }

    default:
        rc = OMPI_ERR_UNREACH;
    }
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
    if(call_progress) opal_progress();
    return rc;
}

/*
 * Complete connection to endpoint.
 */

static int mca_btl_openib_endpoint_connect_qp(
       mca_btl_openib_endpoint_t *endpoint, int prio, int qp)
{
    int rc;
    mca_btl_openib_module_t* openib_btl =
        (mca_btl_openib_module_t*)endpoint->endpoint_btl;

    /* Connection establishment RC */
    rc = mca_btl_openib_endpoint_qp_init_query(openib_btl,
            endpoint->qps[qp].lcl_qp,
            endpoint->qps[qp].lcl_qp_attr,
            endpoint->qps[qp].lcl_psn,
            endpoint->rem_info.rem_qps[qp].rem_qp_num,
            endpoint->rem_info.rem_qps[qp].rem_psn,
            endpoint->rem_info.rem_lid,
            endpoint->rem_info.rem_mtu,
            openib_btl->port_num);
    
    return rc;
}

int mca_btl_openib_endpoint_connect(
    mca_btl_openib_endpoint_t *endpoint)
{
    int rc;
    int qp;
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
        if( mca_btl_openib_component.qp_infos[qp].size <= mca_btl_openib_component.eager_limit) { 
            rc = mca_btl_openib_endpoint_connect_qp(endpoint, 
                                                    BTL_OPENIB_HP_CQ, 
                                                    qp);
        } else { 
            rc = mca_btl_openib_endpoint_connect_qp(endpoint, 
                                                    BTL_OPENIB_LP_CQ, 
                                                    qp);
        }
        if(rc != OMPI_SUCCESS) {
            return rc;
        }
    }
    
    return OMPI_SUCCESS;
}

/* 
 * Create the queue pair note that this is just the initial 
 *  queue pair creation and we need to get the remote queue pair 
 *  info from the peer before the qp is usable, 
 */ 

int mca_btl_openib_endpoint_create_qp(
                                      mca_btl_openib_module_t* openib_btl, 
                                      struct ibv_pd* pd, 
                                      struct ibv_cq* cq, 
                                      struct ibv_srq* srq, 
                                      struct ibv_qp_attr* qp_attr,
                                      struct ibv_qp** qp,
                                      int qp_idx
                                      )
{
    {
        struct ibv_qp* my_qp; 
        struct ibv_qp_init_attr qp_init_attr; 

        memset(&qp_init_attr, 0, sizeof(struct ibv_qp_init_attr)); 

        qp_init_attr.send_cq = cq; 
        qp_init_attr.recv_cq = cq; 
        
        if(MCA_BTL_OPENIB_PP_QP == mca_btl_openib_component.qp_infos[qp_idx].type) { 
            qp_init_attr.cap.max_recv_wr =
                mca_btl_openib_component.qp_infos[qp_idx].rd_num +
                mca_btl_openib_component.qp_infos[qp_idx].u.pp_qp.rd_rsv;
            qp_init_attr.cap.max_send_wr =
                mca_btl_openib_component.qp_infos[qp_idx].rd_num + 1;
        } else { 
            qp_init_attr.cap.max_recv_wr =
                mca_btl_openib_component.qp_infos[qp_idx].rd_num;
            qp_init_attr.cap.max_send_wr =
                mca_btl_openib_component.qp_infos[qp_idx].u.srq_qp.sd_max +
                BTL_OPENIB_EAGER_RDMA_QP(qp_idx);
        }
        
        qp_init_attr.cap.max_send_sge = mca_btl_openib_component.ib_sg_list_size;
        qp_init_attr.cap.max_recv_sge = mca_btl_openib_component.ib_sg_list_size;
        qp_init_attr.qp_type = IBV_QPT_RC; 
        qp_init_attr.srq = srq; 
        my_qp = ibv_create_qp(pd, &qp_init_attr); 
    
        if(NULL == my_qp) { 
            BTL_ERROR(("error creating qp errno says %s", strerror(errno))); 
            return OMPI_ERROR; 
        }
        (*qp) = my_qp; 
        openib_btl->ib_inline_max = qp_init_attr.cap.max_inline_data; 
    }
    
    {
        qp_attr->qp_state = IBV_QPS_INIT; 
        qp_attr->pkey_index = openib_btl->pkey_index; /*mca_btl_openib_component.ib_pkey_ix; */
        qp_attr->port_num = openib_btl->port_num; 
        qp_attr->qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ; 

        if(ibv_modify_qp((*qp), qp_attr, 
                         IBV_QP_STATE | 
                         IBV_QP_PKEY_INDEX | 
                         IBV_QP_PORT | 
                         IBV_QP_ACCESS_FLAGS )) { 
            BTL_ERROR(("error modifying qp to INIT errno says %s", strerror(errno))); 
            return OMPI_ERROR; 
        } 
    } 

    return OMPI_SUCCESS;
}

/* 
 * The queue pair has been created and we have received the remote 
 *  queue pair information from the peer so we init this queue pair 
 *  and are ready to roll. 
 */ 
int mca_btl_openib_endpoint_qp_init_query(
                                          mca_btl_openib_module_t* openib_btl, 
                                          struct ibv_qp* qp, 
                                          struct ibv_qp_attr* attr,
                                          uint32_t lcl_psn, 
                                          uint32_t rem_qp_num, 
                                          uint32_t rem_psn,  
                                          uint16_t rem_lid, 
                                          uint32_t rem_mtu,
                                          uint32_t port_num 
                                          )
     
     
{
    attr->qp_state = IBV_QPS_RTR; 
    attr->path_mtu = (openib_btl->hca->mtu < rem_mtu) ? 
        openib_btl->hca->mtu : rem_mtu;
    if (mca_btl_openib_component.verbose) {
        BTL_OUTPUT(("Set MTU to IBV value %d (%s bytes)", attr->path_mtu,
                    (attr->path_mtu == IBV_MTU_256) ? "256" :
                    (attr->path_mtu == IBV_MTU_512) ? "512" :
                    (attr->path_mtu == IBV_MTU_1024) ? "1024" :
                    (attr->path_mtu == IBV_MTU_2048) ? "2048" :
                    (attr->path_mtu == IBV_MTU_4096) ? "4096" :
                    "unknown (!)"));
    }
    attr->dest_qp_num = rem_qp_num; 
    attr->rq_psn = rem_psn; 
    attr->max_dest_rd_atomic = mca_btl_openib_component.ib_max_rdma_dst_ops; 
    attr->min_rnr_timer = mca_btl_openib_component.ib_min_rnr_timer; 
    attr->ah_attr.is_global = 0; 
    attr->ah_attr.dlid = rem_lid; 
    attr->ah_attr.sl = mca_btl_openib_component.ib_service_level; 
    attr->ah_attr.src_path_bits = openib_btl->src_path_bits;
    attr->ah_attr.port_num = port_num; 
    attr->ah_attr.static_rate = mca_btl_openib_component.ib_static_rate;
    
    if(ibv_modify_qp(qp, attr, 
                     IBV_QP_STATE              |
                     IBV_QP_AV                 |
                     IBV_QP_PATH_MTU           |
                     IBV_QP_DEST_QPN           |
                     IBV_QP_RQ_PSN             |
                     IBV_QP_MAX_DEST_RD_ATOMIC |
                     IBV_QP_MIN_RNR_TIMER)) {
        BTL_ERROR(("error modifing QP to RTR errno says %s",  strerror(errno))); 
        return OMPI_ERROR; 
    }
    attr->qp_state 	     = IBV_QPS_RTS;
    attr->timeout 	     = mca_btl_openib_component.ib_timeout;
    attr->retry_cnt 	 = mca_btl_openib_component.ib_retry_count;
    attr->rnr_retry 	 = mca_btl_openib_component.ib_rnr_retry;
    attr->sq_psn 	     = lcl_psn;
    attr->max_rd_atomic  = mca_btl_openib_component.ib_max_rdma_dst_ops;
    if (ibv_modify_qp(qp, attr,
                      IBV_QP_STATE              |
                      IBV_QP_TIMEOUT            |
                      IBV_QP_RETRY_CNT          |
                      IBV_QP_RNR_RETRY          |
                      IBV_QP_SQ_PSN             |
                      IBV_QP_MAX_QP_RD_ATOMIC)) {
        BTL_ERROR(("error modifying QP to RTS errno says %s", strerror(errno))); 
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}


/**
 * Return control fragment.
 */

static void mca_btl_openib_endpoint_credits(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    
    int32_t checks, qp;
    
    mca_btl_openib_frag_t *frag = (mca_btl_openib_frag_t*) descriptor;
    
    qp = frag->qp_idx;
   
    /* we don't acquire a wqe or token for credit message - so decrement */
    OPAL_THREAD_ADD32(&endpoint->qps[qp].sd_wqe, -1);

    /* check to see if there are additional credits to return */
    if((checks = OPAL_THREAD_ADD32(&endpoint->qps[qp].rd_pending_credit_chks,-1)) > 0) {
        OPAL_THREAD_ADD32(&endpoint->qps[qp].rd_pending_credit_chks, -checks);
        if(btl_openib_check_send_credits(endpoint, qp)) {
            mca_btl_openib_endpoint_send_credits(endpoint, qp);
        }
    }
}

/**
 * Return credits to peer
 */
                                                                                                                             
void mca_btl_openib_endpoint_send_credits(mca_btl_openib_endpoint_t* endpoint,
        const int qp)
{
    mca_btl_openib_module_t* openib_btl = endpoint->endpoint_btl;
    mca_btl_openib_frag_t* frag;
    mca_btl_openib_rdma_credits_header_t *credits_hdr;
    int do_rdma = 0, ib_rc;

    frag = endpoint->qps[qp].credit_frag;

    if(NULL == frag) {
        MCA_BTL_IB_FRAG_ALLOC_CREDIT_WAIT(openib_btl, frag, ib_rc);
        frag->qp_idx = qp;
        endpoint->qps[qp].credit_frag = frag;
    }

    assert(frag->qp_idx == qp);
    credits_hdr =
        (mca_btl_openib_rdma_credits_header_t*)frag->segment.seg_addr.pval;
    if(BTL_OPENIB_EAGER_RDMA_QP(qp)) {
        if(OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens, -1) < 0) {
            OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens, 1);
        } else {
            do_rdma = 1;
        }
    }

    if(0 == do_rdma) {
        if(OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_sent, 1) >
                (mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv - 1)) {
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_sent, -1);
            OPAL_THREAD_ADD32(&endpoint->qps[qp].rd_pending_credit_chks,
                    -endpoint->qps[qp].rd_pending_credit_chks);
            return;
        }
     }

    frag->base.des_cbfunc = mca_btl_openib_endpoint_credits;
    frag->base.des_cbdata = NULL;
    frag->endpoint = endpoint;

    frag->hdr->tag = MCA_BTL_TAG_BTL;
    /* send credits for high/low prios */
    if(endpoint->qps[qp].u.pp_qp.rd_credits > 0) {
        frag->hdr->credits = endpoint->qps[qp].u.pp_qp.rd_credits;
        OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.rd_credits, -frag->hdr->credits);
        
    } else {
        frag->hdr->credits = 0;
    }
    if(endpoint->qps[qp].u.pp_qp.cm_return) {
        frag->hdr->cm_seen = endpoint->qps[qp].u.pp_qp.cm_return;
        OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_return,
                -frag->hdr->cm_seen);
    } else {
        frag->hdr->cm_seen = 0;
    }

    /* send eager RDMA credits only for high prio */
    if(BTL_OPENIB_EAGER_RDMA_QP(qp) && endpoint->eager_rdma_local.credits > 0) {
        credits_hdr->rdma_credits = endpoint->eager_rdma_local.credits;
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits,
                -credits_hdr->rdma_credits);
    } else {
        credits_hdr->rdma_credits = 0;
    }
    credits_hdr->control.type = MCA_BTL_OPENIB_CONTROL_CREDITS;

    if(endpoint->nbo)
         BTL_OPENIB_RDMA_CREDITS_HEADER_HTON((*credits_hdr));
    frag->segment.seg_len = sizeof(mca_btl_openib_rdma_credits_header_t);
    frag->sg_entry.addr = (unsigned long)frag->hdr;

    if((ib_rc = post_send(openib_btl, endpoint, frag, qp, do_rdma))) { 
        if(endpoint->nbo) {
            BTL_OPENIB_HEADER_NTOH((*frag->hdr));
            BTL_OPENIB_RDMA_CREDITS_HEADER_NTOH((*credits_hdr));
        }
        OPAL_THREAD_ADD32(&endpoint->qps[qp].rd_pending_credit_chks, -1);
        OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.rd_credits, frag->hdr->credits);
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits, credits_hdr->rdma_credits);
        if(do_rdma)
            OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens, 1);
        else
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_sent, -1);
        BTL_ERROR(("error posting send request errno %d says %s", ib_rc,
                    strerror(errno)));
    }
}

/* local callback function for completion of eager rdma connect */
static void mca_btl_openib_endpoint_eager_rdma_connect_cb(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    MCA_BTL_IB_FRAG_RETURN(((mca_btl_openib_module_t*)btl),
                           ((mca_btl_openib_frag_t*)descriptor));
}

/* send the eager rdma conect message to the remote endpoint */
static int mca_btl_openib_endpoint_send_eager_rdma(
    mca_btl_base_endpoint_t* endpoint)
{
    mca_btl_openib_module_t* openib_btl = endpoint->endpoint_btl;
    mca_btl_openib_eager_rdma_header_t *rdma_hdr;
    mca_btl_openib_frag_t* frag;
    int rc;

    MCA_BTL_IB_FRAG_ALLOC_CREDIT_WAIT(openib_btl, frag, rc);
    if(NULL == frag) {
        return -1;
    }

    frag->base.des_cbfunc = mca_btl_openib_endpoint_eager_rdma_connect_cb;
    frag->base.des_cbdata = NULL;
    frag->endpoint = endpoint;
    frag->base.des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;

    frag->hdr->tag = MCA_BTL_TAG_BTL;
    rdma_hdr = (mca_btl_openib_eager_rdma_header_t*)frag->segment.seg_addr.pval;
    rdma_hdr->control.type = MCA_BTL_OPENIB_CONTROL_RDMA;
    rdma_hdr->rkey = endpoint->eager_rdma_local.reg->mr->rkey;
    rdma_hdr->rdma_start.lval = ompi_ptr_ptol(endpoint->eager_rdma_local.base.pval);
    BTL_VERBOSE(("sending rkey %lu, rdma_start.lval %llu, pval %p, ival %u type %d and sizeof(rdma_hdr) %d\n", 
               rdma_hdr->rkey, 
               rdma_hdr->rdma_start.lval, 
               rdma_hdr->rdma_start.pval,
               rdma_hdr->rdma_start.ival,
               rdma_hdr->control.type, 
               sizeof(mca_btl_openib_eager_rdma_header_t)
               ));
    frag->segment.seg_len = sizeof(mca_btl_openib_eager_rdma_header_t);
    
    if(endpoint->nbo) {
        BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_HTON((*rdma_hdr));
        
        BTL_VERBOSE(("after HTON: sending rkey %lu, rdma_start.lval %llu, pval %p, ival %u\n", 
                   rdma_hdr->rkey, 
                   rdma_hdr->rdma_start.lval,
                   rdma_hdr->rdma_start.pval,
                   rdma_hdr->rdma_start.ival
                   ));
    }
    if (mca_btl_openib_endpoint_send(endpoint, frag) != OMPI_SUCCESS) {
        MCA_BTL_IB_FRAG_RETURN(openib_btl, frag);
        BTL_ERROR(("Error sending RDMA buffer", strerror(errno)));
        return -1;
    }
    return 0;
}

/* Setup eager RDMA buffers and notify the remote endpoint*/
void mca_btl_openib_endpoint_connect_eager_rdma(
        mca_btl_openib_endpoint_t* endpoint)
{
    mca_btl_openib_module_t* openib_btl = endpoint->endpoint_btl;
    char *buf;
    mca_btl_openib_recv_frag_t *headers_buf;
    unsigned int i;
    orte_std_cntr_t index;

    /* Set local rdma pointer to 1 temporarily so other threads will not try
     * to enter the function */
    if(!opal_atomic_cmpset_ptr(&endpoint->eager_rdma_local.base.pval, NULL,
                (void*)1))
        return;

    headers_buf = (mca_btl_openib_recv_frag_t*)
        malloc(sizeof(mca_btl_openib_recv_frag_t) *
            mca_btl_openib_component.eager_rdma_num);

    if(NULL == headers_buf)
       goto unlock_rdma_local;

    buf = openib_btl->super.btl_mpool->mpool_alloc(openib_btl->super.btl_mpool,
            openib_btl->eager_rdma_frag_size *
            mca_btl_openib_component.eager_rdma_num,
            mca_btl_openib_component.buffer_alignment,
            MCA_MPOOL_FLAGS_CACHE_BYPASS,
            (mca_mpool_base_registration_t**)&endpoint->eager_rdma_local.reg);

    if(!buf)
       goto free_headers_buf;

    buf = buf + openib_btl->eager_rdma_frag_size -
        sizeof(mca_btl_openib_footer_t) - openib_btl->super.btl_eager_limit -
        sizeof(mca_btl_openib_header_t);

    for(i = 0; i < mca_btl_openib_component.eager_rdma_num; i++) {
        ompi_free_list_item_t *item;
        mca_btl_openib_recv_frag_t * frag;
        mca_btl_openib_frag_init_data_t init_data;

        item = (ompi_free_list_item_t*)&headers_buf[i];
        item->registration = (void*)endpoint->eager_rdma_local.reg;
        item->ptr = buf + i * openib_btl->eager_rdma_frag_size;
        OBJ_CONSTRUCT(item, mca_btl_openib_recv_frag_t);

        init_data.length = mca_btl_openib_component.eager_limit;
        init_data.order = mca_btl_openib_component.eager_rdma_qp;
        init_data.type = MCA_BTL_OPENIB_FRAG_EAGER_RDMA;
        init_data.list = NULL;
        
        mca_btl_openib_frag_init(item, &init_data);                                 
        frag = (mca_btl_openib_recv_frag_t*) item;
        frag->ftr = (mca_btl_openib_footer_t*)((char*)frag->segment.seg_addr.pval 
		    + frag->size);
        
        MCA_BTL_OPENIB_RDMA_MAKE_REMOTE(frag->ftr);
        ((mca_btl_openib_frag_t*)item)->endpoint = endpoint;
    }

    endpoint->eager_rdma_local.frags = headers_buf;

    endpoint->eager_rdma_local.rd_win =
        mca_btl_openib_component.eager_rdma_num >> 2;
    endpoint->eager_rdma_local.rd_win =
        endpoint->eager_rdma_local.rd_win?endpoint->eager_rdma_local.rd_win:1;

    /* set local rdma pointer to real value */
    opal_atomic_cmpset_ptr(&endpoint->eager_rdma_local.base.pval, (void*)1,
            buf);

    if(mca_btl_openib_endpoint_send_eager_rdma(endpoint) == 0) {
        /* This can never fail because max number of entries allocated
         * at init time */
        OBJ_RETAIN(endpoint);
        assert(((opal_object_t*)endpoint)->obj_reference_count == 2);
        orte_pointer_array_add(&index, openib_btl->eager_rdma_buffers,
                endpoint);
        /* from this point progress function starts to poll new buffer */
        OPAL_THREAD_ADD32(&openib_btl->eager_rdma_buffers_count, 1);
        return;
    }

    openib_btl->super.btl_mpool->mpool_free(openib_btl->super.btl_mpool,
           buf, (mca_mpool_base_registration_t*)endpoint->eager_rdma_local.reg);
free_headers_buf:
    free(headers_buf);
unlock_rdma_local:
    /* set local rdma pointer back to zero. Will retry later */
    opal_atomic_cmpset_ptr(&endpoint->eager_rdma_local.base.pval, 
            endpoint->eager_rdma_local.base.pval, NULL);
    endpoint->eager_rdma_local.frags = NULL;
}
