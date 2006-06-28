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

static void mca_btl_openib_endpoint_construct(mca_btl_base_endpoint_t* endpoint);
static void mca_btl_openib_endpoint_destruct(mca_btl_base_endpoint_t* endpoint);

int mca_btl_openib_endpoint_create_qp(
                                      mca_btl_openib_module_t* openib_btl, 
                                      struct ibv_pd* pd, 
                                      struct ibv_cq* cq, 
#if OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                                      struct ibv_srq* srq, 
#endif
                                      struct ibv_qp_attr* qp_attr,                                      
                                      struct ibv_qp** qp
                                      ); 



int mca_btl_openib_endpoint_qp_init_query(
                                          mca_btl_openib_module_t* openib_btl, 
                                          struct ibv_qp* qp, 
                                          struct ibv_qp_attr* attr,
                                          uint32_t lcl_psn, 
                                          uint32_t rem_qp_num, 
                                          uint32_t rem_psn,  
                                          uint16_t rem_lid, 
                                          uint32_t port_num 
                                          ); 
                   

/* 
 * post a send to the work queue 
 */ 
static inline int mca_btl_openib_endpoint_post_send(mca_btl_openib_module_t* openib_btl, 
                                                    mca_btl_openib_endpoint_t * endpoint, 
                                                    mca_btl_openib_frag_t * frag)
{ 
    int do_rdma = 0;
    struct ibv_qp* ib_qp; 
    struct ibv_send_wr* bad_wr; 
    frag->sg_entry.addr = (unsigned long) frag->hdr; 

    if((frag->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY) &&
            frag->size <= openib_btl->super.btl_eager_limit){ 
        /* check for a send wqe */
        if (OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp,-1) < 0) {
            OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp,1);
            opal_list_append(&endpoint->pending_frags_hp, 
                    (opal_list_item_t *)frag);
            return OMPI_SUCCESS;
	    } 
        /* check for rdma tocken */	
        if (OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens,-1) < 0) {
            OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens,1);
            /* check for a token */
            if(!mca_btl_openib_component.use_srq &&
                    OPAL_THREAD_ADD32(&endpoint->sd_tokens_hp,-1) < 0) {
                OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp,1);
                OPAL_THREAD_ADD32(&endpoint->sd_tokens_hp,1);
                opal_list_append(&endpoint->pending_frags_hp,
                        (opal_list_item_t *)frag);
                return OMPI_SUCCESS;
          } else if( mca_btl_openib_component.use_srq &&
                  OPAL_THREAD_ADD32(&openib_btl->sd_tokens_hp,-1) < 0) { 
                /* queue the request */
                OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp,1);
                OPAL_THREAD_ADD32(&openib_btl->sd_tokens_hp,1);
                OPAL_THREAD_LOCK(&openib_btl->ib_lock);
                opal_list_append(&openib_btl->pending_frags_hp,
                        (opal_list_item_t *)frag);
                OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
                return OMPI_SUCCESS;
          }
        } else {
            do_rdma = 1;
        }
        frag->hdr->credits =
            (endpoint->rd_credits_hp > 0) ? endpoint->rd_credits_hp : 0;
        OPAL_THREAD_ADD32(&endpoint->rd_credits_hp, -frag->hdr->credits);
        frag->hdr->rdma_credits = endpoint->eager_rdma_local.credits;
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits,
                -frag->hdr->rdma_credits);
        ib_qp = endpoint->lcl_qp_hp; 
    } else {
        /* check for a send wqe */
        if (OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,-1) < 0) {

            OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,1);
            opal_list_append(&endpoint->pending_frags_lp, (opal_list_item_t *)frag);
            return OMPI_SUCCESS;

        /* check for a token */
        } else if(!mca_btl_openib_component.use_srq &&
            OPAL_THREAD_ADD32(&endpoint->sd_tokens_lp,-1) < 0 ) {

            OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,1);
            OPAL_THREAD_ADD32(&endpoint->sd_tokens_lp,1);
            opal_list_append(&endpoint->pending_frags_lp, (opal_list_item_t *)frag); 
            return OMPI_SUCCESS;

        } else if(mca_btl_openib_component.use_srq &&
            OPAL_THREAD_ADD32(&openib_btl->sd_tokens_lp,-1) < 0) {

            OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,1);
            OPAL_THREAD_ADD32(&openib_btl->sd_tokens_lp,1);
            OPAL_THREAD_LOCK(&openib_btl->ib_lock);
            opal_list_append(&openib_btl->pending_frags_lp, (opal_list_item_t *)frag); 
            OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
            return OMPI_SUCCESS;

        /* queue the request */
        } else { 
            frag->hdr->credits = (endpoint->rd_credits_lp > 0) ? endpoint->rd_credits_lp : 0;
            OPAL_THREAD_ADD32(&endpoint->rd_credits_lp, -frag->hdr->credits);
            ib_qp = endpoint->lcl_qp_lp; 
        }
    } 
    
    frag->sg_entry.length =
        frag->segment.seg_len + sizeof(mca_btl_openib_header_t) +
        (do_rdma ? sizeof(mca_btl_openib_footer_t) : 0);
    if(frag->sg_entry.length <= openib_btl->ib_inline_max) { 
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED|IBV_SEND_INLINE;
    } else { 
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED; 
    }

    if(do_rdma) {
        mca_btl_openib_footer_t* ftr =
            (mca_btl_openib_footer_t*)(((char*)frag->segment.seg_addr.pval) +
                                       frag->segment.seg_len);
        frag->wr_desc.sr_desc.opcode = IBV_WR_RDMA_WRITE;
        MCA_BTL_OPENIB_RDMA_FRAG_SET_SIZE(ftr, frag->sg_entry.length);
        MCA_BTL_OPENIB_RDMA_MAKE_LOCAL(ftr);
#ifdef OMPI_ENABLE_DEBUG
        ((mca_btl_openib_footer_t*)(((char*)frag->segment.seg_addr.pval) +
                                    frag->segment.seg_len))->seq = 
            endpoint->eager_rdma_remote.seq++;
#endif
        frag->wr_desc.sr_desc.wr.rdma.rkey = endpoint->eager_rdma_remote.rkey;
        frag->wr_desc.sr_desc.wr.rdma.remote_addr =
            (uintptr_t)endpoint->eager_rdma_remote.base.pval +
            endpoint->eager_rdma_remote.head *
            openib_btl->eager_rdma_frag_size +
            sizeof(mca_btl_openib_frag_t) +
            sizeof(mca_btl_openib_header_t) +
            frag->size +
            sizeof(mca_btl_openib_footer_t);
        frag->wr_desc.sr_desc.wr.rdma.remote_addr -= frag->sg_entry.length;
        MCA_BTL_OPENIB_RDMA_NEXT_INDEX (endpoint->eager_rdma_remote.head);
    } else {
        frag->wr_desc.sr_desc.opcode = IBV_WR_SEND;
    }
    if(ibv_post_send(ib_qp, 
                     &frag->wr_desc.sr_desc, 
                     &bad_wr)) { 
        BTL_ERROR(("error posting send request errno says %s\n", 
                    strerror(errno))); 
        return OMPI_ERROR; 
    }
            
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ 
    if(mca_btl_openib_component.use_srq) { 
        MCA_BTL_OPENIB_POST_SRR_HIGH(openib_btl, 1); 
        MCA_BTL_OPENIB_POST_SRR_LOW(openib_btl, 1);         
    } else { 
#endif 
        MCA_BTL_OPENIB_ENDPOINT_POST_RR_HIGH(endpoint, 1); 
        MCA_BTL_OPENIB_ENDPOINT_POST_RR_LOW(endpoint, 1); 
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    }
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

static void mca_btl_openib_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{
    endpoint->endpoint_btl = 0;
    endpoint->endpoint_proc = 0;
    endpoint->endpoint_tstamp = 0.0;
    endpoint->endpoint_state = MCA_BTL_IB_CLOSED;
    endpoint->endpoint_retries = 0;
    OBJ_CONSTRUCT(&endpoint->endpoint_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&endpoint->pending_send_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->pending_frags_hp, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->pending_frags_lp, opal_list_t);
    
    endpoint->lcl_qp_attr_hp = (struct ibv_qp_attr *) malloc(sizeof(struct ibv_qp_attr)); 
    endpoint->lcl_qp_attr_lp = (struct ibv_qp_attr *) malloc(sizeof(struct ibv_qp_attr)); 
    memset(endpoint->lcl_qp_attr_hp, 0, sizeof(struct ibv_qp_attr)); 
    memset(endpoint->lcl_qp_attr_lp, 0, sizeof(struct ibv_qp_attr)); 

    endpoint->rd_posted_hp = 0;
    endpoint->rd_posted_lp = 0;

    /* number of available send wqes */
    endpoint->sd_wqe_hp = mca_btl_openib_component.rd_num;
    endpoint->sd_wqe_lp = mca_btl_openib_component.rd_num;

    /* zero these out w/ initial posting, so that we start out w/
     * zero credits to return to peer
     */
    endpoint->rd_credits_hp = -(mca_btl_openib_component.rd_num + mca_btl_openib_component.rd_rsv);
    endpoint->rd_credits_lp = -(mca_btl_openib_component.rd_num + mca_btl_openib_component.rd_rsv);
    endpoint->sd_credits_hp = 0;
    endpoint->sd_credits_lp = 0;

    /* initialize the high and low priority tokens */
    endpoint->sd_tokens_hp = mca_btl_openib_component.rd_num;
    endpoint->sd_tokens_lp = mca_btl_openib_component.rd_num;
    endpoint->get_tokens = mca_btl_openib_component.ib_qp_ous_rd_atom;

    /* initialize RDMA eager related parts */
    endpoint->eager_recv_count = 0;
    memset(&endpoint->eager_rdma_remote, 0,
		    sizeof(mca_btl_openib_eager_rdma_remote_t));
    memset (&endpoint->eager_rdma_local, 0,
		    sizeof(mca_btl_openib_eager_rdma_local_t));
    OBJ_CONSTRUCT(&endpoint->eager_rdma_local.lock, opal_mutex_t);

    endpoint->rem_info.rem_qp_num_hp = 0; 
    endpoint->rem_info.rem_qp_num_lp = 0; 
    endpoint->rem_info.rem_lid = 0; 
    endpoint->rem_info.rem_psn_hp = 0;
    endpoint->rem_info.rem_psn_lp = 0; 
    endpoint->rem_info.rem_subnet = 0; 
}

/*
 * Destroy a endpoint
 *
 */

static void mca_btl_openib_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
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


static int mca_btl_openib_endpoint_send_connect_data(mca_btl_base_endpoint_t* endpoint)
{
    orte_buffer_t* buffer = OBJ_NEW(orte_buffer_t);
    int rc;
    if(NULL == buffer) {
         ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
         return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* pack the info in the send buffer */

    rc = orte_dss.pack(buffer, &endpoint->lcl_qp_hp->qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    rc = orte_dss.pack(buffer, &endpoint->lcl_qp_lp->qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = orte_dss.pack(buffer, &endpoint->lcl_psn_hp, 1, ORTE_UINT32); 
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
     
    rc = orte_dss.pack(buffer, &endpoint->lcl_psn_lp, 1, ORTE_UINT32); 
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    rc = orte_dss.pack(buffer, &endpoint->endpoint_btl->lid, 1, ORTE_UINT16);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    
    rc = orte_dss.pack(buffer, &((mca_btl_openib_endpoint_t*) endpoint)->subnet, 1, ORTE_UINT16);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* send to endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, buffer, ORTE_RML_TAG_DYNAMIC-1, 0,
         mca_btl_openib_endpoint_send_cb, NULL);
    
    
    BTL_VERBOSE(("Sending High Priority QP num = %d, Low Priority QP num = %d, LID = %d",
              endpoint->lcl_qp_hp->qp_num,
              endpoint->lcl_qp_lp->qp_num,
              endpoint->endpoint_btl->lid));
    
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
static int mca_btl_openib_endpoint_set_remote_info(mca_btl_base_endpoint_t* endpoint, mca_btl_openib_rem_info_t* rem_info)
{
    
    memcpy(&((mca_btl_openib_endpoint_t*) endpoint)->rem_info, rem_info, sizeof(mca_btl_openib_rem_info_t)); 
    
    BTL_VERBOSE(("Setting High Priority QP num = %d, Low Priority QP num %d,  LID = %d",
                 endpoint->rem_info.rem_qp_num_hp,
                 endpoint->rem_info.rem_qp_num_lp, 
                 endpoint->rem_info.rem_lid));

    return ORTE_SUCCESS;

}



/*
 * Start to connect to the endpoint. We send our Queue Pair
 * information over the TCP OOB communication mechanism.

 * On completion of our send, a send completion handler 
 * is called.
 *
 */

static int mca_btl_openib_endpoint_start_connect(mca_btl_base_endpoint_t* endpoint)
{
    int rc;
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) endpoint->endpoint_btl; 
    
    
    /* Create the High Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_create_qp(openib_btl, 
                                                               openib_btl->hca->ib_pd, 
                                                               openib_btl->ib_cq_hp, 
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                                                               openib_btl->srq_hp, 
#endif
                                                               endpoint->lcl_qp_attr_hp, 
                                                               &endpoint->lcl_qp_hp))) { 
        BTL_ERROR(("error creating queue pair, error code %d", rc)); 
        return rc;
    }
    srand48(getpid() * time(NULL));
    endpoint->lcl_psn_hp = lrand48() & 0xffffff; 
    
    /* Create the Low Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_create_qp(openib_btl, 
                                                               openib_btl->hca->ib_pd, 
                                                               openib_btl->ib_cq_lp, 
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                                                               openib_btl->srq_lp, 
#endif
                                                              endpoint->lcl_qp_attr_lp, 
                                                               &endpoint->lcl_qp_lp))) { 
        BTL_ERROR(("error creating queue pair, error code %d", rc)); 
        return rc;
    }
    endpoint->lcl_psn_lp = lrand48() & 0xffffff; 

    BTL_VERBOSE(("Initialized High Priority QP num = %d, Low Priority QP num = %d,  LID = %d",
              endpoint->lcl_qp_hp->qp_num,
              endpoint->lcl_qp_lp->qp_num, 
              openib_btl->lid)); 

    /* Send connection info over to remote endpoint */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_send_connect_data(endpoint))) {
        BTL_ERROR(("error sending connect request, error code %d", rc)); 
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 * Reply to a `start - connect' message
 *
 */
static int mca_btl_openib_endpoint_reply_start_connect(mca_btl_openib_endpoint_t *endpoint, 
                                                       mca_btl_openib_rem_info_t *rem_info)
{
    int rc;
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) endpoint->endpoint_btl; 
        
        
    /* Create the High Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_create_qp(openib_btl, 
                                                               openib_btl->hca->ib_pd, 
                                                               openib_btl->ib_cq_hp,  
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                                                               openib_btl->srq_hp, 
#endif

                                                               endpoint->lcl_qp_attr_hp, 
                                                               &endpoint->lcl_qp_hp))) { 
        BTL_ERROR(("error creating queue pair, error code %d", rc)); 
        return rc;
    }
    srand48(getpid() * time(NULL));
    endpoint->lcl_psn_hp = lrand48() & 0xffffff;
    
    /* Create the Low Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_create_qp(openib_btl, 
                                                               openib_btl->hca->ib_pd, 
                                                               openib_btl->ib_cq_lp, 
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                                                               openib_btl->srq_lp, 
#endif

                                                               endpoint->lcl_qp_attr_lp, 
                                                               &endpoint->lcl_qp_lp))) { 
        BTL_ERROR(("error creating queue pair, error code %d", rc)); 
        return rc;
    }
    endpoint->lcl_psn_lp = lrand48() & 0xffffff; 

    BTL_VERBOSE(("Initialized High Priority QP num = %d, Low Priority QP num = %d,  LID = %d",
              endpoint->lcl_qp_hp->qp_num,
              endpoint->lcl_qp_lp->qp_num, 
              openib_btl->lid)); 


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
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_send_connect_data(endpoint))) {
        BTL_ERROR(("error in endpoint send connect request error code is %d", rc)); 
        return rc;
    }
    return OMPI_SUCCESS;
}

/* 
 *  endpoint is waiting ack to final connection establishment.. 
 */

static void mca_btl_openib_endpoint_waiting_ack(mca_btl_openib_endpoint_t *endpoint) { 
    endpoint->endpoint_state = MCA_BTL_IB_WAITING_ACK; 
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
    opal_progress_event_decrement();

    /* While there are frags in the list,
     * process them */

    while(!opal_list_is_empty(&(endpoint->pending_send_frags))) {
        frag_item = opal_list_remove_first(&(endpoint->pending_send_frags));
        frag = (mca_btl_openib_frag_t *) frag_item;
        openib_btl = endpoint->endpoint_btl;
        /* We need to post this one */
        
        if(OMPI_SUCCESS !=  mca_btl_openib_endpoint_post_send(openib_btl, endpoint, frag))
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
    orte_process_name_t* endpoint, 
    orte_buffer_t* buffer,
    orte_rml_tag_t tag, 
    void* cbdata)
{
    mca_btl_openib_proc_t *ib_proc;
    mca_btl_openib_endpoint_t *ib_endpoint;
    int endpoint_state;
    int rc;
    uint32_t i; 
    size_t cnt = 1; 
    mca_btl_openib_rem_info_t rem_info; 
    
    /* start by unpacking data first so we know who is knocking at 
       our door */ 
    
    rc = orte_dss.unpack(buffer, &rem_info.rem_qp_num_hp, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    rc = orte_dss.unpack(buffer, &rem_info.rem_qp_num_lp, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    rc = orte_dss.unpack(buffer, &rem_info.rem_psn_hp, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }rc = orte_dss.unpack(buffer, &rem_info.rem_psn_lp, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    rc = orte_dss.unpack(buffer, &rem_info.rem_lid, &cnt, ORTE_UINT16);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    rc = orte_dss.unpack(buffer, &rem_info.rem_subnet, &cnt, ORTE_UINT16);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
#if 0
    rc = orte_dss.unpack(buffer, &ib_endpoint->rdma_buf->r_key, &cnt, ORTE_UINT32); 
    if(rc != ORTE_SUCCESS) { 
        ORTE_ERROR_LOG(rc); 
        return rc; 
    }
    
    rc = orte_dss.unpack(buffer, &ib_endpoint->rdma_buf->rem_base, &cnt, ORTE_UINT32); 
    if(rc != ORTE_SUCCESS) { 
        ORTE_ERROR_LOG(rc); 
        return rc; 
    }
    
    rc = orte_dss.unpack(buffer, &ib_endpoint->rdma_buf->rem_size, &cnt, ORTE_UINT32); 
    if(rc != ORTE_SUCCESS) { 
        ORTE_ERROR_LOG(rc); 
        return rc; 
    }

    rc = orte_dss.unpack(buffer, &ib_endpoint->rdma_buf->rem_cnt, &cnt, ORTE_UINT32); 
    if(rc != ORTE_SUCCESS) { 
        ORTE_ERROR_LOG(rc); 
        return rc; 
    }
#endif

    BTL_VERBOSE(("Received High Priority QP num = %d, Low Priority QP num %d,  LID = %d",
                 rem_info.rem_qp_num_hp,
                 rem_info.rem_qp_num_lp, 
                 rem_info.rem_lid));

    for(ib_proc = (mca_btl_openib_proc_t*)
            opal_list_get_first(&mca_btl_openib_component.ib_procs);
            ib_proc != (mca_btl_openib_proc_t*)
            opal_list_get_end(&mca_btl_openib_component.ib_procs);
            ib_proc  = (mca_btl_openib_proc_t*)opal_list_get_next(ib_proc)) {

        if(orte_ns.compare(ORTE_NS_CMP_ALL, &ib_proc->proc_guid, endpoint) == 0) {
            bool found = false;
            
            /* Try to get the endpoint instance of this proc */

            for(i = 0; i < ib_proc->proc_endpoint_count; i++) { 
                mca_btl_openib_port_info_t port_info; 
                port_info = ib_proc->proc_ports[i]; 
                ib_endpoint = ib_proc->proc_endpoints[i]; 
                if(ib_endpoint->rem_info.rem_lid && 
                   (ib_endpoint->rem_info.rem_lid == rem_info.rem_lid && 
                    ib_endpoint->rem_info.rem_qp_num_hp == rem_info.rem_qp_num_hp)) { 
                    /* we've seen them before! */ 
                    found = true; 
                    break;
                }
            }
            /* If we haven't seen this remote lid before then try to match on 
               endpoint */ 
            for(i = 0; !found && i < ib_proc->proc_endpoint_count; i++) { 
                mca_btl_openib_port_info_t port_info; 
                port_info = ib_proc->proc_ports[i]; 
                ib_endpoint = ib_proc->proc_endpoints[i]; 
                if(!ib_endpoint->rem_info.rem_lid && 
                   ib_endpoint->subnet  == rem_info.rem_subnet) { 
                    /* found a match based on subnet! */ 
                    found = true; 
                    break;
                }
            }
            /* try finding an open port, even if subnets  
               don't match
            */ 
            for(i = 0; !found && i < ib_proc->proc_endpoint_count; i++) { 
                mca_btl_openib_port_info_t port_info; 
                port_info = ib_proc->proc_ports[i]; 
                ib_endpoint = ib_proc->proc_endpoints[i]; 
                if(!ib_endpoint->rem_info.rem_lid) { 
                    /* found an unused end-point */ 
                    found = true; 
                    break;
                }
            }
            
            if(!found) { 
                BTL_ERROR(("can't find suitable endpoint for this peer\n")); 
                return; 
            }
           
            OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock); 
            endpoint_state = ib_endpoint->endpoint_state;

            /* Update status */
            switch(endpoint_state) {
            case MCA_BTL_IB_CLOSED :
                /* We had this connection closed before.
                 * The endpoint is trying to connect. Move the
                 * status of this connection to CONNECTING,
                 * and then reply with our QP information */
                
                if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_reply_start_connect(ib_endpoint, &rem_info))) {
                    BTL_ERROR(("error in endpoint reply start connect")); 
                    break;
                }
                                                                                     
                /** As long as we expect a message from the peer (in order to setup the connection)
                 * let the event engine pool the OOB events. Note: we increment it once peer active
                 * connection.
                 */
                opal_progress_event_increment();
                break;
                
            case MCA_BTL_IB_CONNECTING :

                mca_btl_openib_endpoint_set_remote_info(ib_endpoint, &rem_info);
                if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_connect(ib_endpoint))) {
                    BTL_ERROR(("endpoint connect error: %d", rc)); 
                    break;
                }
                    
                /* Setup state as awaiting ack from peer */
                mca_btl_openib_endpoint_waiting_ack(ib_endpoint);

                /* Send him an ack */
                mca_btl_openib_endpoint_send_connect_data(ib_endpoint);
                break;
                
            case MCA_BTL_IB_WAITING_ACK:
                mca_btl_openib_endpoint_connected(ib_endpoint);
                break; 
                
            case MCA_BTL_IB_CONNECT_ACK:
                mca_btl_openib_endpoint_send_connect_data(ib_endpoint);
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
}

/* 
 *  Post the OOB recv (for receiving the peers information)
 */ 
void mca_btl_openib_post_recv()
{
    
    orte_rml.recv_buffer_nb(
        ORTE_RML_NAME_ANY, 
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
            
            opal_list_append(&endpoint->pending_send_frags,
                    (opal_list_item_t *)frag);
            call_progress = true;
            rc = OMPI_SUCCESS;
            break;

        case MCA_BTL_IB_CONNECT_ACK:
        case MCA_BTL_IB_WAITING_ACK:
            BTL_VERBOSE(("Queuing because waiting for ack"));

            opal_list_append(&endpoint->pending_send_frags,
                    (opal_list_item_t *)frag);
            call_progress = true;
            rc = OMPI_SUCCESS;
            break;

        case MCA_BTL_IB_CLOSED:

            BTL_VERBOSE(("Connection to endpoint closed ... connecting ..."));
            opal_list_append(&endpoint->pending_send_frags,
                    (opal_list_item_t *)frag);
            rc = mca_btl_openib_endpoint_start_connect(endpoint);
            /**
             * As long as we expect a message from the peer (in order to setup the connection)
             * let the event engine pool the OOB events. Note: we increment it once peer active
             * connection.
             */
            opal_progress_event_increment();
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

int mca_btl_openib_endpoint_connect(
    mca_btl_openib_endpoint_t *endpoint)
{
    int rc;
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) endpoint->endpoint_btl; 

    /* Connection establishment RC */
    rc = mca_btl_openib_endpoint_qp_init_query(
                                               openib_btl, 
                                               endpoint->lcl_qp_hp, 
                                               endpoint->lcl_qp_attr_hp, 
                                               endpoint->lcl_psn_hp, 
                                               endpoint->rem_info.rem_qp_num_hp, 
                                               endpoint->rem_info.rem_psn_hp, 
                                               endpoint->rem_info.rem_lid, 
                                               openib_btl->port_num
                                               ); 
    
    
    
    if(rc != OMPI_SUCCESS) {
        return rc;
    }
    rc = mca_btl_openib_endpoint_qp_init_query(
                                               openib_btl, 
                                               endpoint->lcl_qp_lp, 
                                               endpoint->lcl_qp_attr_lp, 
                                               endpoint->lcl_psn_lp, 
                                               endpoint->rem_info.rem_qp_num_lp, 
                                               endpoint->rem_info.rem_psn_lp, 
                                               endpoint->rem_info.rem_lid, 
                                               openib_btl->port_num
                                               ); 
    
    
    
    if(rc != OMPI_SUCCESS) {
        return rc;
    }
             
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ 
    if(mca_btl_openib_component.use_srq) { 
        MCA_BTL_OPENIB_POST_SRR_HIGH(openib_btl, 1); 
        MCA_BTL_OPENIB_POST_SRR_LOW(openib_btl, 1);         
    } else { 
#endif 
        MCA_BTL_OPENIB_ENDPOINT_POST_RR_HIGH(endpoint, 1); 
        MCA_BTL_OPENIB_ENDPOINT_POST_RR_LOW(endpoint, 1); 
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    }
#endif 
    
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
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                                      struct ibv_srq* srq, 
#endif
                                      struct ibv_qp_attr* qp_attr,
                                      struct ibv_qp** qp
                                      )
{
    {
        struct ibv_qp* my_qp; 
        struct ibv_qp_init_attr qp_init_attr; 

        memset(&qp_init_attr, 0, sizeof(struct ibv_qp_init_attr)); 

        qp_init_attr.send_cq = cq; 
        qp_init_attr.recv_cq = cq; 
        qp_init_attr.cap.max_send_wr = mca_btl_openib_component.rd_num + 1;
        qp_init_attr.cap.max_recv_wr = mca_btl_openib_component.rd_num + mca_btl_openib_component.rd_rsv;
        qp_init_attr.cap.max_send_sge = mca_btl_openib_component.ib_sg_list_size;
        qp_init_attr.cap.max_recv_sge = mca_btl_openib_component.ib_sg_list_size;
        qp_init_attr.qp_type = IBV_QPT_RC; 
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ 
        if(mca_btl_openib_component.use_srq) { 
            qp_init_attr.srq = srq; 
        }
#endif
        my_qp = ibv_create_qp(pd, &qp_init_attr); 
    
        if(NULL == my_qp) { 
            BTL_ERROR(("error creating qp errno says %s", strerror(errno))); 
            return OMPI_ERROR; 
        }
        (*qp) = my_qp; 
        if(0 == (openib_btl->ib_inline_max = qp_init_attr.cap.max_inline_data)) {
            BTL_ERROR(("ibv_create_qp: returned 0 byte(s) for max inline data"));
        }
    }
    
    {
        qp_attr->qp_state = IBV_QPS_INIT; 
        qp_attr->pkey_index = mca_btl_openib_component.ib_pkey_ix; 
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
                                          uint32_t port_num 
                                          )
     
     
{
    attr->qp_state = IBV_QPS_RTR; 
    attr->path_mtu = mca_btl_openib_component.ib_mtu; 
    attr->dest_qp_num = rem_qp_num; 
    attr->rq_psn = rem_psn; 
    attr->max_dest_rd_atomic = mca_btl_openib_component.ib_max_rdma_dst_ops; 
    attr->min_rnr_timer = mca_btl_openib_component.ib_min_rnr_timer; 
    attr->ah_attr.is_global = 0; 
    attr->ah_attr.dlid = rem_lid; 
    attr->ah_attr.sl = mca_btl_openib_component.ib_service_level; 
    attr->ah_attr.src_path_bits = openib_btl->src_path_bits;
    attr->ah_attr.port_num = port_num; 
    
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

static void mca_btl_openib_endpoint_credits_lp(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    int32_t credits;

    MCA_BTL_IB_FRAG_RETURN(((mca_btl_openib_module_t*)btl), 
                           ((mca_btl_openib_frag_t*)descriptor));

    /* we don't acquire a wqe or token for credit message - so decrement */
    OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,-1);

    /* check to see if there are addditional credits to return */
    if ((credits = OPAL_THREAD_ADD32(&endpoint->sd_credits_lp,-1)) > 0) {
        OPAL_THREAD_ADD32(&endpoint->sd_credits_lp,-credits);
        if (endpoint->rd_credits_lp >= mca_btl_openib_component.rd_win &&
            OPAL_THREAD_ADD32(&endpoint->sd_credits_lp,1) == 1) {
            mca_btl_openib_endpoint_send_credits_lp(endpoint);
        }
    }
}

/**
 * Return credits to peer
 */
                                                                                                                             
void mca_btl_openib_endpoint_send_credits_lp(
    mca_btl_openib_endpoint_t* endpoint)
{
    mca_btl_openib_module_t* openib_btl = endpoint->endpoint_btl;
    mca_btl_openib_frag_t* frag;
    struct ibv_send_wr* bad_wr; 
    int rc;

    MCA_BTL_IB_FRAG_ALLOC_EAGER(openib_btl, frag, rc);
    if(NULL == frag) {
        BTL_ERROR(("error allocating fragment"));
        return;
    }

    frag->base.des_cbfunc = mca_btl_openib_endpoint_credits_lp;
    frag->base.des_cbdata = NULL;
    frag->endpoint = endpoint;

    frag->hdr->tag = MCA_BTL_TAG_BTL;
    frag->hdr->credits = endpoint->rd_credits_lp;
    OPAL_THREAD_ADD32(&endpoint->rd_credits_lp, -frag->hdr->credits);
    ((mca_btl_openib_control_header_t *)frag->segment.seg_addr.pval)->type = MCA_BTL_OPENIB_CONTROL_NOOP;

    frag->wr_desc.sr_desc.opcode = IBV_WR_SEND;
    frag->sg_entry.length = sizeof(mca_btl_openib_header_t) +
                            sizeof(mca_btl_openib_control_header_t);
    frag->sg_entry.addr = (unsigned long) frag->hdr; 
    
    if(frag->sg_entry.length <= openib_btl->ib_inline_max) { 
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_INLINE | IBV_SEND_SIGNALED; 
    } else {
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED; 
    }

    if(ibv_post_send(endpoint->lcl_qp_lp,
                         &frag->wr_desc.sr_desc,
                         &bad_wr)) {
        OPAL_THREAD_ADD32(&endpoint->sd_credits_lp, -1);
        OPAL_THREAD_ADD32(&endpoint->rd_credits_lp, frag->hdr->credits);
        MCA_BTL_IB_FRAG_RETURN(openib_btl, frag);
        BTL_ERROR(("error posting send request errno %d says %s", strerror(errno)));
        return;
    }
}


/**
 * Return control fragment.
 */

static void mca_btl_openib_endpoint_credits_hp(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    int32_t credits;

    MCA_BTL_IB_FRAG_RETURN(((mca_btl_openib_module_t*)btl), 
                           ((mca_btl_openib_frag_t*)descriptor));

    /* we don't acquire a wqe or token for credit message - so decrement */
    OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp,-1);

    /* check to see if there are addditional credits to return */
    if ((credits = OPAL_THREAD_ADD32(&endpoint->sd_credits_hp,-1)) > 0) {
        OPAL_THREAD_ADD32(&endpoint->sd_credits_hp,-credits);
        if ((endpoint->rd_credits_hp >= mca_btl_openib_component.rd_win ||
	    endpoint->eager_rdma_local.credits >= mca_btl_openib_component.rd_win) &&
            OPAL_THREAD_ADD32(&endpoint->sd_credits_hp,1) == 1) {
            mca_btl_openib_endpoint_send_credits_hp(endpoint);
        } 
    }
}

/**
 * Return credits to peer
 */
                                                                                                                             
void mca_btl_openib_endpoint_send_credits_hp(
    mca_btl_openib_endpoint_t* endpoint)
{
    mca_btl_openib_module_t* openib_btl = endpoint->endpoint_btl;
    mca_btl_openib_frag_t* frag;
    struct ibv_send_wr* bad_wr; 
    int rc;

    MCA_BTL_IB_FRAG_ALLOC_EAGER(openib_btl, frag, rc);
    if(NULL == frag) {
        BTL_ERROR(("error allocating fragment"));
        return;
    }

    frag->base.des_cbfunc = mca_btl_openib_endpoint_credits_hp;
    frag->base.des_cbdata = NULL;
    frag->endpoint = endpoint;

    frag->hdr->tag = MCA_BTL_TAG_BTL;
    frag->hdr->credits =
        (endpoint->rd_credits_hp > 0) ? endpoint->rd_credits_hp : 0;
    OPAL_THREAD_ADD32(&endpoint->rd_credits_hp, -frag->hdr->credits);
    frag->hdr->rdma_credits = endpoint->eager_rdma_local.credits;
    OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits,
            -frag->hdr->rdma_credits);
    ((mca_btl_openib_control_header_t *)frag->segment.seg_addr.pval)->type = MCA_BTL_OPENIB_CONTROL_NOOP;

    frag->wr_desc.sr_desc.opcode = IBV_WR_SEND;
    frag->sg_entry.length = sizeof(mca_btl_openib_header_t) +
                            sizeof(mca_btl_openib_control_header_t);
    frag->sg_entry.addr = (unsigned long) frag->hdr; 
    
    if(frag->sg_entry.length <= openib_btl->ib_inline_max) { 
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_INLINE | IBV_SEND_SIGNALED; 
    } else {
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED; 
    }

    if(ibv_post_send(endpoint->lcl_qp_hp,
                         &frag->wr_desc.sr_desc,
                         &bad_wr)) {
        OPAL_THREAD_ADD32(&endpoint->sd_credits_hp, -1);
        OPAL_THREAD_ADD32(&endpoint->rd_credits_hp, frag->hdr->credits);
        MCA_BTL_IB_FRAG_RETURN(openib_btl, frag);
        BTL_ERROR(("error posting send request errno %d says %s", errno, 
                    strerror(errno)));
        return;
    }
}

static void mca_btl_openib_endpoint_eager_rdma(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    MCA_BTL_IB_FRAG_RETURN(((mca_btl_openib_module_t*)btl),
                           ((mca_btl_openib_frag_t*)descriptor));
}

static int mca_btl_openib_endpoint_send_eager_rdma(
    mca_btl_base_endpoint_t* endpoint)
{
    mca_btl_openib_module_t* openib_btl = endpoint->endpoint_btl;
    mca_btl_openib_eager_rdma_header_t *rdma_hdr;
    mca_btl_openib_frag_t* frag;
    int rc;

    MCA_BTL_IB_FRAG_ALLOC_EAGER(openib_btl, frag, rc);
    if(NULL == frag) {
        BTL_ERROR(("error allocating fragment"));
        return -1;
    }

    frag->base.des_cbfunc = mca_btl_openib_endpoint_eager_rdma;
    frag->base.des_cbdata = NULL;
    frag->endpoint = endpoint;
    frag->base.des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;

    frag->hdr->tag = MCA_BTL_TAG_BTL;
    rdma_hdr = (mca_btl_openib_eager_rdma_header_t*)frag->segment.seg_addr.pval;
    rdma_hdr->control.type = MCA_BTL_OPENIB_CONTROL_RDMA;
    rdma_hdr->rkey = endpoint->eager_rdma_local.reg->mr->rkey;
    rdma_hdr->rdma_start.pval = endpoint->eager_rdma_local.base.pval;
    frag->segment.seg_len = sizeof(mca_btl_openib_eager_rdma_header_t);
    if (mca_btl_openib_endpoint_post_send(openib_btl, endpoint, frag) !=
            OMPI_SUCCESS) {
        MCA_BTL_IB_FRAG_RETURN(openib_btl, frag);
        BTL_ERROR(("Error sending RDMA buffer", strerror(errno)));
        return -1;
    }
    return 0;
}
/* create RDMA buffer for eager messages */
void mca_btl_openib_endpoint_connect_eager_rdma(
        mca_btl_openib_endpoint_t* endpoint)
{
    mca_btl_openib_module_t* openib_btl = endpoint->endpoint_btl;
    char *buf;
    unsigned int i;

    OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
    if (endpoint->eager_rdma_local.base.pval)
        goto unlock_rdma_local;

    buf = openib_btl->super.btl_mpool->mpool_alloc(openib_btl->super.btl_mpool,
            openib_btl->eager_rdma_frag_size * 
            mca_btl_openib_component.eager_rdma_num, 0, 0,
            (mca_mpool_base_registration_t**)&endpoint->eager_rdma_local.reg);

    if(!buf)
       goto unlock_rdma_local;

    for(i = 0; i < mca_btl_openib_component.eager_rdma_num; i++) {
        ompi_free_list_item_t *item = (ompi_free_list_item_t *)(buf +
                i*openib_btl->eager_rdma_frag_size);
        item->user_data = endpoint->eager_rdma_local.reg;
        OBJ_CONSTRUCT(item, mca_btl_openib_recv_frag_eager_t);
        ((mca_btl_openib_frag_t*)item)->endpoint = endpoint;
        ((mca_btl_openib_frag_t*)item)->type = MCA_BTL_OPENIB_FRAG_EAGER_RDMA;
    }

    OPAL_THREAD_LOCK(&openib_btl->eager_rdma_lock);
    if(orte_pointer_array_add (&endpoint->eager_rdma_index,
                openib_btl->eager_rdma_buffers, endpoint) < 0)
	   goto cleanup;

    endpoint->eager_rdma_local.base.pval = buf;
    openib_btl->eager_rdma_buffers_count++;
    if (mca_btl_openib_endpoint_send_eager_rdma(endpoint) == 0) {
        OPAL_THREAD_UNLOCK(&openib_btl->eager_rdma_lock);
        OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
        return;
    }

    openib_btl->eager_rdma_buffers_count--;
    endpoint->eager_rdma_local.base.pval = NULL;
    orte_pointer_array_set_item(openib_btl->eager_rdma_buffers,
            endpoint->eager_rdma_index, NULL);

cleanup:
    OPAL_THREAD_UNLOCK(&openib_btl->eager_rdma_lock);
    openib_btl->super.btl_mpool->mpool_free(openib_btl->super.btl_mpool,
            buf, (mca_mpool_base_registration_t*)endpoint->eager_rdma_local.reg);
unlock_rdma_local:
    OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
}
