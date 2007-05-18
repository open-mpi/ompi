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
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
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
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "btl_mvapi.h"
#include "btl_mvapi_endpoint.h" 
#include "btl_mvapi_proc.h"
#include "btl_mvapi_frag.h"
#include "ompi/class/ompi_free_list.h" 

static void mca_btl_mvapi_endpoint_construct(mca_btl_base_endpoint_t* endpoint);
static void mca_btl_mvapi_endpoint_destruct(mca_btl_base_endpoint_t* endpoint);

int mca_btl_mvapi_endpoint_create_qp(
    mca_btl_mvapi_module_t* mvapi_btl, 
    VAPI_hca_hndl_t nic,
    VAPI_pd_hndl_t ptag, 
    VAPI_cq_hndl_t cq_hndl, 
#ifdef VAPI_FEATURE_SRQ
    VAPI_srq_hndl_t srq_hndl, 
#endif
    VAPI_qp_hndl_t* qp_hndl, 
    VAPI_qp_prop_t* qp_prop, 
    int transport_type
); 

int mca_btl_mvapi_endpoint_qp_init_query(
    mca_btl_mvapi_module_t* mvapi_btl, 
    VAPI_hca_hndl_t nic, 
    VAPI_qp_hndl_t qp_hndl, 
    VAPI_qp_num_t remote_qp_num, 
    IB_lid_t remote_lid, 
    IB_port_t port_id
); 

/*
 * Note this routine must be called w/ endpoint lock held.
 */
                   
static inline int mca_btl_mvapi_endpoint_post_send(
    mca_btl_mvapi_module_t* mvapi_btl, 
    mca_btl_mvapi_endpoint_t * endpoint, 
    mca_btl_mvapi_frag_t * frag)
{
    int do_rdma = 0; 
    VAPI_qp_hndl_t qp_hndl; 
    int ret;

    if(frag->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY &&
            frag->size <= mvapi_btl->super.btl_eager_limit){ 

        /* check for a send wqe */
        if (OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp,-1) < 0) {
            OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp,1);
            opal_list_append(&endpoint->pending_frags_hp, (opal_list_item_t *)frag);
            return OMPI_SUCCESS;
        }
        /* check for rdma tocken */
        if (OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens,-1) < 0) {
            OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens,1);
            /* check for a token */
            if(!mca_btl_mvapi_component.use_srq &&
                    OPAL_THREAD_ADD32(&endpoint->sd_tokens_hp,-1) < 0) {
                OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp,1);
                OPAL_THREAD_ADD32(&endpoint->sd_tokens_hp,1);
                opal_list_append(&endpoint->pending_frags_hp,
                        (opal_list_item_t *)frag);
                return OMPI_SUCCESS;

            } else if( mca_btl_mvapi_component.use_srq &&
                   OPAL_THREAD_ADD32(&mvapi_btl->sd_tokens_hp,-1) < 0) {
                OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp,1);
                OPAL_THREAD_ADD32(&mvapi_btl->sd_tokens_hp,1);
                OPAL_THREAD_LOCK(&mvapi_btl->ib_lock);
                opal_list_append(&mvapi_btl->pending_frags_hp, (opal_list_item_t *)frag);
                OPAL_THREAD_UNLOCK(&mvapi_btl->ib_lock);
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
        qp_hndl = endpoint->lcl_qp_hndl_hp;
    } else {
        /* check for a send wqe */
        if (OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,-1) < 0) {

            OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,1);
            opal_list_append(&endpoint->pending_frags_lp, (opal_list_item_t *)frag);
            return OMPI_SUCCESS;

        /* check for a token */
        } else if(!mca_btl_mvapi_component.use_srq &&
            OPAL_THREAD_ADD32(&endpoint->sd_tokens_lp,-1) < 0 ) {

            OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,1);
            OPAL_THREAD_ADD32(&endpoint->sd_tokens_lp,1);
            opal_list_append(&endpoint->pending_frags_lp, (opal_list_item_t *)frag);
            return OMPI_SUCCESS;

        } else if(mca_btl_mvapi_component.use_srq &&
            OPAL_THREAD_ADD32(&mvapi_btl->sd_tokens_lp,-1) < 0) {

            OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,1);
            OPAL_THREAD_ADD32(&mvapi_btl->sd_tokens_lp,1);
            OPAL_THREAD_LOCK(&mvapi_btl->ib_lock);
            opal_list_append(&mvapi_btl->pending_frags_lp, (opal_list_item_t *)frag);
            OPAL_THREAD_UNLOCK(&mvapi_btl->ib_lock);
            return OMPI_SUCCESS;

        /* queue the request */
        } else {
            frag->hdr->credits = (endpoint->rd_credits_lp > 0) ? endpoint->rd_credits_lp : 0;
            OPAL_THREAD_ADD32(&endpoint->rd_credits_lp, -frag->hdr->credits);
            qp_hndl = endpoint->lcl_qp_hndl_lp;
        }
    } 
    
    frag->desc.sr_desc.remote_qkey = 0; 
    frag->sg_entry.addr = (VAPI_virt_addr_t) (MT_virt_addr_t) frag->hdr; 
    frag->sg_entry.len =
        frag->segment.seg_len + sizeof(mca_btl_mvapi_header_t) + 
        (do_rdma ? sizeof(mca_btl_mvapi_footer_t) : 0);

    if(do_rdma) {
        mca_btl_mvapi_footer_t* ftr =
            (mca_btl_mvapi_footer_t*)(((char*)frag->segment.seg_addr.pval) +
                                       frag->segment.seg_len);
        frag->desc.sr_desc.opcode = VAPI_RDMA_WRITE;
        MCA_BTL_MVAPI_RDMA_FRAG_SET_SIZE(ftr, frag->sg_entry.len);
        MCA_BTL_MVAPI_RDMA_MAKE_LOCAL(ftr);
#ifdef OMPI_ENABLE_DEBUG
        ftr->seq = endpoint->eager_rdma_remote.seq++;
#endif
        frag->desc.sr_desc.r_key = (VAPI_rkey_t)endpoint->eager_rdma_remote.rkey;
        frag->desc.sr_desc.remote_addr = (VAPI_virt_addr_t)
            endpoint->eager_rdma_remote.base.lval +
            endpoint->eager_rdma_remote.head *
            mvapi_btl->eager_rdma_frag_size +
            sizeof(mca_btl_mvapi_frag_t) +
            sizeof(mca_btl_mvapi_header_t) +
            frag->size +
            sizeof(mca_btl_mvapi_footer_t);
        frag->desc.sr_desc.remote_addr -= frag->sg_entry.len;
        MCA_BTL_MVAPI_RDMA_NEXT_INDEX (endpoint->eager_rdma_remote.head);
    } else {
        frag->desc.sr_desc.opcode = VAPI_SEND;
    }


    if(frag->sg_entry.len <= mvapi_btl->ib_inline_max) { 
        ret = EVAPI_post_inline_sr(mvapi_btl->nic, qp_hndl, &frag->desc.sr_desc); 
    } else { 
        ret = VAPI_post_sr(mvapi_btl->nic, qp_hndl, &frag->desc.sr_desc); 
    }

    if(VAPI_OK != ret) {
        BTL_ERROR(("VAPI_post_sr: %s\n", VAPI_strerror(ret)));
        return OMPI_ERROR; 
    }
#ifdef VAPI_FEATURE_SRQ
    if(mca_btl_mvapi_component.use_srq) { 
        MCA_BTL_MVAPI_POST_SRR_HIGH(mvapi_btl, 1); 
        MCA_BTL_MVAPI_POST_SRR_LOW(mvapi_btl, 1);
    } else
#endif
    {
        MCA_BTL_MVAPI_ENDPOINT_POST_RR_HIGH(endpoint, 1); 
        MCA_BTL_MVAPI_ENDPOINT_POST_RR_LOW(endpoint, 1); 
    }
    return OMPI_SUCCESS; 
}



OBJ_CLASS_INSTANCE(mca_btl_mvapi_endpoint_t, 
                   opal_list_item_t, mca_btl_mvapi_endpoint_construct, 
                   mca_btl_mvapi_endpoint_destruct);

/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_btl_mvapi_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
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
    
    endpoint->rd_posted_hp = 0;
    endpoint->rd_posted_lp = 0;

    /* number of available send wqes */
    endpoint->sd_wqe_hp = mca_btl_mvapi_component.rd_num;
    endpoint->sd_wqe_lp = mca_btl_mvapi_component.rd_num;

    /* zero these out w/ initial posting, so that we start out w/
     * zero credits to return to peer
     */
    endpoint->rd_credits_hp = -(mca_btl_mvapi_component.rd_num + mca_btl_mvapi_component.rd_rsv);
    endpoint->rd_credits_lp = -(mca_btl_mvapi_component.rd_num + mca_btl_mvapi_component.rd_rsv);
    endpoint->sd_credits_hp = 0;
    endpoint->sd_credits_lp = 0;

    /* initialize the high and low priority tokens */ 
    endpoint->sd_tokens_hp = mca_btl_mvapi_component.rd_num; 
    endpoint->sd_tokens_lp = mca_btl_mvapi_component.rd_num; 
    endpoint->get_tokens = mca_btl_mvapi_component.ib_qp_ous_rd_atom;

    /* initialize RDMA eager related parts */
    endpoint->eager_recv_count = 0;
    memset(&endpoint->eager_rdma_remote, 0,
           sizeof(mca_btl_mvapi_eager_rdma_remote_t));
    memset (&endpoint->eager_rdma_local, 0,
           sizeof(mca_btl_mvapi_eager_rdma_local_t));
    OBJ_CONSTRUCT(&endpoint->eager_rdma_local.lock, opal_mutex_t);

    endpoint->rem_info.rem_qp_num_hp = 0; 
    endpoint->rem_info.rem_qp_num_lp = 0; 
    endpoint->rem_info.rem_lid = 0; 
    endpoint->rem_info.rem_subnet = 0; 
}

/*
 * Destroy a endpoint
 *
 */

static void mca_btl_mvapi_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
    OBJ_DESTRUCT(&endpoint->endpoint_lock);
    OBJ_DESTRUCT(&endpoint->pending_send_frags);
    OBJ_DESTRUCT(&endpoint->pending_frags_hp);
    OBJ_DESTRUCT(&endpoint->pending_frags_lp);
}

/*
 * Send connection information to remote endpoint using OOB
 *
 */

static void mca_btl_mvapi_endpoint_send_cb(
    int status,
    orte_process_name_t* endpoint, 
    orte_buffer_t* buffer,
    orte_rml_tag_t tag, 
    void* cbdata)
{
    OBJ_RELEASE(buffer);
}


static int mca_btl_mvapi_endpoint_send_connect_data(mca_btl_base_endpoint_t* endpoint)
{
    orte_buffer_t* buffer = OBJ_NEW(orte_buffer_t);
    int rc;
    if(NULL == buffer) {
         ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
         return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* pack the info in the send buffer */

    rc = orte_dss.pack(buffer, &endpoint->lcl_qp_prop_hp.qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    rc = orte_dss.pack(buffer, &endpoint->lcl_qp_prop_lp.qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dss.pack(buffer, &endpoint->endpoint_btl->port.lid, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    rc = orte_dss.pack(buffer, &((mca_btl_mvapi_endpoint_t*)endpoint)->subnet, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* send to endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, buffer, ORTE_RML_TAG_DYNAMIC-1, 0,
         mca_btl_mvapi_endpoint_send_cb, NULL);
    
    
    BTL_VERBOSE(("Sending High Priority QP num = %d, Low Priority QP num = %d, LID = %d",
              endpoint->lcl_qp_prop_hp.qp_num,
              endpoint->lcl_qp_prop_lp.qp_num,
              endpoint->endpoint_btl->port.lid));
    
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 * Set remote connection info
 *
 * XXX: Currently size is unutilized, this shall change
 * as soon as we add more info to be exchanged at connection
 * setup.
 *
 */
static int mca_btl_mvapi_endpoint_set_remote_info(mca_btl_base_endpoint_t* endpoint, mca_btl_mvapi_rem_info_t* rem_info)
{
    
    memcpy(&((mca_btl_mvapi_endpoint_t*) endpoint)->rem_info, rem_info, sizeof(mca_btl_mvapi_rem_info_t)); 
    
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

static int mca_btl_mvapi_endpoint_start_connect(mca_btl_base_endpoint_t* endpoint)
{
    int rc;
    
    
    /* Create the High Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_mvapi_endpoint_create_qp(endpoint->endpoint_btl, 
                                                              endpoint->endpoint_btl->nic, 
                                                              endpoint->endpoint_btl->ptag, 
                                                              endpoint->endpoint_btl->cq_hndl_hp, 
#ifdef VAPI_FEATURE_SRQ
                                                              endpoint->endpoint_btl->srq_hndl_hp, 
#endif
                                                              &endpoint->lcl_qp_hndl_hp, 
                                                              &endpoint->lcl_qp_prop_hp, 
                                                              VAPI_TS_RC))) {
        BTL_ERROR(("error creating queue pair, error code %d", rc)); 
        return rc;
    }

    
    /* Create the Low Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_mvapi_endpoint_create_qp(endpoint->endpoint_btl, 
                                                              endpoint->endpoint_btl->nic, 
                                                              endpoint->endpoint_btl->ptag, 
                                                              endpoint->endpoint_btl->cq_hndl_lp, 
#ifdef VAPI_FEATURE_SRQ
                                                              endpoint->endpoint_btl->srq_hndl_lp, 
#endif
                                                              &endpoint->lcl_qp_hndl_lp, 
                                                              &endpoint->lcl_qp_prop_lp, 
                                                              VAPI_TS_RC))) {
        
        BTL_ERROR(("error creating queue pair, error code %d", rc)); 
        return rc;
    }

    BTL_VERBOSE(("Initialized High Priority QP num = %d, Low Priority QP num = %d,  LID = %d",
              endpoint->lcl_qp_prop_hp.qp_num,
              endpoint->lcl_qp_prop_lp.qp_num,
              endpoint->endpoint_btl->port.lid));

    /* Send connection info over to remote endpoint */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
    if(OMPI_SUCCESS != (rc = mca_btl_mvapi_endpoint_send_connect_data(endpoint))) {
        BTL_ERROR(("error sending connect request, error code %d", rc)); 
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 * Reply to a `start - connect' message
 *
 */
static int mca_btl_mvapi_endpoint_reply_start_connect(mca_btl_mvapi_endpoint_t *endpoint, 
                                                      mca_btl_mvapi_rem_info_t* rem_info)
{
    int rc;

    
    /* Create the High Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_mvapi_endpoint_create_qp(endpoint->endpoint_btl, 
                                                              endpoint->endpoint_btl->nic, 
                                                              endpoint->endpoint_btl->ptag, 
                                                              endpoint->endpoint_btl->cq_hndl_hp, 
#ifdef VAPI_FEATURE_SRQ
                                                              endpoint->endpoint_btl->srq_hndl_hp, 
#endif
                                                              &endpoint->lcl_qp_hndl_hp, 
                                                              &endpoint->lcl_qp_prop_hp, 
                                                              VAPI_TS_RC))) {
        BTL_ERROR(("error creating queue pair, error code %d", rc)); 
        return rc;
    }


    /* Create the Low Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_mvapi_endpoint_create_qp(endpoint->endpoint_btl, 
                                                              endpoint->endpoint_btl->nic, 
                                                              endpoint->endpoint_btl->ptag, 
                                                              endpoint->endpoint_btl->cq_hndl_lp, 
#ifdef VAPI_FEATURE_SRQ
                                                              endpoint->endpoint_btl->srq_hndl_lp, 
#endif
                                                              &endpoint->lcl_qp_hndl_lp, 
                                                              &endpoint->lcl_qp_prop_lp, 
                                                              VAPI_TS_RC))) {
        BTL_ERROR(("error creating queue pair, error code %d", rc)); 
        return rc;
    }

    BTL_VERBOSE(("Initialized High Priority QP num = %d, Low Priority QP num = %d,  LID = %d",
              endpoint->lcl_qp_prop_hp.qp_num,
              endpoint->lcl_qp_prop_lp.qp_num,
              endpoint->endpoint_btl->port.lid));




    /* Set the remote side info */
    mca_btl_mvapi_endpoint_set_remote_info(endpoint, rem_info);
    
    /* Connect to endpoint */
    rc = mca_btl_mvapi_endpoint_connect(endpoint);
    if(rc != OMPI_SUCCESS) {
        BTL_ERROR(("error in endpoint connect error code is %d", rc)); 
        return rc;
    }

    /* Send connection info over to remote endpoint */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECT_ACK;
    if(OMPI_SUCCESS != (rc = mca_btl_mvapi_endpoint_send_connect_data(endpoint))) {
        BTL_ERROR(("error in endpoint send connect request error code is %d", rc)); 
        return rc;
    }
    return OMPI_SUCCESS;
}

static void mca_btl_mvapi_endpoint_waiting_ack(mca_btl_mvapi_endpoint_t *endpoint) { 
    endpoint->endpoint_state = MCA_BTL_IB_WAITING_ACK; 
}

/*
 *
 */

static void mca_btl_mvapi_endpoint_connected(mca_btl_mvapi_endpoint_t *endpoint)
{
    opal_list_item_t *frag_item;
    mca_btl_mvapi_frag_t *frag;
    mca_btl_mvapi_module_t* mvapi_btl; 

    /* While there are frags in the list, process them */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTED;

    /**
     * The connection is correctly setup. Now we can decrease the event trigger.
     */
    opal_progress_event_decrement();

    while(!opal_list_is_empty(&(endpoint->pending_send_frags))) {
        frag_item = opal_list_remove_first(&(endpoint->pending_send_frags));
        frag = (mca_btl_mvapi_frag_t *) frag_item;
        mvapi_btl = endpoint->endpoint_btl;
        /* We need to post this one */
        
        if(OMPI_SUCCESS !=  mca_btl_mvapi_endpoint_post_send(mvapi_btl, endpoint, frag))
            BTL_ERROR(("error in mca_btl_mvapi_endpoint_send"));
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

static void mca_btl_mvapi_endpoint_recv(
    int status,
    orte_process_name_t* endpoint, 
    orte_buffer_t* buffer,
    orte_rml_tag_t tag, 
    void* cbdata)
{
    mca_btl_mvapi_proc_t *ib_proc;
    mca_btl_mvapi_endpoint_t *ib_endpoint = NULL;
    int endpoint_state;
    int rc; 
    uint32_t i;
    int32_t cnt = 1; 
    mca_btl_mvapi_rem_info_t rem_info; 


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
    rc = orte_dss.unpack(buffer, &rem_info.rem_lid, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    rc = orte_dss.unpack(buffer, &rem_info.rem_subnet, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    BTL_VERBOSE(("Received High Priority QP num = %d, Low Priority QP num %d,  LID = %d",
                 rem_info.rem_qp_num_hp,
                 rem_info.rem_qp_num_lp, 
                 rem_info.rem_lid));

    for(ib_proc = (mca_btl_mvapi_proc_t*)
            opal_list_get_first(&mca_btl_mvapi_component.ib_procs);
            ib_proc != (mca_btl_mvapi_proc_t*)
            opal_list_get_end(&mca_btl_mvapi_component.ib_procs);
            ib_proc  = (mca_btl_mvapi_proc_t*)opal_list_get_next(ib_proc)) {

        if(orte_ns.compare_fields(ORTE_NS_CMP_ALL, &ib_proc->proc_guid, endpoint) == ORTE_EQUAL) {
            bool found = false;
            
            /* Try to get the endpoint instance of this proc */
            /* first match the endpoint based on lid meaning we've seen */ 
            /* this endpoint before.. */ 
            for(i = 0; i < ib_proc->proc_endpoint_count; i++) { 
                mca_btl_mvapi_port_info_t port_info; 
                port_info = ib_proc->proc_ports[i]; 
                ib_endpoint = ib_proc->proc_endpoints[i]; 
                if(ib_endpoint->rem_info.rem_lid && 
                   ib_endpoint->rem_info.rem_lid  == rem_info.rem_lid) { 
                    /* we've seen them before! */ 
                    found = true; 
                    break;
                }
            }
            /* If we haven't seen this remote lid before then try to match on 
               endpoint */ 
            for(i = 0; !found && i < ib_proc->proc_endpoint_count; i++) { 
                mca_btl_mvapi_port_info_t port_info; 
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
                mca_btl_mvapi_port_info_t port_info; 
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

                if(OMPI_SUCCESS != (rc = mca_btl_mvapi_endpoint_reply_start_connect(ib_endpoint, &rem_info))) {
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
                
                mca_btl_mvapi_endpoint_set_remote_info(ib_endpoint, &rem_info);
                if(OMPI_SUCCESS != (rc = mca_btl_mvapi_endpoint_connect(ib_endpoint))) {
                    BTL_ERROR(("endpoint connect error: %d", rc)); 
                    break;
                }

                /* Setup state as awaiting ack from peer */
                mca_btl_mvapi_endpoint_waiting_ack(ib_endpoint);

                /* Send him an ack */
                mca_btl_mvapi_endpoint_send_connect_data(ib_endpoint);
                break;

            case MCA_BTL_IB_WAITING_ACK: 
                mca_btl_mvapi_endpoint_connected(ib_endpoint);
                break; 
                
            case MCA_BTL_IB_CONNECT_ACK:

                mca_btl_mvapi_endpoint_send_connect_data(ib_endpoint);
                mca_btl_mvapi_endpoint_connected(ib_endpoint);
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

void mca_btl_mvapi_post_recv()
{
    orte_rml.recv_buffer_nb(
        ORTE_NAME_WILDCARD, 
        ORTE_RML_TAG_DYNAMIC-1, 
        ORTE_RML_PERSISTENT,
        mca_btl_mvapi_endpoint_recv,
        NULL);
}


/*
 * Attempt to send a fragment using a given endpoint. If the endpoint is not
 * connected, queue the fragment and start the connection as required.
 */

int mca_btl_mvapi_endpoint_send(
                             mca_btl_base_endpoint_t* endpoint,
                             mca_btl_mvapi_frag_t* frag
                             )
{
    int rc, call_progress = 0;
    mca_btl_mvapi_module_t *mvapi_btl; 
    
    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
    switch(endpoint->endpoint_state) {
        case MCA_BTL_IB_CONNECTING:

            BTL_VERBOSE(("Queing because state is connecting"));

            opal_list_append(&endpoint->pending_send_frags,
                    (opal_list_item_t *)frag);

            rc = OMPI_SUCCESS;
            call_progress = 1;
            break;

        case MCA_BTL_IB_WAITING_ACK: 
        case MCA_BTL_IB_CONNECT_ACK:

            BTL_VERBOSE(("Queuing because waiting for ack"));

            opal_list_append(&endpoint->pending_send_frags,
                    (opal_list_item_t *)frag);

            rc = OMPI_SUCCESS;
            call_progress = 1;
            break;

        case MCA_BTL_IB_CLOSED:

            BTL_VERBOSE(("Connection to endpoint closed ... connecting ..."));

            opal_list_append(&endpoint->pending_send_frags,
                    (opal_list_item_t *)frag);

            rc = mca_btl_mvapi_endpoint_start_connect(endpoint);
            /**
             * As long as we expect a message from the peer (in order to setup the connection)
             * let the event engine pool the OOB events. Note: we increment it once peer active
             * connection.
             */
            opal_progress_event_increment();
            call_progress = 1;
            break;

        case MCA_BTL_IB_FAILED:

            rc = OMPI_ERR_UNREACH;
            break;

        case MCA_BTL_IB_CONNECTED:
            {
                mvapi_btl = endpoint->endpoint_btl;
                BTL_VERBOSE(("Send to : %d, len : %d, frag : %p", 
                             endpoint->endpoint_proc->proc_guid.vpid,
                             frag->sg_entry.len,
                             frag));
                rc = mca_btl_mvapi_endpoint_post_send(mvapi_btl, endpoint, frag); 
            }   
            break;
            
    default:
        rc = OMPI_ERR_UNREACH;
    }
    
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
    if( call_progress ) opal_progress();
    return rc;
}




/*
 * Complete connection to endpoint.
 */

int mca_btl_mvapi_endpoint_connect(
    mca_btl_mvapi_endpoint_t *endpoint)
{
    int rc;
    /* Connection establishment RC */
    rc = mca_btl_mvapi_endpoint_qp_init_query(endpoint->endpoint_btl, 
                                              endpoint->endpoint_btl->nic, 
                                              endpoint->lcl_qp_hndl_hp, 
                                              endpoint->rem_info.rem_qp_num_hp, 
                                              endpoint->rem_info.rem_lid,                                            
                                              endpoint->endpoint_btl->port_id); 
    
    rc = mca_btl_mvapi_endpoint_qp_init_query(endpoint->endpoint_btl, 
                                              endpoint->endpoint_btl->nic, 
                                              endpoint->lcl_qp_hndl_lp, 
                                              endpoint->rem_info.rem_qp_num_lp, 
                                              endpoint->rem_info.rem_lid,                                            
                                              endpoint->endpoint_btl->port_id); 
    
    

    if(rc != OMPI_SUCCESS) {
        return rc;
    }
    
#ifdef VAPI_FEATURE_SRQ
    if(mca_btl_mvapi_component.use_srq) { 
        MCA_BTL_MVAPI_POST_SRR_HIGH(endpoint->endpoint_btl, 0); 
        MCA_BTL_MVAPI_POST_SRR_LOW(endpoint->endpoint_btl, 0);
    } else 
#endif
    {
        MCA_BTL_MVAPI_ENDPOINT_POST_RR_HIGH(endpoint, 0); 
        MCA_BTL_MVAPI_ENDPOINT_POST_RR_LOW(endpoint, 0); 
    }
    return OMPI_SUCCESS;
}

/* 
 * Create the queue pair note that this is just the initial 
 *  queue pair creation and we need to get the remote queue pair 
 *  info from the peer before the qp is usable, 
 */ 

int mca_btl_mvapi_endpoint_create_qp(
                                  mca_btl_mvapi_module_t* mvapi_btl, 
                                  VAPI_hca_hndl_t nic,
                                  VAPI_pd_hndl_t ptag, 
                                  VAPI_cq_hndl_t cq_hndl, 
#ifdef VAPI_FEATURE_SRQ
                                  VAPI_srq_hndl_t srq_hndl, 
#endif
                                  VAPI_qp_hndl_t* qp_hndl, 
                                  VAPI_qp_prop_t* qp_prop, 
                                  int transport_type)
{
    
    VAPI_ret_t ret;
    VAPI_qp_init_attr_t qp_init_attr;
#ifdef VAPI_FEATURE_SRQ
    VAPI_qp_init_attr_ext_t qp_init_attr_ext;
#endif

    /* worst case number of credit messages could be queued */
    switch(transport_type) {

    case VAPI_TS_RC: /* Set up RC qp parameters */
        qp_init_attr.cap.max_oust_wr_sq = mca_btl_mvapi_component.rd_num + 1;
        qp_init_attr.cap.max_oust_wr_rq = mca_btl_mvapi_component.rd_num + mca_btl_mvapi_component.rd_rsv;
        qp_init_attr.cap.max_sg_size_sq = mca_btl_mvapi_component.ib_sg_list_size;
        qp_init_attr.cap.max_sg_size_rq = mca_btl_mvapi_component.ib_sg_list_size;
        qp_init_attr.pd_hndl            = ptag;
        /* We don't have Reliable Datagram Handle right now */
        qp_init_attr.rdd_hndl           = 0;
        
        /* Signal all work requests on this queue pair */
        qp_init_attr.rq_sig_type        = VAPI_SIGNAL_REQ_WR;
        qp_init_attr.sq_sig_type        = VAPI_SIGNAL_REQ_WR;
        
        /* Use Reliable Connected  transport service */
        qp_init_attr.ts_type            = VAPI_TS_RC;
        
            
        
        /* Set Send and Recv completion queues */
        qp_init_attr.rq_cq_hndl         = cq_hndl;
        qp_init_attr.sq_cq_hndl         = cq_hndl;
         
            break;
        case VAPI_TS_UD: /* Set up UD qp parameters */
        default:
            return OMPI_ERR_NOT_IMPLEMENTED;
    }

#ifdef VAPI_FEATURE_SRQ    
    if(mca_btl_mvapi_component.use_srq) { 
        qp_init_attr_ext.srq_hndl = srq_hndl; 
        
        ret = VAPI_create_qp_ext(nic, 
                                 &qp_init_attr, 
                                 &qp_init_attr_ext,  
                                 qp_hndl, 
                                 qp_prop);
    } else 
#endif
    { 
        ret = VAPI_create_qp(nic, 
                             &qp_init_attr, 
                             qp_hndl, 
                             qp_prop); 
    }
    if(VAPI_OK != ret) {
        BTL_ERROR(("error creating the queue pair: %s", VAPI_strerror(ret))); 
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

/* 
 * The queue pair has been created and we have received the remote 
 *  queue pair information from the peer so we init this queue pair 
 *  and are ready to roll. 
 */ 
int mca_btl_mvapi_endpoint_qp_init_query(

                                      mca_btl_mvapi_module_t* mvapi_btl, 
                                      VAPI_hca_hndl_t nic, 
                                      VAPI_qp_hndl_t qp_hndl, 
                                      VAPI_qp_num_t remote_qp_num, 
                                      IB_lid_t remote_lid, 
                                      IB_port_t port_id
                                      )
     
     
{
    
    VAPI_ret_t              ret;
    VAPI_qp_attr_t          qp_attr;

    VAPI_qp_attr_mask_t     qp_attr_mask;
    VAPI_qp_init_attr_t     qp_init_attr; 
    VAPI_qp_cap_t           qp_cap;

    /* Modifying  QP to INIT */
    QP_ATTR_MASK_CLR_ALL(qp_attr_mask);
    qp_attr.qp_state = VAPI_INIT;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_QP_STATE);
    qp_attr.pkey_ix = mca_btl_mvapi_component.ib_pkey_ix;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PKEY_IX);
    qp_attr.port = port_id; 
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PORT);
    qp_attr.remote_atomic_flags = VAPI_EN_REM_WRITE | VAPI_EN_REM_READ;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_REMOTE_ATOMIC_FLAGS);

    ret = VAPI_modify_qp(nic, qp_hndl,
            &qp_attr, &qp_attr_mask, &qp_cap);

    if(VAPI_OK != ret) {
        BTL_ERROR(("Error modifying the queue pair: %s", VAPI_strerror(ret)));
        return OMPI_ERROR;
    }

    BTL_VERBOSE(("Modified to init..Qp %d", qp_hndl));

    /**********************  INIT --> RTR  ************************/
    QP_ATTR_MASK_CLR_ALL(qp_attr_mask);
    qp_attr.qp_state = VAPI_RTR;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_QP_STATE);
    qp_attr.qp_ous_rd_atom = mca_btl_mvapi_component.ib_qp_ous_rd_atom;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_QP_OUS_RD_ATOM);
    qp_attr.path_mtu = mca_btl_mvapi_component.ib_mtu;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PATH_MTU);
    qp_attr.rq_psn = mca_btl_mvapi_component.ib_psn;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_RQ_PSN);
    qp_attr.pkey_ix = mca_btl_mvapi_component.ib_pkey_ix;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PKEY_IX);
    qp_attr.min_rnr_timer = mca_btl_mvapi_component.ib_min_rnr_timer;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_MIN_RNR_TIMER);

    qp_attr.av.sl = mca_btl_mvapi_component.ib_service_level;
    qp_attr.av.grh_flag = FALSE;
    qp_attr.av.static_rate = mca_btl_mvapi_component.ib_static_rate;
    qp_attr.av.src_path_bits = mca_btl_mvapi_component.ib_src_path_bits;

    qp_attr.dest_qp_num = remote_qp_num;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_DEST_QP_NUM);
    qp_attr.av.dlid = remote_lid;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_AV);

    ret = VAPI_modify_qp(nic, qp_hndl,
            &qp_attr, &qp_attr_mask, &qp_cap);

    if(VAPI_OK != ret) {
        BTL_ERROR(("Error modifying the queue pair: %s", VAPI_strerror(ret)));
        return OMPI_ERROR;
    }
    
    BTL_VERBOSE(("Modified to RTR..Qp %d", qp_hndl));

    /************** RTS *******************/
    QP_ATTR_MASK_CLR_ALL(qp_attr_mask);
    qp_attr.qp_state = VAPI_RTS;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_QP_STATE);
    qp_attr.sq_psn = mca_btl_mvapi_component.ib_psn;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_SQ_PSN);
    qp_attr.timeout = mca_btl_mvapi_component.ib_timeout;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_TIMEOUT);
    qp_attr.retry_count = mca_btl_mvapi_component.ib_retry_count;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_RETRY_COUNT);
    qp_attr.rnr_retry = mca_btl_mvapi_component.ib_rnr_retry;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_RNR_RETRY);
    qp_attr.ous_dst_rd_atom = mca_btl_mvapi_component.ib_max_rdma_dst_ops;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_OUS_DST_RD_ATOM);

    ret = VAPI_modify_qp(nic, qp_hndl,
            &qp_attr, &qp_attr_mask, &qp_cap);

    if(VAPI_OK != ret) {
        return OMPI_ERROR;
    }
    BTL_VERBOSE(("Modified to RTS..Qp %d", qp_hndl));
    
    ret = VAPI_query_qp(nic, qp_hndl, &qp_attr, &qp_attr_mask, &qp_init_attr );          
    if (ret != VAPI_OK) {                                                   
        BTL_ERROR(("Error modifying the queue pair: %s", VAPI_strerror(ret)));
        return OMPI_ERROR; 
    }                      
    
    mvapi_btl->ib_inline_max = qp_init_attr.cap.max_inline_data_sq;  
    
    return OMPI_SUCCESS;
}

                                                                                                                       
/**
 * Return control fragment.
 */

static void mca_btl_mvapi_endpoint_credits_lp(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    int32_t credits;

    /* we don't acquire a wqe or token for credit message - so decrement */
    OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,-1);

    /* check to see if there are addditional credits to return */
    if ((credits = OPAL_THREAD_ADD32(&endpoint->sd_credits_lp,-1)) > 0) {
        OPAL_THREAD_ADD32(&endpoint->sd_credits_lp,-credits);
        if (endpoint->rd_credits_lp >= mca_btl_mvapi_component.rd_win &&
            OPAL_THREAD_ADD32(&endpoint->sd_credits_lp,1) == 1) {
            mca_btl_mvapi_endpoint_send_credits_lp(endpoint);
        }
    }
    MCA_BTL_IB_FRAG_RETURN(((mca_btl_mvapi_module_t*)btl), 
                           ((mca_btl_mvapi_frag_t*)descriptor));
}

/**
 * Return credits to peer
 */

void mca_btl_mvapi_endpoint_send_credits_lp(
    mca_btl_mvapi_endpoint_t* endpoint)
{
    mca_btl_mvapi_module_t* mvapi_btl = endpoint->endpoint_btl;
    mca_btl_mvapi_frag_t* frag;
    int ret;

    MCA_BTL_IB_FRAG_ALLOC_EAGER(mvapi_btl, frag, ret);
    if(NULL == frag) {
        BTL_ERROR(("error allocating fragment"));
        return;
    }

    frag->base.des_cbfunc = mca_btl_mvapi_endpoint_credits_lp;
    frag->base.des_cbdata = NULL;
    frag->endpoint = endpoint;

    frag->hdr->tag = MCA_BTL_TAG_BTL;
    frag->hdr->credits = endpoint->rd_credits_lp;
    OPAL_THREAD_ADD32(&endpoint->rd_credits_lp, -frag->hdr->credits);
    ((mca_btl_mvapi_control_header_t *)frag->segment.seg_addr.pval)->type = MCA_BTL_MVAPI_CONTROL_NOOP;

    frag->desc.sr_desc.opcode = VAPI_SEND; 
    frag->sg_entry.addr = (VAPI_virt_addr_t) (MT_virt_addr_t) frag->hdr; 
    frag->sg_entry.len = sizeof(mca_btl_mvapi_header_t) +
        sizeof(mca_btl_mvapi_control_header_t);

    if(sizeof(mca_btl_mvapi_header_t) <= mvapi_btl->ib_inline_max) {
        ret = EVAPI_post_inline_sr(mvapi_btl->nic, endpoint->lcl_qp_hndl_lp, &frag->desc.sr_desc);
    } else {
        ret = VAPI_post_sr(mvapi_btl->nic, endpoint->lcl_qp_hndl_lp, &frag->desc.sr_desc);
    }
    if(ret != VAPI_SUCCESS) {
        OPAL_THREAD_ADD32(&endpoint->sd_credits_lp, -1);
        OPAL_THREAD_ADD32(&endpoint->rd_credits_lp, frag->hdr->credits);
        MCA_BTL_IB_FRAG_RETURN(mvapi_btl, frag);
        BTL_ERROR(("error posting send request errno %d says %s", strerror(errno)));
        return;
    }
}

/**
 * Return control fragment.
 */

static void mca_btl_mvapi_endpoint_credits_hp(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    int32_t credits;

    /* we don't acquire a wqe or token for credit message - so decrement */
    OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp,-1);

    /* check to see if there are addditional credits to return */
    if ((credits = OPAL_THREAD_ADD32(&endpoint->sd_credits_hp,-1)) > 0) {
        OPAL_THREAD_ADD32(&endpoint->sd_credits_hp,-credits);
        if ((endpoint->rd_credits_hp >= mca_btl_mvapi_component.rd_win ||
                    endpoint->eager_rdma_local.credits >=
                    mca_btl_mvapi_component.rd_win) &&
            OPAL_THREAD_ADD32(&endpoint->sd_credits_hp,1) == 1) {
            mca_btl_mvapi_endpoint_send_credits_hp(endpoint);
        }
    }
    MCA_BTL_IB_FRAG_RETURN(((mca_btl_mvapi_module_t*)btl), 
                           ((mca_btl_mvapi_frag_t*)descriptor));
}

/**
 * Return credits to peer
 */

void mca_btl_mvapi_endpoint_send_credits_hp(
    mca_btl_mvapi_endpoint_t* endpoint)
{
    mca_btl_mvapi_module_t* mvapi_btl = endpoint->endpoint_btl;
    mca_btl_mvapi_frag_t* frag;
    int ret;

    MCA_BTL_IB_FRAG_ALLOC_EAGER(mvapi_btl, frag, ret);
    if(NULL == frag) {
        BTL_ERROR(("error allocating fragment"));
        return;
    }

    frag->base.des_cbfunc = mca_btl_mvapi_endpoint_credits_hp;
    frag->base.des_cbdata = NULL;
    frag->endpoint = endpoint;

    frag->hdr->tag = MCA_BTL_TAG_BTL;
    frag->hdr->credits =
        (endpoint->rd_credits_hp > 0) ? endpoint->rd_credits_hp: 0;
    OPAL_THREAD_ADD32(&endpoint->rd_credits_hp, -frag->hdr->credits);
    frag->hdr->rdma_credits = endpoint->eager_rdma_local.credits;
    OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits,
            -frag->hdr->rdma_credits);
    ((mca_btl_mvapi_control_header_t *)frag->segment.seg_addr.pval)->type = MCA_BTL_MVAPI_CONTROL_NOOP;


    frag->desc.sr_desc.opcode = VAPI_SEND; 
    frag->sg_entry.addr = (VAPI_virt_addr_t) (MT_virt_addr_t) frag->hdr; 
    frag->sg_entry.len = sizeof(mca_btl_mvapi_header_t) +
        sizeof(mca_btl_mvapi_control_header_t);

    if(sizeof(mca_btl_mvapi_header_t) <= mvapi_btl->ib_inline_max) {
        ret = EVAPI_post_inline_sr(mvapi_btl->nic, endpoint->lcl_qp_hndl_hp, &frag->desc.sr_desc);
    } else {
        ret = VAPI_post_sr(mvapi_btl->nic, endpoint->lcl_qp_hndl_hp, &frag->desc.sr_desc);
    }
    if(ret != VAPI_SUCCESS) {
        OPAL_THREAD_ADD32(&endpoint->sd_credits_lp, -1);
        OPAL_THREAD_ADD32(&endpoint->rd_credits_lp, frag->hdr->credits);
        MCA_BTL_IB_FRAG_RETURN(mvapi_btl, frag);
        BTL_ERROR(("error posting send request errno %d says %s", strerror(errno)));
        return;
    }
}

static void mca_btl_mvapi_endpoint_eager_rdma(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    MCA_BTL_IB_FRAG_RETURN(((mca_btl_mvapi_module_t*)btl),
                           ((mca_btl_mvapi_frag_t*)descriptor));
}

static int mca_btl_mvapi_endpoint_send_eager_rdma(
    mca_btl_base_endpoint_t* endpoint)
{
    mca_btl_mvapi_module_t* mvapi_btl = endpoint->endpoint_btl;
    mca_btl_mvapi_eager_rdma_header_t *rdma_hdr;
    mca_btl_mvapi_frag_t* frag;
    int rc;

    MCA_BTL_IB_FRAG_ALLOC_EAGER(mvapi_btl, frag, rc);
    if(NULL == frag) {
        BTL_ERROR(("error allocating fragment"));
        return -1;
    }

    frag->base.des_cbfunc = mca_btl_mvapi_endpoint_eager_rdma;
    frag->base.des_cbdata = NULL;
    frag->endpoint = endpoint;
    frag->base.des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;

    frag->hdr->tag = MCA_BTL_TAG_BTL;
    rdma_hdr = (mca_btl_mvapi_eager_rdma_header_t*)frag->segment.seg_addr.pval;
    rdma_hdr->control.type = MCA_BTL_MVAPI_CONTROL_RDMA;
    rdma_hdr->rkey = endpoint->eager_rdma_local.reg->r_key;
    rdma_hdr->rdma_start.lval = ompi_ptr_ptol(endpoint->eager_rdma_local.base.pval);
    frag->segment.seg_len = sizeof(mca_btl_mvapi_eager_rdma_header_t);
    if (mca_btl_mvapi_endpoint_post_send(mvapi_btl, endpoint, frag) !=
            OMPI_SUCCESS) {
        MCA_BTL_IB_FRAG_RETURN(mvapi_btl, frag);
        BTL_ERROR(("Error sending RDMA buffer", strerror(errno)));
        return -1;
    }
    return 0;
}
/* create RDMA buffer for eager messages */
void mca_btl_mvapi_endpoint_connect_eager_rdma(
        mca_btl_mvapi_endpoint_t* endpoint)
{
    mca_btl_mvapi_module_t* mvapi_btl = endpoint->endpoint_btl;
    char *buf;
    unsigned int i;

    OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
    if (endpoint->eager_rdma_local.base.pval)
        goto unlock_rdma_local;

    buf = mvapi_btl->super.btl_mpool->mpool_alloc(mvapi_btl->super.btl_mpool,
            mvapi_btl->eager_rdma_frag_size * 
            mca_btl_mvapi_component.eager_rdma_num, 0,
            MCA_MPOOL_FLAGS_CACHE_BYPASS,
            (mca_mpool_base_registration_t**)&endpoint->eager_rdma_local.reg);

    if(!buf)
       goto unlock_rdma_local;

    for(i = 0; i < mca_btl_mvapi_component.eager_rdma_num; i++) {
        ompi_free_list_item_t *item = (ompi_free_list_item_t *)(buf +
                i*mvapi_btl->eager_rdma_frag_size);
        item->user_data = (void*)endpoint->eager_rdma_local.reg;
        OBJ_CONSTRUCT(item, mca_btl_mvapi_recv_frag_eager_t);
        ((mca_btl_mvapi_frag_t*)item)->endpoint = endpoint;
        ((mca_btl_mvapi_frag_t*)item)->type = MCA_BTL_MVAPI_FRAG_EAGER_RDMA;
    }

    OPAL_THREAD_LOCK(&mvapi_btl->eager_rdma_lock);
    if(orte_pointer_array_add (&endpoint->eager_rdma_index,
                mvapi_btl->eager_rdma_buffers, endpoint) < 0)
      goto cleanup;

    endpoint->eager_rdma_local.base.pval = buf;
    mvapi_btl->eager_rdma_buffers_count++;
    if (mca_btl_mvapi_endpoint_send_eager_rdma(endpoint) == 0) {
        OPAL_THREAD_UNLOCK(&mvapi_btl->eager_rdma_lock);
        OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
        return;
    }

    mvapi_btl->eager_rdma_buffers_count--;
    endpoint->eager_rdma_local.base.pval = NULL;
    orte_pointer_array_set_item(mvapi_btl->eager_rdma_buffers,
            endpoint->eager_rdma_index, NULL);

cleanup:
    OPAL_THREAD_UNLOCK(&mvapi_btl->eager_rdma_lock);
    mvapi_btl->super.btl_mpool->mpool_free(mvapi_btl->super.btl_mpool,
            buf, (mca_mpool_base_registration_t*)endpoint->eager_rdma_local.reg);
unlock_rdma_local:
    OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
}
