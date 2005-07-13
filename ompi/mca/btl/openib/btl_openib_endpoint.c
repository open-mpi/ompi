/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "include/types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ns/base/base.h"
#include "mca/oob/base/base.h"
#include "mca/rml/rml.h"
#include "mca/errmgr/errmgr.h"
#include "dps/dps.h"
#include "btl_openib.h"
#include "btl_openib_endpoint.h" 
#include "btl_openib_proc.h"
#include "btl_openib_frag.h"
#include "class/ompi_free_list.h" 

static void mca_btl_openib_endpoint_construct(mca_btl_base_endpoint_t* endpoint);
static void mca_btl_openib_endpoint_destruct(mca_btl_base_endpoint_t* endpoint);

int mca_btl_openib_endpoint_create_qp(
                                      mca_btl_openib_module_t* openib_btl, 
                                      struct ibv_pd* pd, 
                                      struct ibv_cq* cq, 
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
                   
static inline int mca_btl_openib_endpoint_post_send(mca_btl_openib_module_t* openib_btl, 
                                                    mca_btl_openib_endpoint_t * endpoint, 
                                                    mca_btl_openib_frag_t * frag)
{ 
    
    struct ibv_qp* ib_qp; 
    struct ibv_send_wr* bad_wr; 
    frag->sg_entry.addr = (uintptr_t) frag->hdr; 
    
    if(frag->base.des_flags && MCA_BTL_DES_FLAGS_PRIORITY  && frag->size <= openib_btl->super.btl_eager_limit){ 
        ib_qp = endpoint->lcl_qp_high; 
    } else {
        ib_qp = endpoint->lcl_qp_low; 
    } 
    
    frag->sr_desc.opcode = IBV_WR_SEND; 
    frag->sr_desc.send_flags = IBV_SEND_SIGNALED; 

    frag->sg_entry.length = frag->segment.seg_len + ((unsigned char*) frag->segment.seg_addr.pval - (unsigned char*) frag->hdr);  /* sizeof(mca_btl_openib_header_t); */ 

    /* TODO: should check if we can inline send,, but can't find 
     * inline send defined in openib verbs api. 
     * if(frag->sg_entry.len <= openib_btl->ib_inline_max) { 
     */  
    if(ibv_post_send(ib_qp, 
                     &frag->sr_desc, 
                     &bad_wr)) { 
        opal_output(0, "%s: error posting send request\n", __func__); 
        return OMPI_ERROR; 
    }
    mca_btl_openib_endpoint_post_rr(endpoint, 1); 

    return OMPI_ERROR; 
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
    OBJ_CONSTRUCT(&endpoint->endpoint_send_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_recv_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&endpoint->pending_send_frags, opal_list_t);
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


static int mca_btl_openib_endpoint_send_connect_req(mca_btl_base_endpoint_t* endpoint)
{
    orte_buffer_t* buffer = OBJ_NEW(orte_buffer_t);
    int rc;
    if(NULL == buffer) {
         ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
         return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* pack the info in the send buffer */

    rc = orte_dps.pack(buffer, &endpoint->lcl_qp_high->qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    rc = orte_dps.pack(buffer, &endpoint->lcl_qp_low->qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = orte_dps.pack(buffer, &endpoint->lcl_psn_high, 1, ORTE_UINT32); 
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
     
    rc = orte_dps.pack(buffer, &endpoint->lcl_psn_low, 1, ORTE_UINT32); 
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = orte_dps.pack(buffer, &endpoint->endpoint_btl->ib_port_attr->lid, 1, ORTE_UINT16);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    
    
    /* send to endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, buffer, ORTE_RML_TAG_DYNAMIC-1, 0,
         mca_btl_openib_endpoint_send_cb, NULL);
    
    
    DEBUG_OUT("Sending High Priority QP num = %d, Low Priority QP num = %d, LID = %d",
              endpoint->lcl_qp_prop_high.qp_num,
              endpoint->lcl_qp_prop_low.qp_num,
              endpoint->endpoint_btl->port.lid);
    
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 * Send connect ACK to remote endpoint
 *
 */

static int mca_btl_openib_endpoint_send_connect_ack(mca_btl_base_endpoint_t* endpoint)
{
    orte_buffer_t* buffer = OBJ_NEW(orte_buffer_t);
    int rc;
    uint32_t zero = 0;

    /* pack the info in the send buffer */
    if(ORTE_SUCCESS != (rc = orte_dps.pack(buffer, &zero, 1, ORTE_UINT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(ORTE_SUCCESS != (rc = orte_dps.pack(buffer, &zero, 1, ORTE_UINT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(ORTE_SUCCESS != (rc = orte_dps.pack(buffer, &zero, 1, ORTE_UINT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(ORTE_SUCCESS != (rc = orte_dps.pack(buffer, &zero, 1, ORTE_UINT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(ORTE_SUCCESS != (rc = orte_dps.pack(buffer, &zero, 1, ORTE_UINT16))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* send to endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, buffer, ORTE_RML_TAG_DYNAMIC-1, 0,
         mca_btl_openib_endpoint_send_cb, NULL);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

/*
 * Set remote connection info
 *
 * XXX: Currently size is unutilized, this shall change
 * as soon as we add more info to be exchanged at connection
 * setup.
 *
 */
static int mca_btl_openib_endpoint_set_remote_info(mca_btl_base_endpoint_t* endpoint, orte_buffer_t* buffer)
{
    int rc;



    size_t cnt = 1;
    rc = orte_dps.unpack(buffer, &endpoint->rem_qp_num_high, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dps.unpack(buffer, &endpoint->rem_qp_num_low, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dps.unpack(buffer, &endpoint->rem_psn_high, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dps.unpack(buffer, &endpoint->rem_psn_low, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dps.unpack(buffer, &endpoint->rem_lid, &cnt, ORTE_UINT16);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    DEBUG_OUT("Received High Priority QP num = %d, Low Priority QP num %d,  LID = %d",
              endpoint->rem_qp_num_high,
              endpoint->rem_qp_num_low, 
              endpoint->rem_lid);

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
                                                               openib_btl->ib_pd, 
                                                               openib_btl->ib_cq_high, 
                                                               endpoint->lcl_qp_attr_high, 
                                                               &endpoint->lcl_qp_high))) { 
        opal_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                    ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }
    endpoint->lcl_psn_high = lrand48() & 0xffffff; 
    
    /* Create the Low Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_create_qp(openib_btl, 
                                                               openib_btl->ib_pd, 
                                                               openib_btl->ib_cq_low, 
                                                               endpoint->lcl_qp_attr_low, 
                                                               &endpoint->lcl_qp_low))) { 
        opal_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                    ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }
    endpoint->lcl_psn_low = lrand48() & 0xffffff; 

    DEBUG_OUT("Initialized High Priority QP num = %d, Low Priority QP num = %d,  LID = %d",
              endpoint->lcl_qp_high->qp_num,
              endpoint->lcl_qp_low.qp_num, 
              openib_btl->ib_port_attr->lid); 

    /* Send connection info over to remote endpoint */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_send_connect_req(endpoint))) {
        opal_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 * Reply to a `start - connect' message
 *
 */
static int mca_btl_openib_endpoint_reply_start_connect(mca_btl_openib_endpoint_t *endpoint, orte_buffer_t* buffer)
{
    int rc;
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) endpoint->endpoint_btl; 
        
        
    /* Create the High Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_create_qp(openib_btl, 
                                                               openib_btl->ib_pd, 
                                                               openib_btl->ib_cq_high,  
                                                               endpoint->lcl_qp_attr_high, 
                                                               &endpoint->lcl_qp_high))) { 
        opal_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                    ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }
    endpoint->lcl_psn_high = lrand48() & 0xffffff;
    
    /* Create the Low Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_create_qp(openib_btl, 
                                                               openib_btl->ib_pd, 
                                                               openib_btl->ib_cq_low, 
                                                               endpoint->lcl_qp_attr_low, 
                                                               &endpoint->lcl_qp_low))) { 
        opal_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                    ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }
    endpoint->lcl_psn_low = lrand48() & 0xffffff; 

    DEBUG_OUT("Initialized High Priority QP num = %d, Low Priority QP num = %d,  LID = %d",
              endpoint->lcl_qp_high->qp_num,
              endpoint->lcl_qp_low.qp_num, 
              openib_btl->ib_port_attr->lid); 


    /* Set the remote side info */
    mca_btl_openib_endpoint_set_remote_info(endpoint, buffer);
    
    /* Connect to endpoint */

    rc = mca_btl_openib_endpoint_connect(endpoint);
    if(rc != OMPI_SUCCESS) {
        opal_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }

    /* Send connection info over to remote endpoint */
    if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_send_connect_req(endpoint))) {
        opal_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 *
 */

static void mca_btl_openib_endpoint_connected(mca_btl_openib_endpoint_t *endpoint)
{
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTED;
    mca_btl_openib_progress_send_frags(endpoint);
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

    for(ib_proc = (mca_btl_openib_proc_t*)
            opal_list_get_first(&mca_btl_openib_component.ib_procs);
            ib_proc != (mca_btl_openib_proc_t*)
            opal_list_get_end(&mca_btl_openib_component.ib_procs);
            ib_proc  = (mca_btl_openib_proc_t*)opal_list_get_next(ib_proc)) {

        if(ib_proc->proc_guid.vpid == endpoint->vpid) {

            /* Try to get the endpoint instance of this proc */

            /* Limitation: Right now, we have only 1 endpoint
             * for every process. Need several changes, some
             * in PML/BTL interface to set this right */
            ib_endpoint = ib_proc->proc_endpoints[0];

            endpoint_state = ib_endpoint->endpoint_state;

            /* Update status */
            switch(endpoint_state) {
            case MCA_BTL_IB_CLOSED :
                /* We had this connection closed before.
                 * The endpoint is trying to connect. Move the
                 * status of this connection to CONNECTING,
                 * and then reply with our QP information */
                
                if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_reply_start_connect(ib_endpoint, buffer))) {
                    opal_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                                ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
                    break;
                }
                
                /* Setup state as connected */
                ib_endpoint->endpoint_state = MCA_BTL_IB_CONNECT_ACK;
                break;
                
            case MCA_BTL_IB_CONNECTING :

                mca_btl_openib_endpoint_set_remote_info(ib_endpoint, buffer);
                if(OMPI_SUCCESS != (rc = mca_btl_openib_endpoint_connect(ib_endpoint))) {
                    opal_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                                ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
                    break;
                }
                    
                /* Setup state as connected */
                mca_btl_openib_endpoint_connected(ib_endpoint);

                /* Send him an ack */
                mca_btl_openib_endpoint_send_connect_ack(ib_endpoint);
                break;

            case MCA_BTL_IB_CONNECT_ACK:

                mca_btl_openib_endpoint_connected(ib_endpoint);

                break;

            case MCA_BTL_IB_CONNECTED :
                break;
            default :
                opal_output(0, "Connected -> Connecting not possible.\n");
            }

            break;
        }
    }

    /* Okay, now that we are done receiving,
     * re-post the buffer */
    mca_btl_openib_post_recv();
}

void mca_btl_openib_post_recv()
{
    
    orte_rml.recv_buffer_nb(
        ORTE_RML_NAME_ANY, 
        ORTE_RML_TAG_DYNAMIC-1, 
        0,
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
    mca_btl_openib_module_t *openib_btl; 
    
    OPAL_THREAD_LOCK(&endpoint->endpoint_send_lock);
    
    switch(endpoint->endpoint_state) {
        case MCA_BTL_IB_CONNECTING:

            DEBUG_OUT("Queing because state is connecting");
            
            opal_list_append(&endpoint->pending_send_frags,
                    (opal_list_item_t *)frag);

            rc = OMPI_SUCCESS;
            break;

        case MCA_BTL_IB_CONNECT_ACK:

            DEBUG_OUT("Queuing because waiting for ack");

            opal_list_append(&endpoint->pending_send_frags,
                    (opal_list_item_t *)frag);

            rc = OMPI_SUCCESS;
            break;

        case MCA_BTL_IB_CLOSED:

            DEBUG_OUT("Connection to endpoint closed ... connecting ...");

            opal_list_append(&endpoint->pending_send_frags,
                    (opal_list_item_t *)frag);

            rc = mca_btl_openib_endpoint_start_connect(endpoint);
            
            break;

        case MCA_BTL_IB_FAILED:

            rc = OMPI_ERR_UNREACH;
            break;

        case MCA_BTL_IB_CONNECTED:
            {
                openib_btl = endpoint->endpoint_btl;
                
                
                DEBUG_OUT("Send to : %d, len : %d, frag : %p", 
                        endpoint->endpoint_proc->proc_guid.vpid,
                        frag->ib_buf.desc.sg_entry.len,
                        frag);
                
                rc = mca_btl_openib_endpoint_post_send(openib_btl, endpoint, frag); 
                
                break; 
            }

    default:
        rc = OMPI_ERR_UNREACH;
    }
    
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_send_lock);
    
    return rc;
}

void mca_btl_openib_progress_send_frags(mca_btl_openib_endpoint_t* endpoint)
{
    opal_list_item_t *frag_item;
    mca_btl_openib_frag_t *frag;
    mca_btl_openib_module_t* openib_btl; 
    /*Check if endpoint is connected */
    if(endpoint->endpoint_state != MCA_BTL_IB_CONNECTED) {

        return;
    }

    /* While there are frags in the list,
     * process them */

    while(!opal_list_is_empty(&(endpoint->pending_send_frags))) {
        frag_item = opal_list_remove_first(&(endpoint->pending_send_frags));
        frag = (mca_btl_openib_frag_t *) frag_item;
        openib_btl = endpoint->endpoint_btl;
        /* We need to post this one */
        
        if(OMPI_SUCCESS !=  mca_btl_openib_endpoint_post_send(openib_btl, endpoint, frag))
            opal_output(0, "error in mca_btl_openib_endpoint_send");
    }
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
                                               endpoint->lcl_qp_high, 
                                               endpoint->lcl_qp_attr_high, 
                                               endpoint->lcl_psn_high, 
                                               endpoint->rem_qp_num_high, 
                                               endpoint->rem_psn_high, 
                                               endpoint->rem_lid, 
                                               openib_btl->port_num
                                               ); 
    
    
    
    if(rc != OMPI_SUCCESS) {
        return rc;
    }
    rc = mca_btl_openib_endpoint_qp_init_query(
                                               openib_btl, 
                                               endpoint->lcl_qp_low, 
                                               endpoint->lcl_qp_attr_low, 
                                               endpoint->lcl_psn_low, 
                                               endpoint->rem_qp_num_low, 
                                               endpoint->rem_psn_low, 
                                               endpoint->rem_lid, 
                                               openib_btl->port_num
                                               ); 
    
    
    
    if(rc != OMPI_SUCCESS) {
        return rc;
    }
    
    mca_btl_openib_endpoint_post_rr(endpoint, 0); 
    
    return OMPI_SUCCESS;
}



int mca_btl_openib_endpoint_create_qp(
                                      mca_btl_openib_module_t* openib_btl, 
                                      struct ibv_pd* pd, 
                                      struct ibv_cq* cq, 
                                      struct ibv_qp_attr* qp_attr,
                                      struct ibv_qp** qp
                                      )
{
    {
        struct ibv_qp_init_attr qp_init_attr; 
        qp_init_attr.send_cq = cq; 
        qp_init_attr.recv_cq = cq; 
        qp_init_attr.cap.max_send_wr = openib_btl->ib_wq_size;
        qp_init_attr.cap.max_recv_wr = openib_btl->ib_wq_size; 
        qp_init_attr.cap.max_send_sge = openib_btl->ib_sg_list_size;
        qp_init_attr.cap.max_recv_sge = openib_btl->ib_sg_list_size;
        qp_init_attr.qp_type = IBV_QPT_RC; 
        
    
        (*qp) = ibv_create_qp(pd, &qp_init_attr); 
    
        if(NULL == (*qp)) { 
            opal_output(0, "%s: error creating qp \n", __func__); 
            return OMPI_ERROR; 
        }
            
        openib_btl->ib_inline_max = qp_init_attr.cap.max_inline_data;  
    
    }
    
    {
        qp_attr->qp_state = IBV_QPS_INIT; 
        qp_attr->pkey_index = openib_btl->ib_pkey_ix; 
        qp_attr->port_num = openib_btl->port_num; 
        qp_attr->qp_access_flags = 0; 
        
        if(ibv_modify_qp((*qp), qp_attr, 
                         IBV_QP_STATE | 
                         IBV_QP_PKEY_INDEX | 
                         IBV_QP_PORT | 
                         IBV_QP_ACCESS_FLAGS )) { 
            opal_output(0, "%s: error modifying qp to INIT\n"); 
            return OMPI_ERROR; 
        } 
    } 

    return OMPI_SUCCESS;
}

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
    attr->path_mtu = openib_btl->ib_mtu; 
    attr->dest_qp_num = rem_qp_num; 
    attr->rq_psn = rem_psn; 
    attr->max_dest_rd_atomic = openib_btl->ib_max_rdma_dst_ops; 
    attr->min_rnr_timer = openib_btl->ib_min_rnr_timer; 
    attr->ah_attr.is_global = 0; 
    attr->ah_attr.dlid = rem_lid; 
    attr->ah_attr.sl = openib_btl->ib_service_level; 
    attr->ah_attr.src_path_bits = openib_btl->ib_src_path_bits; 
    attr->ah_attr.port_num = port_num; 
    
    if(ibv_modify_qp(qp, attr, 
                     IBV_QP_STATE              |
                     IBV_QP_AV                 |
                     IBV_QP_PATH_MTU           |
                     IBV_QP_DEST_QPN           |
                     IBV_QP_RQ_PSN             |
                     IBV_QP_MAX_DEST_RD_ATOMIC |
                     IBV_QP_MIN_RNR_TIMER)) {
        opal_output(0, "%s: error modifing QP to RTR\n", __func__); 
        return OMPI_ERROR; 
    }
    attr->qp_state 	    = IBV_QPS_RTS;
    attr->timeout 	    = openib_btl->ib_timeout;
    attr->retry_cnt 	    = openib_btl->ib_retry_count;
    attr->rnr_retry 	    = openib_btl->ib_rnr_retry;
    attr->sq_psn 	    = lcl_psn;
    attr->max_rd_atomic  = openib_btl->ib_max_rdma_dst_ops;
    if (ibv_modify_qp(qp, attr,
                      IBV_QP_STATE              |
                      IBV_QP_TIMEOUT            |
                      IBV_QP_RETRY_CNT          |
                      IBV_QP_RNR_RETRY          |
                      IBV_QP_SQ_PSN             |
                      IBV_QP_MAX_QP_RD_ATOMIC)) {
        opal_output(0, "%s: error modifying QP to RTS\n", __func__); 
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

