/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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
#include "bmi_ib.h"
#include "bmi_ib_endpoint.h" 
#include "bmi_ib_addr.h"
#include "bmi_ib_proc.h"
#include "bmi_ib_priv.h"
#include "bmi_ib_frag.h"
#include "class/ompi_free_list.h" 

static void mca_bmi_ib_endpoint_construct(mca_bmi_base_endpoint_t* endpoint);
static void mca_bmi_ib_endpoint_destruct(mca_bmi_base_endpoint_t* endpoint);

                   
static inline int mca_bmi_ib_endpoint_post_send(mca_bmi_ib_module_t* ib_bmi, mca_bmi_ib_endpoint_t * endpoint, mca_bmi_ib_frag_t * frag)
{ 
    
    frag->sr_desc.remote_qkey = 0; 
    frag->sr_desc.remote_qp = endpoint->rem_qp_num; 
    
    frag->sg_entry.len = frag->segment.seg_len + sizeof(mca_bmi_ib_header_t); 
    if(frag->sg_entry.len <= ib_bmi->ib_inline_max) { 
            frag->ret = EVAPI_post_inline_sr(ib_bmi->nic, 
                                 endpoint->lcl_qp_hndl, 
                                 &frag->sr_desc); 
        
    }else { 
        frag->ret = VAPI_post_sr(ib_bmi->nic, 
                                 endpoint->lcl_qp_hndl, 
                                 &frag->sr_desc); 
    }

    if(VAPI_OK != frag->ret)
        return OMPI_ERROR; 
    return OMPI_SUCCESS; 
}


 
OBJ_CLASS_INSTANCE(mca_bmi_ib_endpoint_t, 
        ompi_list_item_t, mca_bmi_ib_endpoint_construct, 
        mca_bmi_ib_endpoint_destruct);

/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_bmi_ib_endpoint_construct(mca_bmi_base_endpoint_t* endpoint)
{
    endpoint->endpoint_bmi = 0;
    endpoint->endpoint_proc = 0;
    endpoint->endpoint_tstamp = 0.0;
    endpoint->endpoint_state = MCA_BMI_IB_CLOSED;
    endpoint->endpoint_retries = 0;
    OBJ_CONSTRUCT(&endpoint->endpoint_send_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_recv_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&endpoint->pending_send_frags, ompi_list_t);
}

/*
 * Destroy a endpoint
 *
 */

static void mca_bmi_ib_endpoint_destruct(mca_bmi_base_endpoint_t* endpoint)
{
}

/*
 * Send connection information to remote endpoint using OOB
 *
 */

static void mca_bmi_ib_endpoint_send_cb(
    int status,
    orte_process_name_t* endpoint, 
    orte_buffer_t* buffer,
    orte_rml_tag_t tag, 
    void* cbdata)
{
    OBJ_RELEASE(buffer);
}


static int mca_bmi_ib_endpoint_send_connect_req(mca_bmi_base_endpoint_t* endpoint)
{
    orte_buffer_t* buffer = OBJ_NEW(orte_buffer_t);
    int rc;
    if(NULL == buffer) {
         ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
         return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* pack the info in the send buffer */

    rc = orte_dps.pack(buffer, &endpoint->lcl_qp_prop.qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dps.pack(buffer, &endpoint->endpoint_bmi->port.lid, 1, ORTE_UINT32);

    /* send to endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, buffer, ORTE_RML_TAG_DYNAMIC-1, 0,
         mca_bmi_ib_endpoint_send_cb, NULL);
    
    
    D_PRINT("Sending QP num = %d, LID = %d",
            endpoint->lcl_qp_prop.qp_num,
            endpoint->endpoint_bmi->port.lid);

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

static int mca_bmi_ib_endpoint_send_connect_ack(mca_bmi_base_endpoint_t* endpoint)
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

    /* send to endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, buffer, ORTE_RML_TAG_DYNAMIC-1, 0,
         mca_bmi_ib_endpoint_send_cb, NULL);
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
static int mca_bmi_ib_endpoint_set_remote_info(mca_bmi_base_endpoint_t* endpoint, orte_buffer_t* buffer)
{
    int rc;



    size_t cnt = 1;
    rc = orte_dps.unpack(buffer, &endpoint->rem_qp_num, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    rc = orte_dps.unpack(buffer, &endpoint->rem_lid, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    D_PRINT("Received QP num = %d, LID = %d",
            endpoint->rem_qp_num,
            endpoint->rem_lid);
    return ORTE_SUCCESS;
}


static int mca_bmi_ib_endpoint_init(mca_bmi_ib_endpoint_t *endpoint)
{
    return OMPI_SUCCESS;
}

/*
 * Start to connect to the endpoint. We send our Queue Pair
 * information over the TCP OOB communication mechanism.

 * On completion of our send, a send completion handler 
 * is called.
 *
 */

static int mca_bmi_ib_endpoint_start_connect(mca_bmi_base_endpoint_t* endpoint)
{
    mca_bmi_ib_module_t* ib_bmi = endpoint->endpoint_bmi;
    int rc;
    
    
    /* Create the Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_bmi_ib_create_qp(ib_bmi->nic,
                    ib_bmi->ptag,
                    ib_bmi->cq_hndl,
                    ib_bmi->cq_hndl,
                    &endpoint->lcl_qp_hndl,
                    &endpoint->lcl_qp_prop,
                    VAPI_TS_RC))) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }

    D_PRINT("Initialized QP num = %d, LID = %d",
            endpoint->lcl_qp_prop.qp_num,
            ib_bmi->port.lid);

    /* Send connection info over to remote endpoint */
    endpoint->endpoint_state = MCA_BMI_IB_CONNECTING;
    if(OMPI_SUCCESS != (rc = mca_bmi_ib_endpoint_send_connect_req(endpoint))) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 * Reply to a `start - connect' message
 *
 */
static int mca_bmi_ib_endpoint_reply_start_connect(mca_bmi_ib_endpoint_t *endpoint, orte_buffer_t* buffer)
{
    mca_bmi_ib_module_t* ib_bmi = endpoint->endpoint_bmi;
    int rc;

    /* Create the Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_bmi_ib_create_qp(ib_bmi->nic,
                    ib_bmi->ptag,
                    ib_bmi->cq_hndl,
                    ib_bmi->cq_hndl,
                    &endpoint->lcl_qp_hndl,
                    &endpoint->lcl_qp_prop,
                    VAPI_TS_RC))) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }
    
    D_PRINT("Initialized QP num = %d, LID = %d",
            endpoint->lcl_qp_prop.qp_num,
            ib_bmi->port.lid);



    /* Set the remote side info */
    mca_bmi_ib_endpoint_set_remote_info(endpoint, buffer);

    /* Connect to endpoint */

    rc = mca_bmi_ib_endpoint_connect(endpoint);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }

    /* Send connection info over to remote endpoint */
    if(OMPI_SUCCESS != (rc = mca_bmi_ib_endpoint_send_connect_req(endpoint))) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 *
 */

static void mca_bmi_ib_endpoint_connected(mca_bmi_ib_endpoint_t *endpoint)
{
    endpoint->endpoint_state = MCA_BMI_IB_CONNECTED;
    mca_bmi_ib_progress_send_frags(endpoint);
}

/*
 * Non blocking OOB recv callback.
 * Read incoming QP and other info, and if this endpoint
 * is trying to connect, reply with our QP info, 
 * otherwise try to modify QP's and establish
 * reliable connection
 *
 */

static void mca_bmi_ib_endpoint_recv(
    int status,
    orte_process_name_t* endpoint, 
    orte_buffer_t* buffer,
    orte_rml_tag_t tag, 
    void* cbdata)
{
    mca_bmi_ib_proc_t *ib_proc;
    mca_bmi_ib_endpoint_t *ib_endpoint;
    int endpoint_state;
    int rc;

    for(ib_proc = (mca_bmi_ib_proc_t*)
            ompi_list_get_first(&mca_bmi_ib_component.ib_procs);
            ib_proc != (mca_bmi_ib_proc_t*)
            ompi_list_get_end(&mca_bmi_ib_component.ib_procs);
            ib_proc  = (mca_bmi_ib_proc_t*)ompi_list_get_next(ib_proc)) {

        if(ib_proc->proc_guid.vpid == endpoint->vpid) {

            /* Try to get the endpoint instance of this proc */

            /* Limitation: Right now, we have only 1 endpoint
             * for every process. Need several changes, some
             * in PML/BMI interface to set this right */
            ib_endpoint = ib_proc->proc_endpoints[0];

            endpoint_state = ib_endpoint->endpoint_state;

            /* Update status */
            switch(endpoint_state) {
                case MCA_BMI_IB_CLOSED :
                    /* We had this connection closed before.
                     * The endpoint is trying to connect. Move the
                     * status of this connection to CONNECTING,
                     * and then reply with our QP information */

                    if(OMPI_SUCCESS != (rc = mca_bmi_ib_endpoint_reply_start_connect(ib_endpoint, buffer))) {
                        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
                        break;
                    }

                    /* Setup state as connected */
                    ib_endpoint->endpoint_state = MCA_BMI_IB_CONNECT_ACK;
                    break;

                case MCA_BMI_IB_CONNECTING :

                    mca_bmi_ib_endpoint_set_remote_info(ib_endpoint, buffer);
                    if(OMPI_SUCCESS != (rc = mca_bmi_ib_endpoint_connect(ib_endpoint))) {
                        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
                        break;
                    }

                    /* Setup state as connected */
                    mca_bmi_ib_endpoint_connected(ib_endpoint);

                    /* Send him an ack */
                    mca_bmi_ib_endpoint_send_connect_ack(ib_endpoint);
                    break;

                case MCA_BMI_IB_CONNECT_ACK:

                    mca_bmi_ib_endpoint_connected(ib_endpoint);

                    break;

                case MCA_BMI_IB_CONNECTED :
                    break;
                default :
                    ompi_output(0, "Connected -> Connecting not possible.\n");
            }

            break;
        }
    }

    /* Okay, now that we are done receiving,
     * re-post the buffer */
    mca_bmi_ib_post_recv();
}

void mca_bmi_ib_post_recv()
{
    D_PRINT("");

    orte_rml.recv_buffer_nb(
        ORTE_RML_NAME_ANY, 
        ORTE_RML_TAG_DYNAMIC-1, 
        0,
        mca_bmi_ib_endpoint_recv,
        NULL);
}


/*
 * Attempt to send a fragment using a given endpoint. If the endpoint is not
 * connected, queue the fragment and start the connection as required.
 */

int mca_bmi_ib_endpoint_send(
                             mca_bmi_base_endpoint_t* endpoint,
                             mca_bmi_ib_frag_t* frag
                             )
{
    int rc;
    mca_bmi_ib_module_t *ib_bmi; 
    
    OMPI_THREAD_LOCK(&endpoint->endpoint_send_lock);
    
    switch(endpoint->endpoint_state) {
        case MCA_BMI_IB_CONNECTING:

            D_PRINT("Queing because state is connecting");

            ompi_list_append(&endpoint->pending_send_frags,
                    (ompi_list_item_t *)frag);

            rc = OMPI_SUCCESS;
            break;

        case MCA_BMI_IB_CONNECT_ACK:

            D_PRINT("Queuing because waiting for ack");

            ompi_list_append(&endpoint->pending_send_frags,
                    (ompi_list_item_t *)frag);

            rc = OMPI_SUCCESS;
            break;

        case MCA_BMI_IB_CLOSED:

            D_PRINT("Connection to endpoint closed ... connecting ...");

            ompi_list_append(&endpoint->pending_send_frags,
                    (ompi_list_item_t *)frag);

            rc = mca_bmi_ib_endpoint_start_connect(endpoint);
            
            break;

        case MCA_BMI_IB_FAILED:

            rc = OMPI_ERR_UNREACH;
            break;

        case MCA_BMI_IB_CONNECTED:
            {
                ib_bmi = endpoint->endpoint_bmi;
                
                
                A_PRINT("Send to : %d, len : %d, frag : %p", 
                        endpoint->endpoint_proc->proc_guid.vpid,
                        frag->ib_buf.desc.sg_entry.len,
                        frag);
                
                rc = mca_bmi_ib_endpoint_post_send(ib_bmi, endpoint, frag); 
                
                
                if(ib_bmi->rr_posted <= mca_bmi_ib_component.ib_rr_buf_min+1)
                    mca_bmi_ib_endpoint_post_rr(mca_bmi_ib_component.ib_rr_buf_max - ib_bmi->rr_posted, 
                                                endpoint); 
                
                
/*                 rc = mca_bmi_ib_post_send(endpoint->endpoint_bmi, endpoint, */
/*                                           &frag->ib_buf, (void*) frag); */
/*                 while(NULL != (item = ompi_list_remove_first(&ib_bmi->repost))) { */
/*                     mca_bmi_ib_buffer_repost(ib_bmi->nic, item); */
/*                 } */
/*                 break; */

                break; 
            }

    default:
        rc = OMPI_ERR_UNREACH;
    }
    
    OMPI_THREAD_UNLOCK(&endpoint->endpoint_send_lock);
    
    return rc;
}

void mca_bmi_ib_progress_send_frags(mca_bmi_ib_endpoint_t* endpoint)
{
    ompi_list_item_t *frag_item;
    mca_bmi_ib_frag_t *frag;
    mca_bmi_ib_module_t* ib_bmi; 
    /*Check if endpoint is connected */
    if(endpoint->endpoint_state != MCA_BMI_IB_CONNECTED) {

        return;
    }

    /* While there are frags in the list,
     * process them */

    while(!ompi_list_is_empty(&(endpoint->pending_send_frags))) {
        frag_item = ompi_list_remove_first(&(endpoint->pending_send_frags));
        frag = (mca_bmi_ib_frag_t *) frag_item;
        ib_bmi = endpoint->endpoint_bmi;
        /* We need to post this one */
        
        if(OMPI_SUCCESS !=  mca_bmi_ib_endpoint_post_send(ib_bmi, endpoint, frag))
            ompi_output(0, "error in mca_bmi_ib_endpoint_send");
    }
}



/*
 * Complete connection to endpoint.
 */

int mca_bmi_ib_endpoint_connect(
    mca_bmi_ib_endpoint_t *endpoint)
{
    int rc;
    mca_bmi_ib_module_t *ib_bmi = endpoint->endpoint_bmi;
    /* Connection establishment RC */
    rc = mca_bmi_ib_qp_init(ib_bmi->nic,
                            endpoint->lcl_qp_hndl,
                            endpoint->rem_qp_num,
                            endpoint->rem_lid, 
                            ib_bmi->port_id);

    
    if(rc != OMPI_SUCCESS) {
        return rc;
    }
    rc = mca_bmi_ib_qp_query(ib_bmi, 
                            endpoint->lcl_qp_hndl, 
                            endpoint->rem_qp_num); 
    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    mca_bmi_ib_endpoint_post_rr(mca_bmi_ib_component.ib_rr_buf_max, endpoint); 
    
    return OMPI_SUCCESS;
}
