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
#include "bmi_ib.h"
#include "bmi_ib_endpoint.h" 
#include "bmi_ib_proc.h"
#include "bmi_ib_frag.h"
#include "class/ompi_free_list.h" 

static void mca_bmi_ib_endpoint_construct(mca_bmi_base_endpoint_t* endpoint);
static void mca_bmi_ib_endpoint_destruct(mca_bmi_base_endpoint_t* endpoint);

int mca_bmi_ib_endpoint_create_qp(
                                  mca_bmi_ib_module_t* ib_bmi, 
                                  VAPI_hca_hndl_t nic,
                                  VAPI_pd_hndl_t ptag, 
                                  VAPI_cq_hndl_t cq_hndl, 
                                  VAPI_qp_hndl_t* qp_hndl, 
                                  VAPI_qp_prop_t* qp_prop, 
                                  int transport_type); 


int mca_bmi_ib_endpoint_qp_init_query(

                                      mca_bmi_ib_module_t* ib_bmi, 
                                      VAPI_hca_hndl_t nic, 
                                      VAPI_qp_hndl_t qp_hndl, 
                                      VAPI_qp_num_t remote_qp_num, 
                                      IB_lid_t remote_lid, 
                                      IB_port_t port_id
                                      ); 

                   
static inline int mca_bmi_ib_endpoint_post_send(mca_bmi_ib_module_t* ib_bmi, mca_bmi_ib_endpoint_t * endpoint, mca_bmi_ib_frag_t * frag)
{ 
    
    frag->sr_desc.remote_qkey = 0; 
    
    VAPI_qp_hndl_t qp_hndl; 
    if(frag->base.des_flags && MCA_BMI_DES_FLAGS_PRIORITY){ 
        frag->sr_desc.remote_qp = endpoint->rem_qp_num_high; 
        qp_hndl = endpoint->lcl_qp_hndl_high; 
    } else {
        frag->sr_desc.remote_qp = endpoint->rem_qp_num_low; 
        qp_hndl = endpoint->lcl_qp_hndl_low; 
    } 

    frag->sg_entry.len = frag->segment.seg_len + ((unsigned char*) frag->segment.seg_addr.pval - (unsigned char*) frag->hdr);  /* sizeof(mca_bmi_ib_header_t); */ 

    if(frag->sg_entry.len <= ib_bmi->ib_inline_max) { 
            frag->ret = EVAPI_post_inline_sr(ib_bmi->nic, 
                                 qp_hndl, 
                                 &frag->sr_desc); 
        
    }else { 
        frag->ret = VAPI_post_sr(ib_bmi->nic, 
                                 qp_hndl,
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

    rc = orte_dps.pack(buffer, &endpoint->lcl_qp_prop_high.qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    rc = orte_dps.pack(buffer, &endpoint->lcl_qp_prop_low.qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dps.pack(buffer, &endpoint->endpoint_bmi->port.lid, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* send to endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, buffer, ORTE_RML_TAG_DYNAMIC-1, 0,
         mca_bmi_ib_endpoint_send_cb, NULL);
    
    
    DEBUG_OUT("Sending High Priority QP num = %d, Low Priority QP num = %d, LID = %d",
              endpoint->lcl_qp_prop_high.qp_num,
              endpoint->lcl_qp_prop_low.qp_num,
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
    rc = orte_dps.unpack(buffer, &endpoint->rem_lid, &cnt, ORTE_UINT32);
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

static int mca_bmi_ib_endpoint_start_connect(mca_bmi_base_endpoint_t* endpoint)
{
    int rc;
    
    
    /* Create the High Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_bmi_ib_endpoint_create_qp(endpoint->endpoint_bmi, 
                                                           endpoint->endpoint_bmi->nic, 
                                                           endpoint->endpoint_bmi->ptag, 
                                                           endpoint->endpoint_bmi->cq_hndl_high, 
                                                           &endpoint->lcl_qp_hndl_high, 
                                                           &endpoint->lcl_qp_prop_high, 
                                                           VAPI_TS_RC))) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                    ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }


    /* Create the Low Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_bmi_ib_endpoint_create_qp(endpoint->endpoint_bmi, 
                                                           endpoint->endpoint_bmi->nic, 
                                                           endpoint->endpoint_bmi->ptag, 
                                                           endpoint->endpoint_bmi->cq_hndl_low, 
                                                           &endpoint->lcl_qp_hndl_low, 
                                                           &endpoint->lcl_qp_prop_low, 
                                                           VAPI_TS_RC))) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                    ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }

    DEBUG_OUT("Initialized High Priority QP num = %d, Low Priority QP num = %d,  LID = %d",
              endpoint->lcl_qp_prop_high.qp_num,
              endpoint->lcl_qp_prop_low.qp_num,
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
    int rc;

    
    /* Create the High Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_bmi_ib_endpoint_create_qp(endpoint->endpoint_bmi, 
                                                           endpoint->endpoint_bmi->nic, 
                                                           endpoint->endpoint_bmi->ptag, 
                                                           endpoint->endpoint_bmi->cq_hndl_high, 
                                                           &endpoint->lcl_qp_hndl_high, 
                                                           &endpoint->lcl_qp_prop_high, 
                                                           VAPI_TS_RC))) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                    ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }


    /* Create the Low Priority Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_bmi_ib_endpoint_create_qp(endpoint->endpoint_bmi, 
                                                           endpoint->endpoint_bmi->nic, 
                                                           endpoint->endpoint_bmi->ptag, 
                                                           endpoint->endpoint_bmi->cq_hndl_low, 
                                                           &endpoint->lcl_qp_hndl_low, 
                                                           &endpoint->lcl_qp_prop_low, 
                                                           VAPI_TS_RC))) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                    ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }

    DEBUG_OUT("Initialized High Priority QP num = %d, Low Priority QP num = %d,  LID = %d",
              endpoint->lcl_qp_prop_high.qp_num,
              endpoint->lcl_qp_prop_low.qp_num,
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
    DEBUG_OUT("");

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

            DEBUG_OUT("Queing because state is connecting");

            ompi_list_append(&endpoint->pending_send_frags,
                    (ompi_list_item_t *)frag);

            rc = OMPI_SUCCESS;
            break;

        case MCA_BMI_IB_CONNECT_ACK:

            DEBUG_OUT("Queuing because waiting for ack");

            ompi_list_append(&endpoint->pending_send_frags,
                    (ompi_list_item_t *)frag);

            rc = OMPI_SUCCESS;
            break;

        case MCA_BMI_IB_CLOSED:

            DEBUG_OUT("Connection to endpoint closed ... connecting ...");

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
                
                
                DEBUG_OUT("Send to : %d, len : %d, frag : %p", 
                        endpoint->endpoint_proc->proc_guid.vpid,
                        frag->ib_buf.desc.sg_entry.len,
                        frag);
                
                rc = mca_bmi_ib_endpoint_post_send(ib_bmi, endpoint, frag); 
                
                
                
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
    /* Connection establishment RC */
    rc = mca_bmi_ib_endpoint_qp_init_query(endpoint->endpoint_bmi, 
                                           endpoint->endpoint_bmi->nic, 
                                           endpoint->lcl_qp_hndl_high, 
                                           endpoint->rem_qp_num_high, 
                                           endpoint->rem_lid,                                            
                                           endpoint->endpoint_bmi->port_id); 
    
    rc = mca_bmi_ib_endpoint_qp_init_query(endpoint->endpoint_bmi, 
                                           endpoint->endpoint_bmi->nic, 
                                           endpoint->lcl_qp_hndl_low, 
                                           endpoint->rem_qp_num_low, 
                                           endpoint->rem_lid,                                            
                                           endpoint->endpoint_bmi->port_id); 
    
    
    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    mca_bmi_ib_endpoint_post_rr(endpoint, 0); 
    
    return OMPI_SUCCESS;
}



int mca_bmi_ib_endpoint_create_qp(
                                  mca_bmi_ib_module_t* ib_bmi, 
                                  VAPI_hca_hndl_t nic,
                                  VAPI_pd_hndl_t ptag, 
                                  VAPI_cq_hndl_t cq_hndl, 
                                  VAPI_qp_hndl_t* qp_hndl, 
                                  VAPI_qp_prop_t* qp_prop, 
                                  int transport_type)
{
    
    VAPI_ret_t ret;
    VAPI_qp_init_attr_t qp_init_attr;
    
    switch(transport_type) {

    case VAPI_TS_RC: /* Set up RC qp parameters */
        qp_init_attr.cap.max_oust_wr_rq = ib_bmi->ib_wq_size;
        qp_init_attr.cap.max_oust_wr_sq = ib_bmi->ib_wq_size;
        qp_init_attr.cap.max_sg_size_rq = ib_bmi->ib_sg_list_size;
        qp_init_attr.cap.max_sg_size_sq = ib_bmi->ib_sg_list_size;
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
    
    ret = VAPI_create_qp(nic, &qp_init_attr, 
            qp_hndl, qp_prop);
    
    if(VAPI_OK != ret) {
        MCA_BMI_IB_VAPI_ERROR(ret, "VAPI_create_qp");
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

int mca_bmi_ib_endpoint_qp_init_query(

                                      mca_bmi_ib_module_t* ib_bmi, 
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
    qp_attr.pkey_ix = ib_bmi->ib_pkey_ix;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PKEY_IX);
    qp_attr.port = port_id; 
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PORT);
    qp_attr.remote_atomic_flags = VAPI_EN_REM_WRITE | VAPI_EN_REM_READ;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_REMOTE_ATOMIC_FLAGS);

    ret = VAPI_modify_qp(nic, qp_hndl,
            &qp_attr, &qp_attr_mask, &qp_cap);

    if(VAPI_OK != ret) {
        MCA_BMI_IB_VAPI_ERROR(ret, "VAPI_modify_qp");
        return OMPI_ERROR;
    }

    DEBUG_OUT("Modified to init..Qp %d", qp_hndl);

    /**********************  INIT --> RTR  ************************/
    QP_ATTR_MASK_CLR_ALL(qp_attr_mask);
    qp_attr.qp_state = VAPI_RTR;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_QP_STATE);
    qp_attr.qp_ous_rd_atom = ib_bmi->ib_qp_ous_rd_atom;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_QP_OUS_RD_ATOM);
    qp_attr.path_mtu = ib_bmi->ib_mtu;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PATH_MTU);
    qp_attr.rq_psn = ib_bmi->ib_psn;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_RQ_PSN);
    qp_attr.pkey_ix = ib_bmi->ib_pkey_ix;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PKEY_IX);
    qp_attr.min_rnr_timer = ib_bmi->ib_min_rnr_timer;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_MIN_RNR_TIMER);

    qp_attr.av.sl = ib_bmi->ib_service_level;
    qp_attr.av.grh_flag = FALSE;
    qp_attr.av.static_rate = ib_bmi->ib_static_rate;
    qp_attr.av.src_path_bits = ib_bmi->ib_src_path_bits;

    qp_attr.dest_qp_num = remote_qp_num;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_DEST_QP_NUM);
    qp_attr.av.dlid = remote_lid;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_AV);

    ret = VAPI_modify_qp(nic, qp_hndl,
            &qp_attr, &qp_attr_mask, &qp_cap);

    if(VAPI_OK != ret) {
        MCA_BMI_IB_VAPI_ERROR(ret, "VAPI_modify_qp");
        return OMPI_ERROR;
    }

    DEBUG_OUT("Modified to RTR..Qp %d", qp_hndl);

    /************** RTS *******************/
    QP_ATTR_MASK_CLR_ALL(qp_attr_mask);
    qp_attr.qp_state = VAPI_RTS;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_QP_STATE);
    qp_attr.sq_psn = ib_bmi->ib_psn;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_SQ_PSN);
    qp_attr.timeout = ib_bmi->ib_timeout;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_TIMEOUT);
    qp_attr.retry_count = ib_bmi->ib_retry_count;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_RETRY_COUNT);
    qp_attr.rnr_retry = ib_bmi->ib_rnr_retry;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_RNR_RETRY);
    qp_attr.ous_dst_rd_atom = ib_bmi->ib_max_rdma_dst_ops;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_OUS_DST_RD_ATOM);

    ret = VAPI_modify_qp(nic, qp_hndl,
            &qp_attr, &qp_attr_mask, &qp_cap);

    if(VAPI_OK != ret) {
        MCA_BMI_IB_VAPI_ERROR(ret, "VAPI_modify_qp");
        return OMPI_ERROR;
    }
    DEBUG_OUT("Modified to RTS..Qp %d", qp_hndl);
    
    ret = VAPI_query_qp(nic, qp_hndl, &qp_attr, &qp_attr_mask, &qp_init_attr );          
    if (ret != VAPI_OK) {                                                                     
        ompi_output(0, "error querying the queue pair"); 
        return OMPI_ERROR; 
    }                      
    
    ib_bmi->ib_inline_max = qp_init_attr.cap.max_inline_data_sq;  
    
    return OMPI_SUCCESS;
}

