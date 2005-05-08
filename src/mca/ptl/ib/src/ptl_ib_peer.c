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
#include "ptl_ib.h"
#include "ptl_ib_addr.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_proc.h"
#include "ptl_ib_priv.h"
#include "ptl_ib_sendfrag.h"

static void mca_ptl_ib_peer_construct(mca_ptl_base_peer_t* peer);
static void mca_ptl_ib_peer_destruct(mca_ptl_base_peer_t* peer);

OBJ_CLASS_INSTANCE(mca_ptl_ib_peer_t, 
        ompi_list_item_t, mca_ptl_ib_peer_construct, 
        mca_ptl_ib_peer_destruct);

/*
 * Initialize state of the peer instance.
 *
 */

static void mca_ptl_ib_peer_construct(mca_ptl_base_peer_t* peer)
{
    peer->peer_ptl = 0;
    peer->peer_proc = 0;
    peer->peer_tstamp = 0.0;
    peer->peer_state = MCA_PTL_IB_CLOSED;
    peer->peer_retries = 0;
    OBJ_CONSTRUCT(&peer->peer_send_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&peer->peer_recv_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&peer->pending_send_frags, ompi_list_t);
}

/*
 * Destroy a peer
 *
 */

static void mca_ptl_ib_peer_destruct(mca_ptl_base_peer_t* peer)
{
}

/*
 * Send connection information to remote peer using OOB
 *
 */

static void mca_ptl_ib_peer_send_cb(
    int status,
    orte_process_name_t* peer, 
    orte_buffer_t* buffer,
    orte_rml_tag_t tag, 
    void* cbdata)
{
    OBJ_RELEASE(buffer);
}


static int mca_ptl_ib_peer_send_connect_req(mca_ptl_base_peer_t* peer)
{
    orte_buffer_t* buffer = OBJ_NEW(orte_buffer_t);
    int rc;
    if(NULL == buffer) {
         ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
         return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* pack the info in the send buffer */
    rc = orte_dps.pack(buffer, &peer->lcl_qp_prop.qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dps.pack(buffer, &peer->peer_ptl->port.lid, 1, ORTE_UINT32);

    /* send to peer */
    rc = orte_rml.send_buffer_nb(&peer->peer_proc->proc_guid, buffer, ORTE_RML_TAG_DYNAMIC-1, 0,
         mca_ptl_ib_peer_send_cb, NULL);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 * Send connect ACK to remote peer
 *
 */

static int mca_ptl_ib_peer_send_connect_ack(mca_ptl_base_peer_t* peer)
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

    /* send to peer */
    rc = orte_rml.send_buffer_nb(&peer->peer_proc->proc_guid, buffer, ORTE_RML_TAG_DYNAMIC-1, 0,
         mca_ptl_ib_peer_send_cb, NULL);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
}

/*
 * Set remote connection info
 *
 * XXX: Currently size is unutilized, this shall change
 * as soon as we add more info to be exchanged at connection
 * setup.
 *
 */
static int mca_ptl_ib_peer_set_remote_info(mca_ptl_base_peer_t* peer, orte_buffer_t* buffer)
{
    int rc;
    size_t cnt = 1;
    rc = orte_dps.unpack(buffer, &peer->rem_qp_num, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dps.unpack(buffer, &peer->rem_lid, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    D_PRINT("Received QP num = %d, LID = %d",
            peer->rem_qp_num,
            peer->rem_lid);
    return ORTE_SUCCESS;
}


static int mca_ptl_ib_peer_init(
    mca_ptl_ib_peer_t *peer)
{
    return OMPI_SUCCESS;
}

/*
 * Start to connect to the peer. We send our Queue Pair
 * information over the TCP OOB communication mechanism.
 * On completion of our send, a send completion handler 
 * is called.
 *
 */

static int mca_ptl_ib_peer_start_connect(mca_ptl_base_peer_t* peer)
{
    mca_ptl_ib_module_t* ib_ptl = peer->peer_ptl;
    int rc;

    /* Create the Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_ptl_ib_create_qp(ib_ptl->nic,
                    ib_ptl->ptag,
                    ib_ptl->cq_hndl,
                    ib_ptl->cq_hndl,
                    &peer->lcl_qp_hndl,
                    &peer->lcl_qp_prop,
                    VAPI_TS_RC))) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }

    /* Send connection info over to remote peer */
    peer->peer_state = MCA_PTL_IB_CONNECTING;
    if(OMPI_SUCCESS != (rc = mca_ptl_ib_peer_send_connect_req(peer))) {
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
static int mca_ptl_ib_peer_reply_start_connect(mca_ptl_ib_peer_t *peer, orte_buffer_t* buffer)
{
    mca_ptl_ib_module_t* ib_ptl = peer->peer_ptl;
    int rc;

    /* Create the Queue Pair */
    if(OMPI_SUCCESS != (rc = mca_ptl_ib_create_qp(ib_ptl->nic,
                    ib_ptl->ptag,
                    ib_ptl->cq_hndl,
                    ib_ptl->cq_hndl,
                    &peer->lcl_qp_hndl,
                    &peer->lcl_qp_prop,
                    VAPI_TS_RC))) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }

    /* Set the remote side info */
    mca_ptl_ib_peer_set_remote_info(peer, buffer);

    /* Connect to peer */
    rc = mca_ptl_ib_peer_connect(peer);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }

    /* Send connection info over to remote peer */
    if(OMPI_SUCCESS != (rc = mca_ptl_ib_peer_send_connect_req(peer))) {
        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 *
 */

static void mca_ptl_ib_peer_connected(mca_ptl_ib_peer_t *peer)
{
    peer->peer_state = MCA_PTL_IB_CONNECTED;
    mca_ptl_ib_progress_send_frags(peer);
}

/*
 * Non blocking OOB recv callback.
 * Read incoming QP and other info, and if this peer
 * is trying to connect, reply with our QP info, 
 * otherwise try to modify QP's and establish
 * reliable connection
 *
 */

static void mca_ptl_ib_peer_recv(
    int status,
    orte_process_name_t* peer, 
    orte_buffer_t* buffer,
    orte_rml_tag_t tag, 
    void* cbdata)
{
    mca_ptl_ib_proc_t *ib_proc;
    mca_ptl_ib_peer_t *ib_peer;
    int peer_state;
    int rc;

    for(ib_proc = (mca_ptl_ib_proc_t*)
            ompi_list_get_first(&mca_ptl_ib_component.ib_procs);
            ib_proc != (mca_ptl_ib_proc_t*)
            ompi_list_get_end(&mca_ptl_ib_component.ib_procs);
            ib_proc  = (mca_ptl_ib_proc_t*)ompi_list_get_next(ib_proc)) {

        if(ib_proc->proc_guid.vpid == peer->vpid) {

            /* Try to get the peer instance of this proc */

            /* Limitation: Right now, we have only 1 peer
             * for every process. Need several changes, some
             * in PML/PTL interface to set this right */
            ib_peer = ib_proc->proc_peers[0];

            peer_state = ib_peer->peer_state;

            /* Update status */
            switch(peer_state) {
                case MCA_PTL_IB_CLOSED :
                    /* We had this connection closed before.
                     * The peer is trying to connect. Move the
                     * status of this connection to CONNECTING,
                     * and then reply with our QP information */

                    if(OMPI_SUCCESS != (rc = mca_ptl_ib_peer_reply_start_connect(ib_peer, buffer))) {
                        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
                        break;
                    }

                    /* Setup state as connected */
                    ib_peer->peer_state = MCA_PTL_IB_CONNECT_ACK;
                    break;

                case MCA_PTL_IB_CONNECTING :

                    mca_ptl_ib_peer_set_remote_info(ib_peer, buffer);
                    if(OMPI_SUCCESS != (rc = mca_ptl_ib_peer_connect(ib_peer))) {
                        ompi_output(0, "[%lu,%lu,%lu] %s:%d errcode %d\n", 
                            ORTE_NAME_ARGS(orte_process_info.my_name), __FILE__,__LINE__,rc);
                        break;
                    }

                    /* Setup state as connected */
                    mca_ptl_ib_peer_connected(ib_peer);

                    /* Send him an ack */
                    mca_ptl_ib_peer_send_connect_ack(ib_peer);
                    break;

                case MCA_PTL_IB_CONNECT_ACK:

                    mca_ptl_ib_peer_connected(ib_peer);

                    break;

                case MCA_PTL_IB_CONNECTED :
                    break;
                default :
                    ompi_output(0, "Connected -> Connecting not possible.\n");
            }

            break;
        }
    }

    /* Okay, now that we are done receiving,
     * re-post the buffer */
    mca_ptl_ib_post_recv();
}

void mca_ptl_ib_post_recv()
{
    D_PRINT("");

    orte_rml.recv_buffer_nb(
        ORTE_RML_NAME_ANY, 
        ORTE_RML_TAG_DYNAMIC-1, 
        0,
        mca_ptl_ib_peer_recv,
        NULL);
}


/*
 * Attempt to send a fragment using a given peer. If the peer is not
 * connected, queue the fragment and start the connection as required.
 */

int mca_ptl_ib_peer_send(mca_ptl_base_peer_t* peer,
        mca_ptl_ib_send_frag_t* frag)
{
    int rc;


    OMPI_THREAD_LOCK(&peer->peer_send_lock);

    switch(peer->peer_state) {
        case MCA_PTL_IB_CONNECTING:

            D_PRINT("Queing because state is connecting");

            ompi_list_append(&peer->pending_send_frags,
                    (ompi_list_item_t *)frag);

            rc = OMPI_SUCCESS;
            break;

        case MCA_PTL_IB_CONNECT_ACK:

            D_PRINT("Queuing because waiting for ack");

            ompi_list_append(&peer->pending_send_frags,
                    (ompi_list_item_t *)frag);

            rc = OMPI_SUCCESS;
            break;

        case MCA_PTL_IB_CLOSED:

            D_PRINT("Connection to peer closed ... connecting ...");

            ompi_list_append(&peer->pending_send_frags,
                    (ompi_list_item_t *)frag);

            rc = mca_ptl_ib_peer_start_connect(peer);

            break;

        case MCA_PTL_IB_FAILED:

            rc = OMPI_ERR_UNREACH;
            break;

        case MCA_PTL_IB_CONNECTED:
            {
            mca_ptl_ib_module_t* ib_ptl = peer->peer_ptl;
            ompi_list_item_t* item;

            A_PRINT("Send to : %d, len : %d, frag : %p", 
                    peer->peer_proc->proc_guid.vpid,
                    frag->ib_buf.desc.sg_entry.len,
                    frag);

            rc = mca_ptl_ib_post_send(peer->peer_ptl, peer,
                    &frag->ib_buf, (void*) frag);
            while(NULL != (item = ompi_list_remove_first(&ib_ptl->repost))) {
                mca_ptl_ib_buffer_repost(ib_ptl->nic, item);
            }
            break;
            }
        default:
            rc = OMPI_ERR_UNREACH;
    }

    OMPI_THREAD_UNLOCK(&peer->peer_send_lock);

    return rc;
}

void mca_ptl_ib_progress_send_frags(mca_ptl_ib_peer_t* peer)
{
    ompi_list_item_t *frag_item;
    mca_ptl_ib_send_frag_t *sendfrag;

    /*Check if peer is connected */
    if(peer->peer_state != MCA_PTL_IB_CONNECTED) {

        return;
    }

    /* While there are frags in the list,
     * process them */

    while(!ompi_list_is_empty(&(peer->pending_send_frags))) {

        frag_item = ompi_list_remove_first(&(peer->pending_send_frags));
        sendfrag = (mca_ptl_ib_send_frag_t *) frag_item;

        /* We need to post this one */
        if(mca_ptl_ib_post_send(peer->peer_ptl, peer, &sendfrag->ib_buf,
                    (void*) sendfrag)
                != OMPI_SUCCESS) {
            ompi_output(0, "Error in posting send");
        }
    }
}


/*
 * Complete connection to peer.
 */

int mca_ptl_ib_peer_connect(
    mca_ptl_ib_peer_t *peer)
{
    int rc, i;
    VAPI_ret_t ret;
    ib_buffer_t *ib_buf_ptr;
    mca_ptl_ib_module_t *ib_ptl = peer->peer_ptl;

    /* Establish Reliable Connection */
    rc = mca_ptl_ib_qp_init(ib_ptl->nic,
                peer->lcl_qp_hndl,
                peer->rem_qp_num,
                peer->rem_lid);
                                                                                                                                
    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    /* Allocate resources to this connection */
    peer->lcl_recv = (ib_buffer_t*)
        malloc(sizeof(ib_buffer_t) * NUM_IB_RECV_BUF);
    if(NULL == peer->lcl_recv) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Register the buffers */
    for(i = 0; i < NUM_IB_RECV_BUF; i++) {

        rc = mca_ptl_ib_register_mem(ib_ptl->nic, ib_ptl->ptag,
                (void*) peer->lcl_recv[i].buf,
                MCA_PTL_IB_FIRST_FRAG_SIZE,
                &peer->lcl_recv[i].hndl);
        if(rc != OMPI_SUCCESS) {
            return OMPI_ERROR;
        }

        ib_buf_ptr = &peer->lcl_recv[i];
        ib_buf_ptr->qp_hndl = peer->lcl_qp_hndl;

        IB_PREPARE_RECV_DESC(ib_buf_ptr);
    }
                                                                                                                                
    /* Post receives */
    for(i = 0; i < NUM_IB_RECV_BUF; i++) {

        ret = VAPI_post_rr(ib_ptl->nic,
                peer->lcl_qp_hndl,
                &peer->lcl_recv[i].desc.rr);
        if(VAPI_OK != ret) {
            MCA_PTL_IB_VAPI_RET(ret, "VAPI_post_rr");
        }
    }
    return OMPI_SUCCESS;
}

