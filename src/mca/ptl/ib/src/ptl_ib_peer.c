/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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
#include "ptl_ib.h"
#include "ptl_ib_addr.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_proc.h"
#include "ptl_ib_sendfrag.h"

static void mca_ptl_ib_peer_construct(mca_ptl_base_peer_t* module_peer);
static void mca_ptl_ib_peer_destruct(mca_ptl_base_peer_t* module_peer);

OBJ_CLASS_INSTANCE(mca_ptl_ib_peer_t, 
        ompi_list_item_t, mca_ptl_ib_peer_construct, 
        mca_ptl_ib_peer_destruct);

/*
 * Callback function for OOB send completion.
 * Not much to do over here right now ...
 * 
 */

static void mca_ptl_ib_peer_connect_send_callback(int status,
        ompi_process_name_t* peer, ompi_buffer_t buffer,
        int tag, void* cbdata)
{
    D_PRINT("OOB Send to %d complete", peer->vpid);
}

/*
 * Wrapper around mca_oob_send_packed_nb
 *
 * Post a non-blocking OOB send request to peer with
 * pre-allocated user buffer
 *
 */

static int mca_ptl_ib_post_oob_send_nb(ompi_process_name_t *name,
        void* user_buf, int len)
{
    int rc;
    ompi_buffer_t buffer;

    rc = ompi_buffer_init_preallocated(&buffer, user_buf,
            len);

    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    rc = mca_oob_send_packed_nb(name, buffer, 
            131313, 0,
            (mca_oob_callback_packed_fn_t)mca_ptl_ib_peer_connect_send_callback, 
            NULL);

    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    return rc;
}

/*
 * Initialize state of the peer instance.
 *
 */

static void mca_ptl_ib_peer_construct(mca_ptl_base_peer_t* module_peer)
{
    module_peer->peer_module = 0;
    module_peer->peer_proc = 0;
    module_peer->peer_ts = 0.0;
    module_peer->peer_state = MCA_PTL_IB_CLOSED;
    module_peer->peer_retries = 0;
    OBJ_CONSTRUCT(&module_peer->peer_send_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&module_peer->peer_recv_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&module_peer->pending_send_frags, ompi_list_t);
}

/*
 * Destroy a peer
 *
 */

static void mca_ptl_ib_peer_destruct(mca_ptl_base_peer_t* module_peer)
{
}

/*
 * Allocate peer connection structures
 *
 */

static int mca_ptl_ib_alloc_peer_conn(mca_ptl_base_peer_t* peer)
{
    /* Allocate space for peer connection */
    peer->peer_conn = (mca_ptl_ib_peer_conn_t *)
        malloc(sizeof(mca_ptl_ib_peer_conn_t));
    if(NULL == peer->peer_conn) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

/*
 * Send connection information to remote peer using OOB
 *
 */

static int mca_ptl_ib_peer_send_conn_info(mca_ptl_base_peer_t* peer)
{
    int rc;
    ompi_process_name_t *name;
    char* sendbuf;

    name = &peer->peer_proc->proc_guid;

    sendbuf = (char*) malloc(sizeof(char)*50);

    if(NULL == sendbuf) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Zero out the send buffer */
    memset(sendbuf, 0, 50);

    /* Copy the info in the send buffer */

    /* Format:
     *
     * <QP> <LID>
     * Ofcourse without the <'s and >'s moron!
     * Size of each field is limited to maximum
     * 8 characters. This should be enough for all
     * platforms, and is internal information
     */
    sprintf(sendbuf, "%08d %08d", 
            peer->peer_conn->lres->qp_prop.qp_num,
            peer->peer_module->ib_state->port.lid);

    /* Send it off */
    rc = mca_ptl_ib_post_oob_send_nb(name, 
            (void*)sendbuf, 50);

    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    D_PRINT("Sent buffer : %s", sendbuf);

    return OMPI_SUCCESS;
}

/*
 * Send connect ACK to remote peer
 *
 */

static void mca_ptl_ib_peer_send_connect_ack(mca_ptl_base_peer_t* peer)
{
    int rc;
    ompi_process_name_t *name;
    char* sendbuf;
    int zero = 0;

    name = &peer->peer_proc->proc_guid;

    sendbuf = (char*) malloc(sizeof(char)*50);

    if(NULL == sendbuf) {
        ompi_output(0, "Out of resources");
    }

    /* Zero out the send buffer */
    memset(sendbuf, 0, 50);

    /* Copy the info in the send buffer */

    /* Format:
     *
     * <QP> <LID>
     * Ofcourse without the <'s and >'s moron!
     * Size of each field is limited to maximum
     * 8 characters. This should be enough for all
     * platforms, and is internal information
     */
    sprintf(sendbuf, "%08d %08d", zero, zero);

    /* Send it off */
    rc = mca_ptl_ib_post_oob_send_nb(name, 
            (void*)sendbuf, 50);

    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "Error in sending connect ack!");
    }

    D_PRINT("Sent buffer : %s", sendbuf);
}

/*
 * Set remote connection info
 *
 * XXX: Currently size is unutilized, this shall change
 * as soon as we add more info to be exchanged at connection
 * setup.
 *
 */
static void mca_ptl_ib_peer_set_remote_info(mca_ptl_base_peer_t* peer,
        void* baseptr, size_t size)
{
    char tempbuf[9];

    memset(tempbuf, 0, 9);
    strncpy(tempbuf, (char*)baseptr, 8);

    peer->peer_conn->rres->qp_num = atoi(tempbuf);

    memset(tempbuf, 0, 9);
    strncpy(tempbuf, (char*)baseptr + 9*sizeof(char), 8);

    peer->peer_conn->rres->lid = atoi(tempbuf);

    D_PRINT("Received QP num = %d, LID = %d",
            peer->peer_conn->rres->qp_num,
            peer->peer_conn->rres->lid);
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
    int rc;

    /* Allocate peer connection structures */
    rc = mca_ptl_ib_alloc_peer_conn(peer);
    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    /* Initialize the peer */
    rc = mca_ptl_ib_init_peer(peer->peer_module->ib_state,
            peer->peer_conn);
    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    /* Send connection info over to remote peer */
    rc = mca_ptl_ib_peer_send_conn_info(peer);
    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    /* Update status of peer to as connecting */
    peer->peer_state = MCA_PTL_IB_CONNECTING;

    return rc;
}

/*
 * Reply to a `start - connect' message
 *
 */
static int mca_ptl_ib_peer_reply_start_connect(mca_ptl_ib_peer_t *peer,
        void* baseptr, size_t size)
{
    int rc;

    /* Allocate peer connection structures */
    rc = mca_ptl_ib_alloc_peer_conn(peer);
    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    /* Initialize the peer */
    rc = mca_ptl_ib_init_peer(peer->peer_module->ib_state,
            peer->peer_conn);
    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    /* Set the remote side info */
    mca_ptl_ib_peer_set_remote_info(peer, baseptr, size);

    /* Connect to peer */
    rc = mca_ptl_ib_peer_connect(peer->peer_module->ib_state,
            peer->peer_conn);
    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    /* Send connection info over to remote peer */
    rc = mca_ptl_ib_peer_send_conn_info(peer);
    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    return rc;
}

/*
 *
 */

static void mca_ptl_ib_peer_connected(mca_ptl_ib_peer_t *peer)
{
    peer->peer_state = MCA_PTL_IB_CONNECTED;

    DUMP_PEER(peer);
}

/*
 * Non blocking OOB recv callback.
 * Read incoming QP and other info, and if this peer
 * is trying to connect, reply with our QP info, 
 * otherwise try to modify QP's and establish
 * reliable connection
 *
 */

static void mca_ptl_ib_peer_connect_recv_callback(int status,
        ompi_process_name_t* peer, ompi_buffer_t buffer,
        int tag, void* cbdata)
{
    size_t size;
    void *baseptr, *dataptr, *fromptr;
    mca_ptl_ib_proc_t *ib_proc;
    mca_ptl_ib_peer_t *ib_peer;
    int peer_state;

    ompi_buffer_size(buffer, &size);

    ompi_buffer_get_ptrs(buffer, &baseptr,
            &dataptr, &fromptr);
    
    D_PRINT("Size recv: %d, Data: %s", size, baseptr);

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

#if 0
                    D_PRINT("Start Connect %d",
                            ib_proc->proc_guid.vpid);
#endif
                    ompi_output(0, "Start Connect %d",
                            ib_proc->proc_guid.vpid);

                    if(mca_ptl_ib_peer_reply_start_connect(ib_peer,
                                baseptr, size)
                            != OMPI_SUCCESS) {
                        D_PRINT("Connect Error");
                    }

                    /* Setup state as connected */
                    ib_peer->peer_state = MCA_PTL_IB_CONNECT_ACK;

                    break;

                case MCA_PTL_IB_CONNECTING :
                    /* We are already connecting with this peer,
                     * this means that we have initiated OOB sends
                     * with this peer, and the peer is replying.
                     * No need to send him any more stuff */

#if 0
                    D_PRINT("Connect reply %d", 
                            ib_proc->proc_guid.vpid);
#endif
                    ompi_output(0, "Connect reply %d", 
                            ib_proc->proc_guid.vpid);

                    mca_ptl_ib_peer_set_remote_info(ib_peer, 
                            baseptr, size);

                    if(mca_ptl_ib_peer_connect(ib_peer->peer_module->ib_state,
                                ib_peer->peer_conn)
                            != OMPI_SUCCESS) {
                        D_PRINT("Connect Error");
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
    mca_ptl_ib_post_oob_recv_nb();
}

void mca_ptl_ib_post_oob_recv_nb()
{
    D_PRINT("");

    mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY,
            131313, 0, 
            (mca_oob_callback_packed_fn_t)mca_ptl_ib_peer_connect_recv_callback,
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

            /* Send the frag off */

            A_PRINT("Send to : %d, len : %d, frag : %p", 
                    peer->peer_proc->proc_guid.vpid,
                    frag->ib_buf.desc.sg_entry.len,
                    frag);

            rc = mca_ptl_ib_post_send(peer->peer_module->ib_state,
                    peer->peer_conn, 
                    &frag->ib_buf, (void*) frag);


            break;
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
        if(mca_ptl_ib_post_send(peer->peer_module->ib_state,
                    peer->peer_conn, &sendfrag->ib_buf,
                    (void*) sendfrag)
                != OMPI_SUCCESS) {
            ompi_output(0, "Error in posting send");
        }
    }
}
