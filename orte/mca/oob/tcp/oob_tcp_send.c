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
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/util/proc_info.h"

#include "orte/mca/oob/tcp/oob_tcp.h"

static int mca_oob_tcp_send_self(
    mca_oob_tcp_peer_t* peer,
    mca_oob_tcp_msg_t* msg,
    struct iovec* iov, 
    int count)
{
    unsigned char *ptr;
    int size = 0;
    int rc;

    for(rc = 0; rc < count; rc++) {
        size += iov[rc].iov_len;
    }
    msg->msg_rwbuf = malloc(size);
    if(NULL == msg->msg_rwbuf) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    ptr = (unsigned char *)msg->msg_rwbuf;
    for(rc = 0; rc < count; rc++) {
        memcpy(ptr, iov[rc].iov_base, iov[rc].iov_len);
        ptr += iov[rc].iov_len;
    }
    msg->msg_hdr.msg_size = size;

    /*
     * Copied original buffer - so local send completion.
    */
                                                                                                     
    opal_mutex_lock(&msg->msg_lock);
    msg->msg_complete = true;
    if(NULL != msg->msg_cbfunc) {
        msg->msg_cbfunc(
            ORTE_SUCCESS, 
            &peer->peer_name, 
            msg->msg_uiov, 
            msg->msg_ucnt, 
            msg->msg_hdr.msg_tag, 
            msg->msg_cbdata);
    } else {
        opal_condition_broadcast(&msg->msg_condition);
    }
    opal_mutex_unlock(&msg->msg_lock);

    /*
     * Attempt to match against posted receive
     */
    mca_oob_tcp_msg_recv_complete(msg, peer);
    return size;
}

/*
 * Similiar to unix writev(2).
 *
 * @param peer (IN)   Opaque name of peer process.
 * @param msg (IN)    Array of iovecs describing user buffers and lengths.
 * @param count (IN)  Number of elements in iovec array.
 * @param flags (IN)  Currently unused.
 * @return            OMPI error code (<0) on error number of bytes actually sent.
 */

int mca_oob_tcp_send(
    orte_process_name_t* name, 
    struct iovec *iov, 
    int count, 
    int tag,
    int flags)
{
    mca_oob_tcp_peer_t* peer = mca_oob_tcp_peer_lookup(name);
    mca_oob_tcp_msg_t* msg;
    int size;
    int rc;

    if(NULL == peer)
        return ORTE_ERR_UNREACH;

    if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_ALL) {
        opal_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_send: tag %d\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            tag);
    }

    MCA_OOB_TCP_MSG_ALLOC(msg, rc);
    if(NULL == msg) 
        return rc;

    /* calculate the size of the message */
    size = 0;
    for(rc = 0; rc < count; rc++) {
        size += iov[rc].iov_len;
    }
   
    /* turn the size to network byte order so there will be no problems */
    msg->msg_hdr.msg_type = MCA_OOB_TCP_DATA;
    msg->msg_hdr.msg_size = size;
    msg->msg_hdr.msg_tag = tag;
    if (NULL == orte_process_info.my_name) {
        msg->msg_hdr.msg_src = *ORTE_NAME_INVALID;
    } else {
        msg->msg_hdr.msg_src = *orte_process_info.my_name;
    }
    msg->msg_hdr.msg_dst = *name;

    /* create one additional iovect that will hold the header */
    msg->msg_type = MCA_OOB_TCP_POSTED;
    msg->msg_rc = 0;
    msg->msg_flags = flags;
    msg->msg_uiov = iov;
    msg->msg_ucnt = count;
    msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg, count+1);
    msg->msg_rwiov[0].iov_base = (ompi_iov_base_ptr_t)(&msg->msg_hdr);
    msg->msg_rwiov[0].iov_len = sizeof(msg->msg_hdr);
    msg->msg_rwptr = msg->msg_rwiov;
    msg->msg_rwcnt = msg->msg_rwnum = count + 1;
    memcpy(msg->msg_rwiov+1, msg->msg_uiov, sizeof(struct iovec)*msg->msg_ucnt);
    msg->msg_rwbuf = NULL;
    msg->msg_cbfunc = NULL;
    msg->msg_cbdata = NULL;
    msg->msg_complete = false;
    msg->msg_peer = peer->peer_name;
    
    if (NULL != name && NULL != orte_process_info.my_name &&
        ORTE_EQUAL == mca_oob_tcp_process_name_compare(name, orte_process_info.my_name)) {  /* local delivery */
        return mca_oob_tcp_send_self(peer,msg,iov,count);
    }

    MCA_OOB_TCP_HDR_HTON(&msg->msg_hdr);
    rc = mca_oob_tcp_peer_send(peer, msg);
    if(rc != ORTE_SUCCESS) {
        MCA_OOB_TCP_MSG_RETURN(msg);
        return rc;
    }

    rc = mca_oob_tcp_msg_wait(msg, &size);
    MCA_OOB_TCP_MSG_RETURN(msg);
    if(rc != ORTE_SUCCESS)
        return rc;
    size -= sizeof(mca_oob_tcp_hdr_t);
    return size;
}

/*
 * Non-blocking version of mca_oob_send().
 *
 * @param peer (IN)    Opaque name of peer process.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   Currently unused.
 * @param cbfunc (IN)  Callback function on send completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error number of bytes actually sent.
 *
 */

int mca_oob_tcp_send_nb(
    orte_process_name_t* name, 
    struct iovec* iov, 
    int count,
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc, 
    void* cbdata)
{
    mca_oob_tcp_peer_t* peer = mca_oob_tcp_peer_lookup(name);
    mca_oob_tcp_msg_t* msg;
    int size;
    int rc;

    if(NULL == peer)
        return ORTE_ERR_UNREACH;

    MCA_OOB_TCP_MSG_ALLOC(msg, rc);
    if(NULL == msg) 
        return rc;

    /* calculate the size of the message */
    size = 0;
    for(rc = 0; rc < count; rc++) {
        size += iov[rc].iov_len;
    }
    /* turn the size to network byte order so there will be no problems */
    msg->msg_hdr.msg_type = MCA_OOB_TCP_DATA;
    msg->msg_hdr.msg_size = size;
    msg->msg_hdr.msg_tag = tag;
    if (NULL == orte_process_info.my_name) {
        msg->msg_hdr.msg_src = *ORTE_NAME_INVALID;
    } else {
        msg->msg_hdr.msg_src = *orte_process_info.my_name;
    }
    msg->msg_hdr.msg_dst = *name;

    /* create one additional iovect that will hold the size of the message */
    msg->msg_type = MCA_OOB_TCP_POSTED;
    msg->msg_rc = 0;
    msg->msg_flags = flags;
    msg->msg_uiov = iov;
    msg->msg_ucnt = count;
    msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg,count+1);
    msg->msg_rwiov[0].iov_base = (ompi_iov_base_ptr_t)(&msg->msg_hdr);
    msg->msg_rwiov[0].iov_len = sizeof(msg->msg_hdr);
    msg->msg_rwptr = msg->msg_rwiov;
    msg->msg_rwcnt = msg->msg_rwnum = count + 1;
    memcpy(msg->msg_rwiov+1, msg->msg_uiov, sizeof(struct iovec)*msg->msg_ucnt);
    msg->msg_rwbuf = NULL;
    msg->msg_cbfunc = cbfunc;
    msg->msg_cbdata = cbdata;
    msg->msg_complete = false;
    msg->msg_peer = peer->peer_name;
    
    if (ORTE_EQUAL == mca_oob_tcp_process_name_compare(name, orte_process_info.my_name)) {  /* local delivery */
        return mca_oob_tcp_send_self(peer,msg,iov,count);
    }

    MCA_OOB_TCP_HDR_HTON(&msg->msg_hdr);
    rc = mca_oob_tcp_peer_send(peer, msg);
    if(rc != ORTE_SUCCESS) {
        MCA_OOB_TCP_MSG_RETURN(msg);
        return rc;
    }
    return ORTE_SUCCESS;
}

