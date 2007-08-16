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

#include "orte/mca/ns/ns.h"

#include "orte/mca/oob/tcp/oob_tcp.h"

/*
 * Similiar to unix readv(2)
 *
 * @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
 * @param count (IN)   Number of elements in iovec array.
 * @param tag (IN)     User supplied tag for matching send/recv.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to the number of bytes provided in the
 *                     iovec array without removing the message from the queue.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */
int mca_oob_tcp_recv(
    orte_process_name_t* peer, 
    struct iovec *iov, 
    int count, 
    int tag,
    int flags)
{
    mca_oob_tcp_msg_t *msg;
    int i, rc = 0, size = 0;

    if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_ALL) {
        opal_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_recv: tag %d\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(peer),
            tag);
    }

    /* lock the tcp struct */
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);

    /* check to see if a matching receive is on the list */
    msg = mca_oob_tcp_msg_match_recv(peer, tag);
    if(NULL != msg) {

        if(msg->msg_rc < 0)  {
            OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
            return msg->msg_rc;
        }
 
        /* if we are returning an allocated buffer - just take it from the message */
        if(flags & MCA_OOB_ALLOC) {

            if(NULL == iov || 0 == count) {
                return ORTE_ERR_BAD_PARAM;
            }
            iov[0].iov_base = (ompi_iov_base_ptr_t)msg->msg_rwbuf;
            iov[0].iov_len = msg->msg_hdr.msg_size;
            msg->msg_rwbuf = NULL;
            rc = msg->msg_hdr.msg_size;

        } else {

            /* if we are just doing peek, return bytes without dequeing message */
            rc = mca_oob_tcp_msg_copy(msg, iov, count);
            if(rc >= 0 && MCA_OOB_TRUNC & flags) {
                rc = 0;
                /* skip first iovec element which is the header */
                for(i=1; i<msg->msg_rwcnt+1; i++)
                   rc += msg->msg_rwiov[i].iov_len;
            }
            if(MCA_OOB_PEEK & flags) {
                OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
                return rc;
            }
        }

        /* otherwise dequeue the message and return to free list */
        opal_list_remove_item(&mca_oob_tcp_component.tcp_msg_recv, (opal_list_item_t *) msg);
        MCA_OOB_TCP_MSG_RETURN(msg);
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
        return rc;
    }

    /* the message has not already been received. So we add it to the receive queue */
    MCA_OOB_TCP_MSG_ALLOC(msg, rc);
    if(NULL == msg) {
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
        return rc;
    }

    /* determine overall size of user supplied buffer */
    for(i = 0; i < count; i++) {
        size += iov[i].iov_len;
    }

    /* fill in the struct */
    msg->msg_hdr.msg_size = size;
    msg->msg_hdr.msg_tag = tag;
    msg->msg_hdr.msg_type = MCA_OOB_TCP_DATA;
    msg->msg_hdr.msg_src = *peer;
    if (NULL == orte_process_info.my_name) {
        msg->msg_hdr.msg_dst = *ORTE_NAME_INVALID;
    } else {
        msg->msg_hdr.msg_dst = *orte_process_info.my_name;
    }
    msg->msg_type = MCA_OOB_TCP_POSTED;
    msg->msg_rc = 0;
    msg->msg_flags = flags;
    msg->msg_uiov = iov;
    msg->msg_ucnt = count;
    msg->msg_cbfunc = NULL;
    msg->msg_cbdata = NULL;
    msg->msg_complete = false;
    msg->msg_peer = *peer;
    msg->msg_rwbuf = NULL;
    msg->msg_rwiov = NULL;
    opal_list_append(&mca_oob_tcp_component.tcp_msg_post, (opal_list_item_t *) msg);
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);

    /* wait for the receive to complete */
    mca_oob_tcp_msg_wait(msg, &rc);
    MCA_OOB_TCP_MSG_RETURN(msg);
    return rc;
}


/**
 * Process a matched posted receive
 *
 * Note that the match lock must be held prior to the call.
 */

static void mca_oob_tcp_msg_matched(mca_oob_tcp_msg_t* msg, mca_oob_tcp_msg_t* match)
{
    int i,rc;
    if(match->msg_rc < 0)  {
        rc = match->msg_rc; 
    }

    /* if we are returning an allocated buffer - just take it from the message */
    else if(msg->msg_flags & MCA_OOB_ALLOC) {

        msg->msg_uiov[0].iov_base = (ompi_iov_base_ptr_t)match->msg_rwbuf;
        msg->msg_uiov[0].iov_len = match->msg_hdr.msg_size;
        match->msg_rwbuf = NULL;
        rc = match->msg_hdr.msg_size;

    } else {

        /* if we are just doing peek, return bytes without dequeing message */
        rc = mca_oob_tcp_msg_copy(match, msg->msg_uiov, msg->msg_ucnt);
        if(rc >= 0 && MCA_OOB_TRUNC & msg->msg_flags) {
            rc = 0;
            for(i=1; i<match->msg_rwcnt+1; i++)
                rc += match->msg_rwiov[i].iov_len;
        }
        if(MCA_OOB_PEEK & msg->msg_flags) {
            OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
            msg->msg_cbfunc(rc, 
                &match->msg_peer, 
                msg->msg_uiov, 
                msg->msg_ucnt,  
                match->msg_hdr.msg_tag, 
                msg->msg_cbdata);
            OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);
            return;
        }
    }

    /* otherwise remove the match */
    opal_list_remove_item(&mca_oob_tcp_component.tcp_msg_recv, (opal_list_item_t *) match);

    /* invoke callback */
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
    msg->msg_cbfunc(rc, 
        &match->msg_peer, 
        msg->msg_uiov, 
        msg->msg_ucnt, 
        match->msg_hdr.msg_tag, 
        msg->msg_cbdata);
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);

    /* return match to free list */
    MCA_OOB_TCP_MSG_RETURN(match);
}

/*
 * Non-blocking version of mca_oob_recv().
 *
 * @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param tag (IN)     User supplied tag for matching send/recv.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
 * @param cbfunc (IN)  Callback function on recv completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error.
 */
int mca_oob_tcp_recv_nb(
    orte_process_name_t* peer, 
    struct iovec* iov, 
    int count,
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc, 
    void* cbdata)
{
    mca_oob_tcp_msg_t *msg;
    mca_oob_tcp_msg_t *match;
    int i, rc, size = 0;

    /* validate params */
    if(NULL == iov || 0 == count) {
        return ORTE_ERR_BAD_PARAM;
    }

    /* allocate/initialize the posted receive */
    MCA_OOB_TCP_MSG_ALLOC(msg, rc);
    if(NULL == msg) {
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
        return rc;
    }

    /* determine overall size of user supplied buffer */
    for(i = 0; i < count; i++) {
        size += iov[i].iov_len;
    }

    /* fill in the header */
    if (NULL == orte_process_info.my_name) {
        msg->msg_hdr.msg_src = *ORTE_NAME_INVALID;
    } else {
        msg->msg_hdr.msg_src = *orte_process_info.my_name;
    }
    msg->msg_hdr.msg_dst = *peer;
    msg->msg_hdr.msg_size = size;
    msg->msg_hdr.msg_tag = tag;
    msg->msg_type = MCA_OOB_TCP_POSTED;
    msg->msg_rc = 0;
    msg->msg_flags = flags;
    msg->msg_uiov = iov;
    msg->msg_ucnt = count;
    msg->msg_cbfunc = cbfunc;
    msg->msg_cbdata = cbdata;
    msg->msg_complete = false;
    msg->msg_peer = *peer;
    msg->msg_rwbuf = NULL;
    msg->msg_rwiov = NULL;

    /* acquire the match lock */
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);
    if(flags & MCA_OOB_PERSISTENT) {

        opal_list_append(&mca_oob_tcp_component.tcp_msg_post, (opal_list_item_t *) msg);
        while(NULL != (match = mca_oob_tcp_msg_match_recv(peer,tag))) {
            mca_oob_tcp_msg_matched(msg, match);
        }

    } else {

        /* check to see if a matching receive is on the list */
        match = mca_oob_tcp_msg_match_recv(peer, tag);
        if(NULL != match) {
            mca_oob_tcp_msg_matched(msg, match);
            MCA_OOB_TCP_MSG_RETURN(msg);
        } else {
            opal_list_append(&mca_oob_tcp_component.tcp_msg_post, (opal_list_item_t *) msg);
        }
    }
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
    return 0;
}


/*
 * Cancel non-blocking recv.
 *
 * @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
 * @param tag (IN)     User supplied tag for matching send/recv.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv_cancel(
    orte_process_name_t* name, 
    int tag)
{
    int matched = 0;
    opal_list_item_t *item, *next;

    /* wait for any previously matched messages to be processed */
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);
#if OMPI_ENABLE_PROGRESS_THREADS
    if(opal_event_progress_thread() == false) {
        while(mca_oob_tcp_component.tcp_match_count) {
            opal_condition_wait(
                &mca_oob_tcp_component.tcp_match_cond, 
                &mca_oob_tcp_component.tcp_match_lock);
        }
    }
#endif

    /* remove any matching posted receives */
    for(item =  opal_list_get_first(&mca_oob_tcp_component.tcp_msg_post);
        item != opal_list_get_end(&mca_oob_tcp_component.tcp_msg_post);
        item =  next) {
        mca_oob_tcp_msg_t* msg = (mca_oob_tcp_msg_t*)item;
        next = opal_list_get_next(item);

        if (ORTE_EQUAL == orte_dss.compare(name, &msg->msg_peer, ORTE_NAME)) {
            if (msg->msg_hdr.msg_tag == tag) {
                opal_list_remove_item(&mca_oob_tcp_component.tcp_msg_post, &msg->super.super);
                MCA_OOB_TCP_MSG_RETURN(msg);
                matched++;
            }
        }
    }
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
    return (matched > 0) ? ORTE_SUCCESS : ORTE_ERR_NOT_FOUND;
}

