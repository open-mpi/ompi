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

#include "mca/ns/ns.h"

#include "mca/oob/tcp/oob_tcp.h"

/*
 * Similiar to unix readv(2)
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
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

    if(mca_oob_tcp_component.tcp_debug > 3) {
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_recv: tag %d\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(peer),
            tag);
    }

    /* lock the tcp struct */
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);

    /* check to see if a matching receive is on the list */
    msg = mca_oob_tcp_msg_match_recv(peer, tag);
    if(NULL != msg) {

        if(msg->msg_rc < 0)  {
            OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
            return msg->msg_rc;
        }
 
        /* if we are returning an allocated buffer - just take it from the message */
        if(flags & MCA_OOB_ALLOC) {

            if(NULL == iov || 0 == count) {
                return OMPI_ERR_BAD_PARAM;
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
                OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
                return rc;
            }
        }

        /* otherwise dequeue the message and return to free list */
        ompi_list_remove_item(&mca_oob_tcp_component.tcp_msg_recv, (ompi_list_item_t *) msg);
        MCA_OOB_TCP_MSG_RETURN(msg);
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
        return rc;
    }

    /* the message has not already been received. So we add it to the receive queue */
    MCA_OOB_TCP_MSG_ALLOC(msg, rc);
    if(NULL == msg) {
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
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
        msg->msg_hdr.msg_dst = *MCA_OOB_NAME_ANY;
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
    ompi_list_append(&mca_oob_tcp_component.tcp_msg_post, (ompi_list_item_t *) msg);
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);

    /* wait for the receive to complete */
    mca_oob_tcp_msg_wait(msg, &rc);
    MCA_OOB_TCP_MSG_RETURN(msg);
    return rc;
}

/*
 * Non-blocking version of mca_oob_recv().
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
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
    int i, rc, size = 0;

    /* lock the tcp struct */
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);

    /* check to see if a matching receive is on the list */
    msg = mca_oob_tcp_msg_match_recv(peer, tag);
    if(NULL != msg) {

        if(msg->msg_rc < 0)  {
            OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
            return msg->msg_rc; 
        }

        /* if we are returning an allocated buffer - just take it from the message */
        if(flags & MCA_OOB_ALLOC) {

            if(NULL == iov || 0 == count) {
                OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
                return OMPI_ERR_BAD_PARAM;
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
                for(i=1; i<msg->msg_rwcnt+1; i++)
                   rc += msg->msg_rwiov[i].iov_len;
            }
            if(MCA_OOB_PEEK & flags) {
                 OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
                 cbfunc(rc, &msg->msg_peer, iov, count, tag, cbdata);
                 return 0;
            }
        }

        /* otherwise dequeue the message and return to free list */
        ompi_list_remove_item(&mca_oob_tcp_component.tcp_msg_recv, (ompi_list_item_t *) msg);
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
        cbfunc(rc, &msg->msg_peer, iov, count, msg->msg_hdr.msg_tag, cbdata);
        MCA_OOB_TCP_MSG_RETURN(msg);
        return rc;
    }

    /* the message has not already been received. So we add it to the receive queue */
    MCA_OOB_TCP_MSG_ALLOC(msg, rc);
    if(NULL == msg) {
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
        return rc;
    }

    /* determine overall size of user supplied buffer */
    for(i = 0; i < count; i++) {
        size += iov[i].iov_len;
    }

    /* fill in the header */
    msg->msg_hdr.msg_src = *orte_process_info.my_name;
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
    ompi_list_append(&mca_oob_tcp_component.tcp_msg_post, (ompi_list_item_t *) msg);
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
    return 0;
}


/*
 * Cancel non-blocking recv.
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
 * @param tag (IN)     User supplied tag for matching send/recv.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv_cancel(
    orte_process_name_t* name, 
    int tag)
{
    int matched = 0, cmpval1, cmpval2;
    ompi_list_item_t *item, *next;

    /* wait for any previously matched messages to be processed */
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);
    while(mca_oob_tcp_component.tcp_match_count) {
        ompi_condition_wait(
            &mca_oob_tcp_component.tcp_match_cond, 
            &mca_oob_tcp_component.tcp_match_lock);
    }

    /* remove any matching posted receives */
    for(item =  ompi_list_get_first(&mca_oob_tcp_component.tcp_msg_post);
        item != ompi_list_get_end(&mca_oob_tcp_component.tcp_msg_post);
        item =  next) {
        mca_oob_tcp_msg_t* msg = (mca_oob_tcp_msg_t*)item;
        next = ompi_list_get_next(item);

        cmpval1 = orte_ns.compare(ORTE_NS_CMP_ALL, name, MCA_OOB_NAME_ANY);
        cmpval2 = orte_ns.compare(ORTE_NS_CMP_ALL, &msg->msg_peer, name);
        if ((0 == cmpval1) || (0 == cmpval2)) {
            if (msg->msg_hdr.msg_tag == tag) {
                ompi_list_remove_item(&mca_oob_tcp_component.tcp_msg_post, &msg->super);
                MCA_OOB_TCP_MSG_RETURN(msg);
                matched++;
            }
        }
    }
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
    return (matched > 0) ? OMPI_SUCCESS : OMPI_ERR_NOT_FOUND;
}

