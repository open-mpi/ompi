#include "oob_tcp.h"
#include "oob_tcp_msg.h"
#include "oob_tcp_peer.h"

/*
 * Similiar to unix send(2).
 *
 * @param peer (IN)   Opaque name of peer process.
 * @param msg (IN)    Array of iovecs describing user buffers and lengths.
 * @param count (IN)  Number of elements in iovec array.
 * @param flags (IN)  Currently unused.
 * @return            OMPI error code (<0) on error number of bytes actually sent.
 */

int mca_oob_tcp_send(const ompi_process_name_t* name, const struct iovec *iov, int count, int flags)
{
    mca_oob_tcp_peer_t* peer = mca_oob_tcp_peer_lookup(name);
    mca_oob_tcp_msg_t* msg;
    int rc, sent;
    if(NULL == peer)
        return OMPI_ERR_UNREACH;

    MCA_OOB_TCP_MSG_ALLOC(msg, rc);
    if(NULL == msg) 
        return rc;

    msg->msg_user = iov;
    msg->msg_iov = (struct iovec*)malloc(sizeof(struct iovec)*count);
    msg->msg_rwptr = msg->msg_iov;
    msg->msg_count = msg->msg_rwcnt = count;
    memcpy(msg->msg_iov, msg->msg_user, sizeof(struct iovec)*count);
    msg->msg_cbfunc = NULL;
    msg->msg_cbdata = NULL;
    msg->msg_complete = false;

    rc = mca_oob_tcp_peer_send(peer, msg);
    if(rc != OMPI_SUCCESS) {
        MCA_OOB_TCP_MSG_RETURN(msg);
        return rc;
    }

    rc = mca_oob_tcp_msg_wait(msg, &sent);
    if(rc != OMPI_SUCCESS)
        return rc;
    return sent;
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
    const ompi_process_name_t* name, 
    const struct iovec* iov, 
    int count,
    int flags, 
    mca_oob_callback_fn_t cbfunc, 
    void* cbdata)
{
    mca_oob_tcp_peer_t* peer = mca_oob_tcp_peer_lookup(name);
    mca_oob_tcp_msg_t* msg;
    int rc;
    if(NULL == peer)
        return OMPI_ERR_UNREACH;

    MCA_OOB_TCP_MSG_ALLOC(msg, rc);
    if(NULL == msg) 
        return rc;

    msg->msg_user = iov;
    msg->msg_iov = (struct iovec*)malloc(sizeof(struct iovec)*count);
    msg->msg_rwptr = msg->msg_iov;
    msg->msg_count = msg->msg_rwcnt = count;
    memcpy(msg->msg_iov, msg->msg_user, sizeof(struct iovec)*count);
    msg->msg_cbfunc = cbfunc;
    msg->msg_cbdata = cbdata;
    msg->msg_complete = false;

    rc = mca_oob_tcp_peer_send(peer, msg);
    if(rc != OMPI_SUCCESS) {
        MCA_OOB_TCP_MSG_RETURN(msg);
        return rc;
    }
    return OMPI_SUCCESS;
}

