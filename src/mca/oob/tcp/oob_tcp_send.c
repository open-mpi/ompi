/*
 * $HEADER$
 */
#include "ompi_config.h"
#include "mca/oob/tcp/oob_tcp.h"

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
    ompi_process_name_t* name, 
    struct iovec *iov, 
    int count, 
    int tag,
    int flags)
{
    mca_oob_tcp_peer_t* peer = mca_oob_tcp_peer_lookup(name);
    mca_oob_tcp_msg_t* msg;
    int size;
    int rc;

    if(mca_oob_tcp_component.tcp_debug > 1) {
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_send: tag %d\n",
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            tag);
    }
    if(NULL == peer)
        return OMPI_ERR_UNREACH;

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
    msg->msg_hdr.msg_src = mca_oob_name_self;
    msg->msg_hdr.msg_dst = *name;
    MCA_OOB_TCP_HDR_HTON(&msg->msg_hdr);

    /* create one additional iovect that will hold the header */
    msg->msg_type = MCA_OOB_TCP_POSTED;
    msg->msg_rc = 0;
    msg->msg_flags = flags;
    msg->msg_uiov = iov;
    msg->msg_ucnt = count;
    msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg, count+1);
    msg->msg_rwiov[0].iov_base = &msg->msg_hdr;
    msg->msg_rwiov[0].iov_len = sizeof(msg->msg_hdr);
    msg->msg_rwptr = msg->msg_rwiov;
    msg->msg_rwcnt = msg->msg_rwnum = count + 1;
    memcpy(msg->msg_rwiov+1, msg->msg_uiov, sizeof(struct iovec)*msg->msg_ucnt);
    msg->msg_rwbuf = NULL;
    msg->msg_cbfunc = NULL;
    msg->msg_cbdata = NULL;
    msg->msg_complete = false;
    msg->msg_peer = peer->peer_name;
    
    rc = mca_oob_tcp_peer_send(peer, msg);
    if(rc != OMPI_SUCCESS) {
        MCA_OOB_TCP_MSG_RETURN(msg);
        return rc;
    }

    rc = mca_oob_tcp_msg_wait(msg, &size);
    MCA_OOB_TCP_MSG_RETURN(msg);
    if(rc != OMPI_SUCCESS)
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
    ompi_process_name_t* name, 
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
        return OMPI_ERR_UNREACH;

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
    msg->msg_hdr.msg_src = mca_oob_name_self;
    msg->msg_hdr.msg_dst = *name;
    MCA_OOB_TCP_HDR_HTON(&msg->msg_hdr);

    /* create one additional iovect that will hold the size of the message */
    msg->msg_type = MCA_OOB_TCP_POSTED;
    msg->msg_rc = 0;
    msg->msg_flags = flags;
    msg->msg_uiov = iov;
    msg->msg_ucnt = count;
    msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg,count+1);
    msg->msg_rwiov[0].iov_base = &msg->msg_hdr;
    msg->msg_rwiov[0].iov_len = sizeof(msg->msg_hdr);
    msg->msg_rwptr = msg->msg_rwiov;
    msg->msg_rwcnt = msg->msg_rwnum = count + 1;
    memcpy(msg->msg_rwiov+1, msg->msg_uiov, sizeof(struct iovec)*msg->msg_ucnt);
    msg->msg_rwbuf = NULL;
    msg->msg_cbfunc = cbfunc;
    msg->msg_cbdata = cbdata;
    msg->msg_complete = false;
    msg->msg_peer = peer->peer_name;
    
    rc = mca_oob_tcp_peer_send(peer, msg);
    if(rc != OMPI_SUCCESS) {
        MCA_OOB_TCP_MSG_RETURN(msg);
        return rc;
    }
    return OMPI_SUCCESS;
}

