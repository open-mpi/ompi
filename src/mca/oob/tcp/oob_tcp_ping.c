/*
 * $HEADER$
 */
#include "ompi_config.h"
#include "mca/oob/tcp/oob_tcp.h"

/*
 * Ping a peer to see if it is alive.
 *
 * @param peer (IN)   Opaque name of peer process.
 * @param tv (IN)     Timeout to wait for a response.
 * @return            OMPI error code (<0) on error number of bytes actually sent.
 */

int mca_oob_tcp_ping(
    const ompi_process_name_t* name, 
    const struct timeval *timeout)
{
    mca_oob_tcp_peer_t* peer = mca_oob_tcp_peer_lookup(name);
    mca_oob_tcp_msg_t* msg;
    struct timeval tv;
    struct timespec ts;
    int rc;

    if(mca_oob_tcp_component.tcp_debug > 1) {
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_ping: timout %d secs %d usecs\n",
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            timeout->tv_sec, timeout->tv_usec);
    }
    if(NULL == peer)
        return OMPI_ERR_UNREACH;

    MCA_OOB_TCP_MSG_ALLOC(msg, rc);
    if(NULL == msg) 
        return rc;

    /* convert the header network byte order */
    msg->msg_hdr.msg_type = MCA_OOB_TCP_PING;
    msg->msg_hdr.msg_size = 0;
    msg->msg_hdr.msg_tag = 0;
    msg->msg_hdr.msg_src = mca_oob_name_self;
    msg->msg_hdr.msg_dst = *name;
    MCA_OOB_TCP_HDR_HTON(&msg->msg_hdr);

    /* create an iovec to hold the header */
    msg->msg_type = MCA_OOB_TCP_POSTED;
    msg->msg_rc = 0;
    msg->msg_flags = 0;
    msg->msg_uiov = NULL;
    msg->msg_ucnt = 0;
    msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg, 1);
    msg->msg_rwiov[0].iov_base = &msg->msg_hdr;
    msg->msg_rwiov[0].iov_len = sizeof(msg->msg_hdr);
    msg->msg_rwptr = msg->msg_rwiov;
    msg->msg_rwcnt = msg->msg_rwnum = 1;
    msg->msg_rwbuf = NULL;
    msg->msg_cbfunc = NULL;
    msg->msg_cbdata = NULL;
    msg->msg_complete = false;
    msg->msg_peer = peer->peer_name;
    
    /* initiate the send */
    rc = mca_oob_tcp_peer_send(peer, msg);
    if(rc != OMPI_SUCCESS) {
        MCA_OOB_TCP_MSG_RETURN(msg);
        return rc;
    }

    /* setup a timeout based on absolute time and wait for completion */
    gettimeofday(&tv, NULL);
    tv.tv_sec += timeout->tv_sec;
    tv.tv_usec += timeout->tv_usec;
    while(tv.tv_usec > 1000000) {
        tv.tv_sec++;
        tv.tv_usec -= 1000000;
    }
    ts.tv_sec = tv.tv_sec;
    ts.tv_nsec = (tv.tv_usec * 1000);
    rc = mca_oob_tcp_msg_timedwait(msg, NULL, &ts);
    if(rc != OMPI_SUCCESS)
        mca_oob_tcp_peer_dequeue_msg(peer,msg);
    MCA_OOB_TCP_MSG_RETURN(msg);
    return rc;
}

