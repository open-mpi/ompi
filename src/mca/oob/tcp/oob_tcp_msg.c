/* 
 * $HEADER$
 */
#include "mca/oob/tcp/oob_tcp.h"
#include "mca/oob/tcp/oob_tcp_msg.h"


static void mca_oob_tcp_msg_construct(mca_oob_tcp_msg_t*);
static void mca_oob_tcp_msg_destruct(mca_oob_tcp_msg_t*);


OBJ_CLASS_INSTANCE(
    mca_oob_tcp_msg_t,
    ompi_list_item_t,
    mca_oob_tcp_msg_construct,
    mca_oob_tcp_msg_destruct);


static void mca_oob_tcp_msg_construct(mca_oob_tcp_msg_t* msg)
{
    OBJ_CONSTRUCT(&msg->msg_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&msg->msg_condition, ompi_condition_t);
}


static void mca_oob_tcp_msg_destruct(mca_oob_tcp_msg_t* msg)
{
    OBJ_DESTRUCT(&msg->msg_lock);
    OBJ_DESTRUCT(&msg->msg_condition);
}


/*
 *  Wait for a msg to complete.
 *  @param  msg (IN)   Message to wait on.
 *  @param  rc (OUT)   Return code (number of bytes read on success or error code on failure).
 *  @retval OMPI_SUCCESS or error code on failure.
 */

int mca_oob_tcp_msg_wait(mca_oob_tcp_msg_t* msg, int* rc)
{
#if OMPI_HAVE_THREADS
    OMPI_THREAD_LOCK(&msg->msg_lock);
    while(msg->msg_complete == false) {
        if(ompi_event_progress_thread()) {
            OMPI_THREAD_UNLOCK(&msg->msg_lock);
            ompi_event_loop(OMPI_EVLOOP_ONCE);
            OMPI_THREAD_LOCK(&msg->msg_lock);
        } else {
           ompi_condition_wait(&msg->msg_condition, &msg->msg_lock);
        }
    }
    OMPI_THREAD_UNLOCK(&msg->msg_lock);

#else
    /* wait for message to complete */
    while(msg->msg_complete == false)
        ompi_event_loop(OMPI_EVLOOP_ONCE);
#endif

    /* return status */
    if(NULL != rc) {
        *rc = msg->msg_rc;
    }
    return OMPI_SUCCESS;
}

/*
 *  Signal that a message has completed.
 *  @param  msg (IN)   Message to wait on.
 *  @param peer (IN) the peer of the message
 *  @retval OMPI_SUCCESS or error code on failure.
 */
int mca_oob_tcp_msg_complete(mca_oob_tcp_msg_t* msg, ompi_process_name_t * peer)
{
    ompi_mutex_lock(&msg->msg_lock);
    msg->msg_complete = true;
    if(NULL != msg->msg_cbfunc) {
        msg->msg_cbfunc(msg->msg_rc, peer, msg->msg_uiov, msg->msg_ucnt, ntohl(msg->msg_hdr.msg_tag), msg->msg_cbdata);
        ompi_mutex_unlock(&msg->msg_lock);
        MCA_OOB_TCP_MSG_RETURN(msg);
    } else {
        ompi_condition_broadcast(&msg->msg_condition);
        ompi_mutex_unlock(&msg->msg_lock);
    }
    return OMPI_SUCCESS;
}

/*
 * The function that actually sends the data!
 * @param msg a pointer to the message to send
 * @param peer the peer we are sending to
 * @retval true if the entire message has been sent
 * @retval false if the entire message has not been sent
 */
bool mca_oob_tcp_msg_send_handler(mca_oob_tcp_msg_t* msg, struct mca_oob_tcp_peer_t * peer)
{
    int rc;
    while(1) {
        rc = writev(peer->peer_sd, msg->msg_rwptr, msg->msg_rwnum);
        if(rc < 0) {
            if(errno == EINTR)
                continue;
            else if (errno == EAGAIN)
                return false;
            else {
                ompi_output(0, "mca_oob_tcp_msg_send_handler: bad return from writev. errno=%d", errno);
                mca_oob_tcp_peer_close(peer);
                return false;
            }
        }

        msg->msg_rc += rc;
        do {/* while there is still more iovecs to write */
            if(rc < msg->msg_rwptr->iov_len) {
                msg->msg_rwptr->iov_len -= rc;
                msg->msg_rwptr->iov_base = (void *) ((char *) msg->msg_rwptr->iov_base + rc);
                break;
            } else {
                rc -= msg->msg_rwptr->iov_len;
                (msg->msg_rwnum)--;
                (msg->msg_rwptr)++;
                if(0 == msg->msg_rwnum) {
                    return true;
                }
            }
        } while(msg->msg_rwnum);
    }
}

/*
 * Receives message data.
 * @param msg the message to be recieved into
 * @param peer the peer to recieve from
 * @retval true if the whole message was received
 * @retval false if the whole message was not received
 */
bool mca_oob_tcp_msg_recv_handler(mca_oob_tcp_msg_t* msg, struct mca_oob_tcp_peer_t * peer)
{
    int rc;
    while(1) {
        rc = readv(peer->peer_sd, msg->msg_rwptr, msg->msg_rwnum);
        if(rc < 0) {
            if(errno == EINTR)
                continue;
            else if (errno == EAGAIN)
                return false;
            else {
                ompi_output(0, "mca_oob_tcp_msg_recv_handler: readv failed with errno=%d", errno);
                mca_oob_tcp_peer_close(peer);
                return false;
            }
        } else if (rc == 0)  {
            ompi_output(0, "mca_oob_tcp_msg_recv_handler: read failedd - peer closed connection");
            mca_oob_tcp_peer_close(peer);
            return false;
        }

        do {
            if(rc < msg->msg_rwptr->iov_len) {
                msg->msg_rwptr->iov_len -= rc;
                msg->msg_rwptr->iov_base = (void *) ((char *) msg->msg_rwptr->iov_base + rc);
                break;
            } else {
                rc -= msg->msg_rwptr->iov_len;
                (msg->msg_rwnum)--;
                (msg->msg_rwptr)++;
                if(0 == msg->msg_rwnum) {
                    return true;
                }
            }
        } while(msg->msg_rwnum);
    }
}

/*
 *  Called to copy the results of a message into user supplied iovec array.
 *  @param  msg (IN)   Message send that is in progress.
 *  @param  iov (IN)   Iovec array of user supplied buffers.
 *  @retval count      Number of elements in iovec array.
 */

int mca_oob_tcp_msg_copy(mca_oob_tcp_msg_t* msg, struct iovec* iov, int count)
{
    int i;
    struct iovec *src = msg->msg_rwiov;
    struct iovec *dst = iov;
    unsigned char* src_ptr = (unsigned char*)src->iov_base;
    size_t src_len = src->iov_len;
    int src_cnt = 0;
    int rc = 0;

    for(i=0; i<count; i++) {
        unsigned char* dst_ptr = (unsigned char*)dst->iov_base;
        size_t dst_len = dst->iov_len;
        while(dst_len > 0) {
            size_t len = (dst_len <= src_len) ? dst_len : src_len;
            memcpy(dst_ptr, src_ptr, len);
            rc += len;
            dst_ptr += len;
            dst_len -= len;
            src_ptr += len;
            src_len -= len;
            if(src_len == 0) {
                if(++src_cnt == count)
                    return rc;
                src++;
                src_ptr = src->iov_base;
                src_len = src->iov_len;
            }
        }
        dst++;
    }
    return rc;
}

/*
 *  Match name to a message that has been received asynchronously (unexpected).
 *
 *  @param  name (IN)  Name associated with peer or wildcard to match first posted recv.
 *  @return msg        Matched message or NULL.
 *
 *  Note - this routine requires the caller to be holding the module lock.
 */

mca_oob_tcp_msg_t* mca_oob_tcp_msg_match_recv(ompi_process_name_t* name, int tag)
{
    mca_oob_tcp_msg_t* msg;
    for(msg =  (mca_oob_tcp_msg_t*) ompi_list_get_first(&mca_oob_tcp_component.tcp_msg_recv);
        msg != (mca_oob_tcp_msg_t*) ompi_list_get_end(&mca_oob_tcp_component.tcp_msg_recv);
        msg =  (mca_oob_tcp_msg_t*) ompi_list_get_next(msg)) {

        if((0 == mca_oob_tcp_process_name_compare(name,MCA_OOB_NAME_ANY) ||
           (0 == mca_oob_tcp_process_name_compare(name, &msg->msg_peer)))) {
            if (tag == MCA_OOB_TAG_ANY || tag == msg->msg_hdr.msg_tag) {
                return msg;
            }
        }
    }
    return NULL;
}

/*
 *  Match name to a posted recv request.
 *
 *  @param  name (IN)  Name associated with peer or wildcard to match first posted recv.
 *  @return msg        Matched message or NULL.
 *
 *  Note - this routine requires the caller to be holding the module lock.
 */
                                                                                                                    
mca_oob_tcp_msg_t* mca_oob_tcp_msg_match_post(ompi_process_name_t* name, int tag, bool peek)
{
    mca_oob_tcp_msg_t* msg;
    for(msg =  (mca_oob_tcp_msg_t*) ompi_list_get_first(&mca_oob_tcp_component.tcp_msg_post);
        msg != (mca_oob_tcp_msg_t*) ompi_list_get_end(&mca_oob_tcp_component.tcp_msg_post);
        msg =  (mca_oob_tcp_msg_t*) ompi_list_get_next(msg)) {

        if((0 == mca_oob_tcp_process_name_compare(&msg->msg_peer,MCA_OOB_NAME_ANY) ||
           (0 == mca_oob_tcp_process_name_compare(&msg->msg_peer,name)))) {
            if (msg->msg_hdr.msg_tag == MCA_OOB_TAG_ANY || msg->msg_hdr.msg_tag == tag) {
                if((msg->msg_flags & MCA_OOB_PEEK) == 0 || peek) {
                    ompi_list_remove_item(&mca_oob_tcp_component.tcp_msg_post, &msg->super);
                    return msg;
                } else {
                    return NULL;
                }
            }
        }
    }
    return NULL;
}




