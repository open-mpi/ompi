/* 
 * $HEADER$
 */
#include "ompi_config.h"
#include "mca/oob/tcp/oob_tcp.h"
#include "mca/oob/tcp/oob_tcp_msg.h"


static void mca_oob_tcp_msg_construct(mca_oob_tcp_msg_t*);
static void mca_oob_tcp_msg_destruct(mca_oob_tcp_msg_t*);
static void mca_oob_tcp_msg_ident(mca_oob_tcp_msg_t* msg, mca_oob_tcp_peer_t* peer);
static bool mca_oob_tcp_msg_recv(mca_oob_tcp_msg_t* msg, mca_oob_tcp_peer_t* peer);
static void mca_oob_tcp_msg_data(mca_oob_tcp_msg_t* msg, mca_oob_tcp_peer_t* peer);
static void mca_oob_tcp_msg_ping(mca_oob_tcp_msg_t* msg, mca_oob_tcp_peer_t* peer);


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
            int rc;
            OMPI_THREAD_UNLOCK(&msg->msg_lock);
            rc = ompi_event_loop(OMPI_EVLOOP_ONCE);
            assert(rc == 0);
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
 *  Wait up to a timeout for the message to complete.
 *  @param  msg (IN)   Message to wait on.
 *  @param  rc (OUT)   Return code (number of bytes read on success or error code on failure).
 *  @retval OMPI_SUCCESS or error code on failure.
 */

int mca_oob_tcp_msg_timedwait(mca_oob_tcp_msg_t* msg, int* rc, struct timespec* abstime)
{
    struct timeval tv;
    uint32_t secs = abstime->tv_sec;
    uint32_t usecs = abstime->tv_nsec * 1000;
    gettimeofday(&tv,NULL);

#if OMPI_HAVE_THREADS
    OMPI_THREAD_LOCK(&msg->msg_lock);
    while(msg->msg_complete == false && 
          (tv.tv_sec <= secs ||
          (tv.tv_sec == secs && tv.tv_usec < usecs))) {
        if(ompi_event_progress_thread()) {
            int rc;
            OMPI_THREAD_UNLOCK(&msg->msg_lock);
            rc = ompi_event_loop(OMPI_EVLOOP_ONCE);
            assert(rc == 0);
            OMPI_THREAD_LOCK(&msg->msg_lock);
        } else {
           ompi_condition_timedwait(&msg->msg_condition, &msg->msg_lock, abstime);
        }
        gettimeofday(&tv,NULL);
    }
    OMPI_THREAD_UNLOCK(&msg->msg_lock);
#else
    /* wait for message to complete */
    while(msg->msg_complete == false &&
          ((uint32_t)tv.tv_sec <= secs ||
	   ((uint32_t)tv.tv_sec == secs && (uint32_t)tv.tv_usec < usecs))) {
        ompi_event_loop(OMPI_EVLOOP_ONCE);
        gettimeofday(&tv,NULL);
    }
#endif

    /* return status */
    if(NULL != rc) {
        *rc = msg->msg_rc;
    }
    if(msg->msg_rc < 0)
        return msg->msg_rc;
    return (msg->msg_complete ? OMPI_SUCCESS : OMPI_ERR_TIMEOUT);
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
        msg->msg_cbfunc(msg->msg_rc, peer, msg->msg_uiov, msg->msg_ucnt, msg->msg_hdr.msg_tag, msg->msg_cbdata);
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
            if(ompi_errno == EINTR)
                continue;
            else if (ompi_errno == EAGAIN)
                return false;
            else {
                ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_msg_send_handler: writev failed with ompi_errno=%d", 
                    OMPI_NAME_ARGS(mca_oob_name_self), 
                    OMPI_NAME_ARGS(peer->peer_name), 
                    ompi_errno);
                mca_oob_tcp_peer_close(peer);
                return false;
            }
        }

        msg->msg_rc += rc;
        do {/* while there is still more iovecs to write */
            if(rc < (int)msg->msg_rwptr->iov_len) {
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
    /* has entire header been received */
    if(msg->msg_rwptr == msg->msg_rwiov) {
        if(mca_oob_tcp_msg_recv(msg, peer) == false)
            return false;

        /* allocate a buffer for the receive */
        MCA_OOB_TCP_HDR_NTOH(&msg->msg_hdr);
        if(msg->msg_hdr.msg_size > 0) {
             msg->msg_rwbuf = malloc(msg->msg_hdr.msg_size);
             if(NULL == msg->msg_rwbuf) {
                 ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_msg_recv_handler: malloc(%d) failed\n", 
                     OMPI_NAME_ARGS(mca_oob_name_self),
                     OMPI_NAME_ARGS(peer->peer_name),
                     msg->msg_hdr.msg_size);
                 mca_oob_tcp_peer_close(peer);
                 return false;
             }
             msg->msg_rwiov[1].iov_base = msg->msg_rwbuf;
             msg->msg_rwiov[1].iov_len = msg->msg_hdr.msg_size;
             msg->msg_rwnum = 1;
        }
    }

    /* do the right thing based on the message type */
    switch(msg->msg_hdr.msg_type)  {
        case MCA_OOB_TCP_IDENT:
            /* done - there is nothing else to receive */
            return true; 
        case MCA_OOB_TCP_PING:
            /* done - there is nothing else to receive */
            return true;
        case MCA_OOB_TCP_DATA:
            /* finish receiving message */
            return mca_oob_tcp_msg_recv(msg, peer);
        default:
            return true;
    }
}

/**
 * Process the current iovec
 */

static bool mca_oob_tcp_msg_recv(mca_oob_tcp_msg_t* msg, mca_oob_tcp_peer_t* peer)
{
    int rc;
    while(1) {
        rc = readv(peer->peer_sd, msg->msg_rwptr, msg->msg_rwnum);
        if(rc < 0) {
            if(ompi_errno == EINTR)
                continue;
            else if (ompi_errno == EAGAIN)
                return false;
            else {
                ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_msg_recv: readv failed with ompi_errno=%d", 
                    OMPI_NAME_ARGS(mca_oob_name_self),
                    OMPI_NAME_ARGS(peer->peer_name),
                    ompi_errno);
                mca_oob_tcp_peer_close(peer);
                return false;
            }
        } else if (rc == 0)  {
            if(mca_oob_tcp_component.tcp_debug > 3) {
                ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_msg_recv: peer closed connection", 
                   OMPI_NAME_ARGS(mca_oob_name_self),
                   OMPI_NAME_ARGS(peer->peer_name),
                   ompi_errno);
            }
            mca_oob_tcp_peer_close(peer);
            return false;
        }

        do {
            if(rc < (int)msg->msg_rwptr->iov_len) {
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

/**
 * Process a completed message.
 */

void mca_oob_tcp_msg_recv_complete(mca_oob_tcp_msg_t* msg, mca_oob_tcp_peer_t* peer)
{
    switch(msg->msg_hdr.msg_type)  {
        case MCA_OOB_TCP_IDENT:
            mca_oob_tcp_msg_ident(msg,peer);
            break;
        case MCA_OOB_TCP_PING:
            mca_oob_tcp_msg_ping(msg,peer);
            break;
        case MCA_OOB_TCP_DATA:
            mca_oob_tcp_msg_data(msg,peer);
            break;
        default:
            ompi_output(0, "[%d,%d,%d] mca_oob_tcp_msg_recv_complete: invalid message type: %d\n",
                 OMPI_NAME_ARGS(mca_oob_name_self), msg->msg_hdr.msg_type);
            MCA_OOB_TCP_MSG_RETURN(msg);
            break;
    }
}

/**
 * Process an ident message.
 */

static void mca_oob_tcp_msg_ident(mca_oob_tcp_msg_t* msg, mca_oob_tcp_peer_t* peer)
{
    ompi_process_name_t src = msg->msg_hdr.msg_src;
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    if(ompi_name_server.compare(OMPI_NS_CMP_ALL, &peer->peer_name, &src) != 0) {
        ompi_rb_tree_delete(&mca_oob_tcp_component.tcp_peer_tree, &peer->peer_name);
        peer->peer_name = src;
        ompi_rb_tree_insert(&mca_oob_tcp_component.tcp_peer_tree, &peer->peer_name, peer);
    }
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
}


/**
 * Process a ping message.
 */

static void mca_oob_tcp_msg_ping(mca_oob_tcp_msg_t* msg, mca_oob_tcp_peer_t* peer)
{
    /* for now - we dont do anything - may want to send back a response at some poing */
}


/*
 * Progress a completed recv:
 * (1) signal a posted recv as complete
 * (2) queue an unexpected message in the recv list
 */

static void mca_oob_tcp_msg_data(mca_oob_tcp_msg_t* msg, mca_oob_tcp_peer_t* peer)
{
    /* attempt to match unexpected message to a posted recv */
    mca_oob_tcp_msg_t* post;
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);
    post = mca_oob_tcp_msg_match_post(&peer->peer_name, msg->msg_hdr.msg_tag,true);
    if(NULL != post) {
                                                                                                                          
        if(post->msg_flags & MCA_OOB_ALLOC) {
                                                                                                                          
            /* set the users iovec struct to point to pre-allocated buffer */
            if(NULL == post->msg_uiov || 0 == post->msg_ucnt) {
                post->msg_rc = OMPI_ERR_BAD_PARAM;
            } else {
                /* first iovec of recv message contains the header -
                 * subsequent contain user data
                */
                post->msg_uiov[0].iov_base = msg->msg_rwbuf;
                post->msg_uiov[0].iov_len = msg->msg_hdr.msg_size;
		post->msg_rc = msg->msg_hdr.msg_size;
                msg->msg_rwbuf = NULL;
            }
                                                                                                                          
        } else {
                                                                                                                          
            /* copy msg data into posted recv */
            post->msg_rc = mca_oob_tcp_msg_copy(msg, post->msg_uiov, post->msg_ucnt);
            if(post->msg_flags & MCA_OOB_TRUNC) {
                 int i, size = 0;
                 for(i=1; i<msg->msg_rwcnt+1; i++)
                     size += msg->msg_rwiov[i].iov_len;
                 post->msg_rc = size;
            }
        }
                                                                                                                          
        if(post->msg_flags & MCA_OOB_PEEK) {
            /* will need message for actual receive */
            ompi_list_append(&mca_oob_tcp_component.tcp_msg_recv, &msg->super);
        } else {
            MCA_OOB_TCP_MSG_RETURN(msg);
        }
        mca_oob_tcp_component.tcp_match_count++;
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);

        mca_oob_tcp_msg_complete(post, &peer->peer_name);

        OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);
        if(--mca_oob_tcp_component.tcp_match_count == 0)
            ompi_condition_signal(&mca_oob_tcp_component.tcp_match_cond);
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);

    } else {
        ompi_list_append(&mca_oob_tcp_component.tcp_msg_recv, (ompi_list_item_t*)msg);
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
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
    struct iovec *src = msg->msg_rwiov+1;
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
                if(++src_cnt == msg->msg_rwcnt)
                    return rc;
                src++;
                src_ptr = (unsigned char*)src->iov_base;
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

        if((0 == ompi_name_server.compare(OMPI_NS_CMP_ALL, name,MCA_OOB_NAME_ANY) ||
	    (0 == ompi_name_server.compare(OMPI_NS_CMP_ALL, name, &msg->msg_peer)))) {
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

        if((0 == ompi_name_server.compare(OMPI_NS_CMP_ALL, &msg->msg_peer,MCA_OOB_NAME_ANY) ||
	    (0 == ompi_name_server.compare(OMPI_NS_CMP_ALL, &msg->msg_peer,name)))) {
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




