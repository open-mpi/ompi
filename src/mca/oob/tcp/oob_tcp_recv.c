#include "mca/oob/tcp/oob_tcp.h"

/*
 * Similiar to unix readv(2)
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to the number of bytes provided in the
 *                     iovec array without removing the message from the queue.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */
int mca_oob_tcp_recv(ompi_process_name_t* peer, const struct iovec *iov, int count, int flags)
{
    mca_oob_tcp_msg_t * msg = (mca_oob_tcp_msg_t *) 
                              ompi_list_get_first(&mca_oob_tcp_module.tcp_msg_recv);
    int i, amount, read, size = 0;
    char * base;
    for(i = 0; i < count; i++) {
        size += iov[i].iov_len;
    }
    /* lock the tcp struct */
    OMPI_THREAD_LOCK(&mca_oob_tcp_module.tcp_lock);
    /* check to see if a matching recieve is on the list */
    while(NULL != msg) {
        if(MCA_OOB_BASE_ANY == peer || 
           (0 == memcmp(peer, &msg->msg_peer, sizeof(ompi_process_name_t)))) {
            /* if we are just doing peek, all we need to do is match the peer name,
             * nothing else. */
            if(MCA_OOB_PEEK & flags) {
                base = (char *) msg->msg_iov[1].iov_base;
                size = msg->msg_iov[i].iov_len;
                read = 0;
                for(i = 0; i < count; i++) {
                    amount = ((iov[i].iov_len < size) ? iov[i].iov_len : size); 
                    memcpy(iov[i].iov_base, base, amount);
                    size -= amount;
                    base +=amount;
                    read += amount;
                }
                OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
                return read;
            }
            /* what are we going to define as matching? we don't know the number
             * of iovecs we got off the network, we only know the total size
             * For now, we wil just use the size == the size we have available */    
            if(size == msg->msg_size) {
                /* match */
                base = (char *) msg->msg_iov[1].iov_base;
                size = msg->msg_iov[i].iov_len;
                read = 0;
                for(i = 0; i < count; i++) {
                    memcpy(iov[i].iov_base, base, iov[i].iov_len);
                    base +=amount;
                    read += amount;
                }
                ompi_list_remove_item(&mca_oob_tcp_module.tcp_msg_recv, 
                                      (ompi_list_item_t *) msg);
                MCA_OOB_TCP_MSG_RETURN(msg);
                OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
                return read;
            }
            msg = (mca_oob_tcp_msg_t *) ompi_list_get_next(msg);
        } 
    }
    /* the message has not already been recieved. So we add it to the recieve queue */
    MCA_OOB_TCP_MSG_ALLOC(msg, i);
    if(NULL == msg) {
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
        return i;
    }
    /* fill in the struct */
    msg->msg_state = 0;
    msg->msg_size = size;
    msg->msg_user = iov;
    msg->msg_iov = NULL;
    msg->msg_rwcnt = msg->msg_count = count;
    msg->msg_cbfunc = NULL;
    msg->msg_complete = false;
    if(0 == memcmp(MCA_OOB_BASE_ANY, peer, sizeof(ompi_process_name_t))) {
        msg->msg_peer = MCA_OOB_BASE_ANY;
    } else {
        mca_oob_tcp_peer_t * other = mca_oob_tcp_peer_lookup(peer, false);
        msg->msg_peer = &other->peer_name;
    }
    /* add to list */
    ompi_list_append(&mca_oob_tcp_module.tcp_msg_recv, (ompi_list_item_t *) msg);
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
    /* wait for the recieve to complete */
    mca_oob_tcp_msg_wait(msg, &read);
    return read;
}

/*
 * Non-blocking version of mca_oob_recv().
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
 * @param cbfunc (IN)  Callback function on recv completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */
int mca_oob_tcp_recv_nb(ompi_process_name_t* peer, const struct iovec* iov, int count,
                        int flags, mca_oob_callback_fn_t cbfunc, void* cbdata)
{
    mca_oob_tcp_msg_t * msg = (mca_oob_tcp_msg_t *)
                              ompi_list_get_first(&mca_oob_tcp_module.tcp_msg_recv);
    int i, amount, read, size = 0;
    char * base;
    for(i = 0; i < count; i++) {
        size += iov[i].iov_len;
    }
    /* lock the tcp struct */
    OMPI_THREAD_LOCK(&mca_oob_tcp_module.tcp_lock);
    /* check to see if a matching recieve is on the list */
    while(NULL != msg) {
        if(MCA_OOB_BASE_ANY == peer ||
           (0 == memcmp(peer, &msg->msg_peer, sizeof(ompi_process_name_t)))) {
            /* if we are just doing peek, all we need to do is match the peer name,
             * nothing else. */
            if(MCA_OOB_PEEK & flags) {
                base = (char *) msg->msg_iov[1].iov_base;
                size = msg->msg_iov[i].iov_len;
                read = 0;
                for(i = 0; i < count; i++) {
                    amount = ((iov[i].iov_len < size) ? iov[i].iov_len : size);
                    memcpy(iov[i].iov_base, base, amount);
                    size -= amount;
                    base +=amount;
                    read += amount;
                }
                cbfunc(read, peer, iov, count, cbdata);
                OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
                return read;
            }
            /* what are we going to define as matching? we don't know the number
             * of iovecs we got off the network, we only know the total size
             * For now, we wil just use the size == the size we have available */
            if(size == msg->msg_size) {
                /* match */
                base = (char *) msg->msg_iov[1].iov_base;
                size = msg->msg_iov[i].iov_len;
                read = 0;
                for(i = 0; i < count; i++) {
                    memcpy(iov[i].iov_base, base, iov[i].iov_len);
                    base +=amount;
                    read += amount;
                }
                ompi_list_remove_item(&mca_oob_tcp_module.tcp_msg_recv,
                                      (ompi_list_item_t *) msg);
                MCA_OOB_TCP_MSG_RETURN(msg);
                OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
                cbfunc(read, peer, iov, count, cbdata);
                return read;
            }
            msg = (mca_oob_tcp_msg_t *) ompi_list_get_next(msg);
        }
    }
    /* the message has not already been recieved. So we add it to the recieve queue */
    MCA_OOB_TCP_MSG_ALLOC(msg, i);
    if(NULL == msg) {
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
        return i;
    }
    /* fill in the struct */
    msg->msg_state = 0;
    msg->msg_size = size;
    msg->msg_user = iov;
    msg->msg_iov = NULL;
    msg->msg_rwcnt = msg->msg_count = count;
    msg->msg_cbfunc = cbfunc;
    msg->msg_cbdata = cbdata;
    if(0 == memcmp(MCA_OOB_BASE_ANY, peer, sizeof(ompi_process_name_t))) {
        msg->msg_peer = MCA_OOB_BASE_ANY;
    } else {
        mca_oob_tcp_peer_t * other = mca_oob_tcp_peer_lookup(peer, false);
        msg->msg_peer = &other->peer_name;
    }
    msg->msg_complete = false;
    /* add to list */
    ompi_list_append(&mca_oob_tcp_module.tcp_msg_recv, (ompi_list_item_t *) msg);
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
    return OMPI_SUCCESS;
}

