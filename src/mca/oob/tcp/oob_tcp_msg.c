#include "oob_tcp.h"
#include "oob_tcp_msg.h"


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


/**
 *  Wait for a msg to complete.
 *  @param  msg (IN)   Message to wait on.
 *  @param  size (OUT) Number of bytes delivered.
 *  @retval OMPI_SUCCESS or error code on failure.
 */

int mca_oob_tcp_msg_wait(mca_oob_tcp_msg_t* msg, int* size)
{
    int rc = OMPI_SUCCESS;
    ompi_mutex_lock(&msg->msg_lock);
    while(msg->msg_complete == false)
        ompi_condition_wait(&msg->msg_condition, &msg->msg_lock);
    ompi_mutex_unlock(&msg->msg_lock);
    *size = msg->msg_state;
    MCA_OOB_TCP_MSG_RETURN(msg);
    return rc;
}

/**
 *  Signal that a message has completed.
 *  @param  msg (IN)   Message to wait on.
 *  @param peer (IN) the peer of the message
 *  @retval OMPI_SUCCESS or error code on failure.
 */
int mca_oob_tcp_msg_complete(mca_oob_tcp_msg_t* msg, struct mca_oob_tcp_peer_t * peer)
{
    ompi_mutex_lock(&msg->msg_lock);
    msg->msg_complete = true;
    if(NULL != msg->msg_cbfunc) {
        msg->msg_cbfunc(msg->msg_state, &peer->peer_name, msg->msg_iov, msg->msg_count, msg->msg_cbdata);
        ompi_mutex_unlock(&msg->msg_lock);
        MCA_OOB_TCP_MSG_RETURN(msg);
    } else {
        ompi_condition_broadcast(&msg->msg_condition);
        ompi_mutex_unlock(&msg->msg_lock);
    }
    return OMPI_SUCCESS;
}

/**
 * The function that actually sends the data!
 * @param msg a pointer to the message to send
 * @param peer the peer we are sending to
 * @retval true if the entire message has been sent
 * @retval false if the entire message has not been sent
 */
bool mca_oob_tcp_msg_send_handler(mca_oob_tcp_msg_t* msg, mca_oob_tcp_peer_t * peer)
{
    int rc;
    while(1) {
        rc = writev(peer->peer_sd, msg->msg_rwptr, msg->msg_rwcnt);
        if(rc <= 0) {
            if(errno == EINTR)
                continue;
            else if (errno == EAGAIN)
                return false;
            else {
                close(peer->peer_sd);
                peer->peer_state = MCA_OOB_TCP_CLOSED;
                return false;
            }
        }
        msg->msg_state += rc;
        do {/* while there is still more iovects to write */
            if(rc < msg->msg_rwptr->iov_len) {
                msg->msg_rwptr->iov_len -= rc;
                msg->msg_rwptr->iov_base = (void *) ((char *) msg->msg_rwptr->iov_base + rc);
                break;
            } else {
                rc -= msg->msg_rwptr->iov_len;
                (msg->msg_rwcnt)--;
                (msg->msg_rwptr)++;
                if(0 == msg->msg_rwcnt) {
                    ompi_list_remove_item(&peer->peer_send_queue, (ompi_list_item_t *) msg);
                    mca_oob_tcp_msg_complete(msg, peer);
                    return true;
                }
            }
        } while(msg->msg_rwcnt);
    }
}

/**
 * Actually recieves the data
 *
 * @param msg the message to be recieved into
 * @param peer the peer to recieve from
 * @retval true if the whole message was recieved
 * @retval false if the whole message was not recieved
 */
bool mca_oob_tcp_msg_recv_handler(mca_oob_tcp_msg_t* msg, mca_oob_tcp_peer_t * peer)
{
    int rc;
    while(1) {
        rc = readv(peer->peer_sd, msg->msg_rwptr, msg->msg_rwcnt);
        if(rc <= 0) {
            if(errno == EINTR)
                continue;
            else if (errno == EAGAIN)
                return false;
            else {
                close(peer->peer_sd);
                peer->peer_state = MCA_OOB_TCP_CLOSED;
                return false;
            }
        }
        msg->msg_state += rc;
        do {
            if(rc < msg->msg_rwptr->iov_len) {
                msg->msg_rwptr->iov_len -= rc;
                msg->msg_rwptr->iov_base = (void *) ((char *) msg->msg_rwptr->iov_base + rc);
                break;
            } else {
                rc -= msg->msg_rwptr->iov_len;
                (msg->msg_rwcnt)--;
                (msg->msg_rwptr)++;
                if(0 == msg->msg_rwcnt) {
                    mca_oob_tcp_msg_complete(msg, peer);
                    return true;
                }
            }
        } while(msg->msg_rwcnt);
    }
}

