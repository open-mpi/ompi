#include <unistd.h>
#include <fcntl.h>
#include <sys/uio.h>
#include <sys/types.h>
#include <sys/errno.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "util/output.h"
#include "mca/oob/tcp/oob_tcp_peer.h"
#include "mca/gpr/base/base.h"
#include "mca/gpr/gpr.h"


static int  mca_oob_tcp_peer_start_connect(mca_oob_tcp_peer_t* peer);
static int  mca_oob_tcp_peer_event_init(mca_oob_tcp_peer_t* peer);
static void mca_oob_tcp_peer_connected(mca_oob_tcp_peer_t* peer);
static void mca_oob_tcp_peer_construct(mca_oob_tcp_peer_t* peer);
static void mca_oob_tcp_peer_destruct(mca_oob_tcp_peer_t* peer);
static int  mca_oob_tcp_peer_send_connect_ack(mca_oob_tcp_peer_t* peer);
static int  mca_oob_tcp_peer_recv_connect_ack(mca_oob_tcp_peer_t* peer);
static int  mca_oob_tcp_peer_recv_blocking(mca_oob_tcp_peer_t* peer, void* data, size_t size);
static int  mca_oob_tcp_peer_send_blocking(mca_oob_tcp_peer_t* peer, void* data, size_t size);
static void mca_oob_tcp_peer_recv_handler(int sd, short flags, void* user);
static void mca_oob_tcp_peer_send_handler(int sd, short flags, void* user);
static void mca_oob_tcp_peer_dump(mca_oob_tcp_peer_t* peer, const char* msg);


OBJ_CLASS_INSTANCE(
    mca_oob_tcp_peer_t,
    ompi_list_item_t,
    mca_oob_tcp_peer_construct,
    mca_oob_tcp_peer_destruct);


/*
 * This is the constructor function for the mca_oob_tcp_peer
 * struct. Note that this function and OBJ_NEW should NEVER
 * be called directly. Instead, use mca_oob_tcp_add_peer
 *
 * @param peer a pointer to the mca_oob_tcp_peer_t struct to be initialized
 * @retval none
 */
static void mca_oob_tcp_peer_construct(mca_oob_tcp_peer_t* peer) 
{ 
    OBJ_CONSTRUCT(&(peer->peer_send_queue), ompi_list_t);
    OBJ_CONSTRUCT(&(peer->peer_lock), ompi_mutex_t);
}

/*
 * This is the destructor function for the mca_oob_tcp_peer
 * struct. Note that this function and OBJ_RELEASE should NEVER
 * be called directly. Instead, use mca_oob_tcp_del_peer
 *
 * @param peer a pointer to the mca_oob_tcp_peer_t struct to be destroyed
 * @retval none
 */
static void mca_oob_tcp_peer_destruct(mca_oob_tcp_peer_t * peer)
{
    mca_oob_tcp_peer_close(peer); 
    OBJ_DESTRUCT(&(peer->peer_send_queue));
    OBJ_DESTRUCT(&(peer->peer_lock));
}

/*
 * Initialize events to be used by the peer instance for TCP select/poll callbacks.
 */
static int mca_oob_tcp_peer_event_init(mca_oob_tcp_peer_t* peer)
{
    ompi_event_set(
        &peer->peer_recv_event,
        peer->peer_sd,
        OMPI_EV_READ|OMPI_EV_PERSIST,
        mca_oob_tcp_peer_recv_handler,
        peer);
    ompi_event_set(
        &peer->peer_send_event,
        peer->peer_sd,
        OMPI_EV_WRITE|OMPI_EV_PERSIST,
        mca_oob_tcp_peer_send_handler,
        peer);
    return OMPI_SUCCESS;
}

/*
 *  Initiate the appropriate action based on the state of the connection
 *  to the peer.
 *
 */
int mca_oob_tcp_peer_send(mca_oob_tcp_peer_t* peer, mca_oob_tcp_msg_t* msg)
{
    int rc = OMPI_SUCCESS;
    OMPI_THREAD_LOCK(&peer->peer_lock);
    switch(peer->peer_state) {
    case MCA_OOB_TCP_CONNECTING:
    case MCA_OOB_TCP_CONNECT_ACK:
    case MCA_OOB_TCP_CLOSED:
        /*
         * queue the message and start the connection to the peer
         */
        ompi_list_append(&peer->peer_send_queue, (ompi_list_item_t*)msg);
        if(peer->peer_state == MCA_OOB_TCP_CLOSED)
            rc = mca_oob_tcp_peer_start_connect(peer);
        break;
    case MCA_OOB_TCP_FAILED:
        rc = OMPI_ERR_UNREACH;
        break;
    case MCA_OOB_TCP_CONNECTED:
        /*
         * start the message and queue if not completed 
         */
        if (NULL != peer->peer_send_msg) {
            ompi_list_append(&peer->peer_send_queue, (ompi_list_item_t*)msg);
        } else {
            /*if the send does not complete */
            if(!mca_oob_tcp_msg_send_handler(msg, peer)) {
                peer->peer_send_msg = msg;
                ompi_event_add(&peer->peer_send_event, 0);
            } else {
                mca_oob_tcp_msg_complete(msg, &peer->peer_name);
            }
        }
        break;
    }
    OMPI_THREAD_UNLOCK(&peer->peer_lock);
    return rc;
}

/*
 * Lookup a peer by name, create one if it doesn't exist.
 * @param name  Peers globally unique identifier.
 * @param get_lock says whether the function should get the main tcp lock or not.
 *              this should be true unless the caller already owns the lock.
 * @retval      Pointer to the newly created struture or NULL on error.
 */
mca_oob_tcp_peer_t * mca_oob_tcp_peer_lookup(ompi_process_name_t* name, bool get_lock)
{
    int rc;
    mca_oob_tcp_peer_t * peer, * old;

    if(get_lock) {
        OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    }
    peer = (mca_oob_tcp_peer_t*)ompi_rb_tree_find(&mca_oob_tcp_component.tcp_peer_tree,
           (ompi_process_name_t *)  name);
    if(NULL != peer) {
        if(get_lock) {
            OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        }
        return peer;
    }

    MCA_OOB_TCP_PEER_ALLOC(peer, rc);
    if(NULL == peer) {
        if(get_lock) {
            OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        }
        return NULL;
    }

    /* initialize peer state */
    peer->peer_name = *name;
    peer->peer_sd = -1;
    peer->peer_state = MCA_OOB_TCP_CLOSED;
    peer->peer_recv_msg = NULL;
    peer->peer_send_msg = NULL;
    peer->peer_retries = 0;

    if(OMPI_SUCCESS != ompi_rb_tree_insert(&mca_oob_tcp_component.tcp_peer_tree, &peer->peer_name, peer)) {
        MCA_OOB_TCP_PEER_RETURN(peer);
        if(get_lock) {
            OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        }
        return NULL;
    }

    /* if the peer list is over the maximum size, remove one unsed peer */
    ompi_list_prepend(&mca_oob_tcp_component.tcp_peer_list, (ompi_list_item_t *) peer);
    if(ompi_list_get_size(&mca_oob_tcp_component.tcp_peer_list) > 
        mca_oob_tcp_component.tcp_peer_limit) {
        old = (mca_oob_tcp_peer_t *) 
              ompi_list_get_last(&mca_oob_tcp_component.tcp_peer_list);
        while(1) {
            if(0 == ompi_list_get_size(&(old->peer_send_queue)) &&
               NULL == peer->peer_recv_msg) { 
                ompi_list_remove_item(&mca_oob_tcp_component.tcp_peer_list, 
                                      (ompi_list_item_t *) old);
                MCA_OOB_TCP_PEER_RETURN(old);
                break;
            } else {
                old = (mca_oob_tcp_peer_t *) ompi_list_get_prev(old);
                if(ompi_list_get_begin(&mca_oob_tcp_component.tcp_peer_list) == (ompi_list_item_t*)old) {
                    /* we tried, but we couldn't find one that was valid to get rid
                     * of. Oh well. */
                    break;
                }
            }
        }
    }
    if(get_lock) {
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
    }
    return peer;
}

/*
 *  Start a connection to the peer. This will likely not complete,
 *  as the socket is set to non-blocking, so register for event
 *  notification of connect completion. On connection we send
 *  our globally unique process identifier to the peer and wait for
 *  the peers response.
 */
                                                                                                                                 
static int mca_oob_tcp_peer_start_connect(mca_oob_tcp_peer_t* peer)
{
    int rc,flags;
    peer->peer_sd = socket(AF_INET, SOCK_STREAM, 0);
    if (peer->peer_sd < 0) {
        peer->peer_retries++;
        return OMPI_ERR_UNREACH;
    }

    /* setup event callbacks */
    mca_oob_tcp_peer_event_init(peer);

    /* setup the socket as non-blocking */
    if((flags = fcntl(peer->peer_sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "mca_oob_tcp_peer_connect: fcntl(F_GETFL) failed with errno=%d\n", errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(peer->peer_sd, F_SETFL, flags) < 0)
            ompi_output(0, "mca_oob_tcp_peer_connect: fcntl(F_SETFL) failed with errno=%d\n", errno);
    }

    /* resolve the peer address */
    if ((rc = mca_oob_tcp_peer_name_lookup(peer)) != OMPI_SUCCESS) {
        mca_oob_tcp_peer_close(peer);
        return OMPI_ERR_UNREACH;
    }

    /* start the connect - will likely fail with EINPROGRESS */
    if(connect(peer->peer_sd, (struct sockaddr*)&(peer->peer_addr), sizeof(peer->peer_addr)) < 0) {
        /* non-blocking so wait for completion */
        if(errno == EINPROGRESS) {
            peer->peer_state = MCA_OOB_TCP_CONNECTING;
            ompi_event_add(&peer->peer_send_event, 0);
            return OMPI_SUCCESS;
        }
        ompi_output(0, "mca_oob_tcp_msg_peer_start_connect: unable to connect to peer. errno=%d", errno);
        mca_oob_tcp_peer_close(peer);
        return OMPI_ERR_UNREACH;
    }

    /* send our globally unique process identifier to the peer */
    if((rc = mca_oob_tcp_peer_send_connect_ack(peer)) == OMPI_SUCCESS) {
        peer->peer_state = MCA_OOB_TCP_CONNECT_ACK;
        ompi_event_add(&peer->peer_recv_event, 0);
    } else {
        ompi_output(0, "mca_oob_tcp_peer_start_connect: unable to send connect ack to peer. errno=%d", errno);
        mca_oob_tcp_peer_close(peer);
    }
    return rc;
}


/*
 * Check the status of the connection. If the connection failed, will retry
 * later. Otherwise, send this processes identifier to the peer on the
 * newly connected socket.
 */
static void mca_oob_tcp_peer_complete_connect(mca_oob_tcp_peer_t* peer)
{
    int so_error = 0;
    ompi_socklen_t so_length = sizeof(so_error);

    /* unregister from receiving event notifications */
    ompi_event_del(&peer->peer_send_event);

    /* check connect completion status */
    if(getsockopt(peer->peer_sd, SOL_SOCKET, SO_ERROR, &so_error, &so_length) < 0) {
        ompi_output(0, "mca_oob_tcp_peer_complete_connect: getsockopt() failed with errno=%d\n", errno);
        mca_oob_tcp_peer_close(peer);
        return;
    }
    if(so_error == EINPROGRESS) {
        ompi_event_add(&peer->peer_send_event, 0);
        return;
    } else if (so_error == ECONNREFUSED) {
        if(peer->peer_retries++ > mca_oob_tcp_component.tcp_peer_retries) {
           ompi_output(0, "mca_oob_tcp_peer_complete_connect: unable to contact peer after %d retries\n", peer->peer_retries);
           mca_oob_tcp_peer_close(peer);
           return;
        }
        mca_oob_tcp_peer_close(peer);
        sleep(1);
        mca_oob_tcp_peer_start_connect(peer);
        return;
    } else if(so_error != 0) {
        ompi_output(0, "mca_oob_tcp_peer_complete_connect: connect() failed with errno=%d\n", so_error);
        mca_oob_tcp_peer_close(peer);
        return;
    }

    if(mca_oob_tcp_peer_send_connect_ack(peer) == OMPI_SUCCESS) {
        peer->peer_state = MCA_OOB_TCP_CONNECT_ACK;
        ompi_event_add(&peer->peer_recv_event, 0);
    } else {
         ompi_output(0, "mca_oob_tcp_peer_complete_connect: unable to send connect ack.");
        mca_oob_tcp_peer_close(peer);
    }
}

/*
 *  Setup peer state to reflect that connection has been established,
 *  and start any pending sends.
 */
static void mca_oob_tcp_peer_connected(mca_oob_tcp_peer_t* peer)
{
    peer->peer_state = MCA_OOB_TCP_CONNECTED;
    peer->peer_retries = 0;
    if(ompi_list_get_size(&peer->peer_send_queue) > 0) {
        if(NULL == peer->peer_send_msg)
            peer->peer_send_msg = (mca_oob_tcp_msg_t*)
                ompi_list_remove_first(&peer->peer_send_queue);
        ompi_event_add(&peer->peer_send_event, 0);
    }
}

/*
 * Remove any event registrations associated with the socket
 * and update the peer state to reflect the connection has
 * been closed.
 */
void mca_oob_tcp_peer_close(mca_oob_tcp_peer_t* peer)
{
    if(peer->peer_state != MCA_OOB_TCP_CLOSED &&
       peer->peer_sd >= 0) {
        ompi_event_del(&peer->peer_recv_event);
        ompi_event_del(&peer->peer_send_event);
        close(peer->peer_sd);
        peer->peer_sd = -1;
    }
    peer->peer_state = MCA_OOB_TCP_CLOSED;
    peer->peer_retries++;
}

/*
 * Send the globally unique identifier for this process to a peer on
 * a newly connected socket.
 */
static int mca_oob_tcp_peer_send_connect_ack(mca_oob_tcp_peer_t* peer)
{
    /* send process identifier to remote peer */
    ompi_process_name_t guid = mca_oob_name_self;
    OMPI_PROCESS_NAME_HTON(guid);
    if(mca_oob_tcp_peer_send_blocking( peer, &guid, sizeof(guid)) != sizeof(guid)) {
        return OMPI_ERR_UNREACH;
    }
    return OMPI_SUCCESS;
}

/*
 *  Receive the peers globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 */
static int mca_oob_tcp_peer_recv_connect_ack(mca_oob_tcp_peer_t* peer)
{
    ompi_process_name_t guid;
    if((mca_oob_tcp_peer_recv_blocking(peer, &guid, sizeof(ompi_process_name_t))) != sizeof(ompi_process_name_t)) {
        return OMPI_ERR_UNREACH;
    }
    OMPI_PROCESS_NAME_NTOH(guid);
                                                                                                            
    /* compare this to the expected values */
    if(memcmp(&peer->peer_name, &guid, sizeof(ompi_process_name_t)) != 0) {
        ompi_output(0, "mca_oob_tcp_peer_connect: received unexpected process identifier");
        mca_oob_tcp_peer_close(peer);
        return OMPI_ERR_UNREACH;
    }

    /* connected */
    mca_oob_tcp_peer_connected(peer);
#if OMPI_ENABLE_DEBUG
    mca_oob_tcp_peer_dump(peer, "connected");
#endif
    return OMPI_SUCCESS;
}

/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the peers endpoint.
 */
static int mca_oob_tcp_peer_recv_blocking(mca_oob_tcp_peer_t* peer, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    while(cnt < size) {
        int retval = recv(peer->peer_sd, ptr+cnt, size-cnt, 0);

        /* remote closed connection */
        if(retval == 0) {
            ompi_output(0, "mca_oob_tcp_peer_recv_blocking: remote connection closed");
            mca_oob_tcp_peer_close(peer);
            return -1;
        }

        /* socket is non-blocking so handle errors */
        if(retval < 0) {
            if(errno != EINTR && errno != EAGAIN && errno != EWOULDBLOCK) {
                ompi_output(0, "mca_oob_tcp_peer_recv_blocking: recv() failed with errno=%d\n",errno);
                mca_oob_tcp_peer_close(peer);
                return -1;
            }
            continue;
        }
        cnt += retval;
    }
    if((int)cnt == -1)
        ompi_output(0, "mca_oob_tcp_peer_recv_blocking: invalid cnt\n");
    return cnt;
}

/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the peers endpoint.
 */
static int mca_oob_tcp_peer_send_blocking(mca_oob_tcp_peer_t* peer, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    while(cnt < size) {
        int retval = send(peer->peer_sd, ptr+cnt, size-cnt, 0);
        if(retval < 0) {
            if(errno != EINTR && errno != EAGAIN && errno != EWOULDBLOCK) {
                ompi_output(0, "mca_oob_tcp_peer_send_blocking: send() failed with errno=%d\n",errno);
                mca_oob_tcp_peer_close(peer);
                return -1;
            }
            continue;
        }
        cnt += retval;
    }
    return cnt;
}

/*
 * Progress a completed recv:
 * (1) signal a posted recv as complete
 * (2) queue an unexpected message in the recv list
 */
static void mca_oob_tcp_peer_recv_progress(mca_oob_tcp_peer_t* peer, mca_oob_tcp_msg_t *msg)
{
    /* was this a posted recv? */
    if(msg->msg_type == MCA_OOB_TCP_POSTED) {

        if(msg->msg_flags & MCA_OOB_ALLOC) {
            /* set the users iovec struct to point to pre-allocated buffer */
            if(NULL == msg->msg_uiov || 0 == msg->msg_ucnt) {
                msg->msg_rc = OMPI_ERR_BAD_PARAM;
            } else {
                msg->msg_uiov[0].iov_base = msg->msg_rwiov->iov_base;
                msg->msg_uiov[0].iov_len = msg->msg_rwiov->iov_len;
                msg->msg_rwbuf = NULL;
                msg->msg_rc = msg->msg_rwiov->iov_len;
            }
         }
         mca_oob_tcp_msg_complete(msg, &peer->peer_name);

     } else {
        /* if not attempt to match unexpected message to a posted recv */
        mca_oob_tcp_msg_t* post;
        OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);
        post = mca_oob_tcp_msg_match_post(&peer->peer_name, msg->msg_hdr.msg_tag,true);
        if(NULL != post) {

            if(post->msg_flags & MCA_OOB_ALLOC) {

                /* set the users iovec struct to point to pre-allocated buffer */
                if(NULL == post->msg_uiov || 0 == post->msg_ucnt) {
                    post->msg_rc = OMPI_ERR_BAD_PARAM;
                } else {
                    post->msg_uiov[0].iov_base = msg->msg_rwiov->iov_base;
                    post->msg_uiov[0].iov_len = msg->msg_rwiov->iov_len;
                    msg->msg_rwbuf = NULL;
                    post->msg_rc = msg->msg_rwiov->iov_len;
                }

            } else {
       
                /* copy msg data into posted recv */
                post->msg_rc = mca_oob_tcp_msg_copy(msg, post->msg_uiov, post->msg_ucnt);
                if(post->msg_flags & MCA_OOB_TRUNC) {
                     int i, size = 0;
                     for(i=0; i<msg->msg_rwcnt; i++)
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
            OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
            mca_oob_tcp_msg_complete(post, &peer->peer_name);
           
        } else {
            ompi_list_append(&mca_oob_tcp_component.tcp_msg_recv, (ompi_list_item_t*)msg);
            OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
        }
    }
}


/*
 * Start receiving a new message. 
 * (1) receive header
 * (2) attempt to match posted receives
 * (3) if a posted receive is available - receive into users buffer
 * (4) otherwise, allocate a new message and buffer for receive
 */
static void mca_oob_tcp_peer_recv_start(mca_oob_tcp_peer_t* peer)
{
    mca_oob_tcp_msg_t* msg;
    mca_oob_tcp_hdr_t  hdr;
    uint32_t size;

    /* blocking receive of the message header */
    if(mca_oob_tcp_peer_recv_blocking(peer, &hdr, sizeof(hdr)) != sizeof(hdr))
        return;
    size = ntohl(hdr.msg_size);

    /* attempt to match posted receive 
     * however - dont match message w/ peek attribute, as we need to
     * queue the message anyway to match subsequent recv.
    */
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);
    msg = mca_oob_tcp_msg_match_post(&peer->peer_name, hdr.msg_tag, false);
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
    if(NULL != msg) {
        uint32_t posted_size = 0;
        int i;

        /* setup buffer for receive */
        for(i=0; i<msg->msg_ucnt; i++)
            posted_size += msg->msg_uiov[i].iov_len;

        /* allocate an additional buffer to receive entire message */
        if(msg->msg_flags & MCA_OOB_ALLOC) {
            msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg,1);
            msg->msg_rwbuf = malloc(size);
            msg->msg_rwiov[0].iov_base = msg->msg_rwbuf;
            msg->msg_rwiov[0].iov_len = size;
            msg->msg_rwcnt = msg->msg_rwnum = 1;
        } else if (posted_size < size) {
            uint32_t alloc_size = size - posted_size;
            msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg,msg->msg_ucnt+1);
            memcpy(msg->msg_rwiov, msg->msg_uiov, msg->msg_ucnt * sizeof(struct iovec));
            msg->msg_rwbuf = malloc(alloc_size);
            msg->msg_rwiov[msg->msg_ucnt].iov_base = msg->msg_rwbuf;
            msg->msg_rwiov[msg->msg_ucnt].iov_len = alloc_size;
            msg->msg_rwcnt = msg->msg_rwnum = msg->msg_ucnt+1;
        } else {
            msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg,msg->msg_ucnt);
            memcpy(msg->msg_rwiov, msg->msg_uiov, msg->msg_ucnt * sizeof(struct iovec));
            msg->msg_rwcnt = msg->msg_rwnum = msg->msg_ucnt;
        }

    } else {
        /* allocate a new message along with buffer */
        int rc;
        MCA_OOB_TCP_MSG_ALLOC(msg, rc);
        if(NULL == msg) {
            return;
        } 
        msg->msg_type = MCA_OOB_TCP_UNEXPECTED;
        msg->msg_rc = 0;
        msg->msg_flags = 0;
        msg->msg_peer = peer->peer_name;
        msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg,1);
        msg->msg_rwbuf = malloc(size);
        msg->msg_rwiov->iov_base = msg->msg_rwbuf;
        msg->msg_rwiov->iov_len = size;
        msg->msg_rwcnt = msg->msg_rwnum = 1;
    } 

    msg->msg_rwptr = msg->msg_rwiov;
    msg->msg_hdr = hdr;

    /* if receive of message data completed - queue the receive message */
    if(mca_oob_tcp_msg_recv_handler(msg, peer)) {
       mca_oob_tcp_peer_recv_progress(peer, msg);
    } else {
       /* continue processing until complete */
       peer->peer_recv_msg = msg;
    }
}


/*
 * Dispatch to the appropriate action routine based on the state
 * of the connection with the peer.
 */

static void mca_oob_tcp_peer_recv_handler(int sd, short flags, void* user)
{
    mca_oob_tcp_peer_t* peer = user;
    OMPI_THREAD_LOCK(&peer->peer_lock);
    switch(peer->peer_state) {
        case MCA_OOB_TCP_CONNECT_ACK: 
        {
            mca_oob_tcp_peer_recv_connect_ack(peer);
            break;
        }
        case MCA_OOB_TCP_CONNECTED: 
        {
            if(NULL == peer->peer_recv_msg) {
                mca_oob_tcp_peer_recv_start(peer);
            } else if (mca_oob_tcp_msg_recv_handler(peer->peer_recv_msg, peer)) {
               mca_oob_tcp_peer_recv_progress(peer, peer->peer_recv_msg);
               peer->peer_recv_msg = NULL;
            }
            break;
        }
        default: 
        {
            ompi_output(0, "mca_oob_tcp_peer_recv_handler: invalid socket state(%d)", peer->peer_state);
            mca_oob_tcp_peer_close(peer);
            break;
        }
    }
    OMPI_THREAD_UNLOCK(&peer->peer_lock);
}

/*
 * A file descriptor is available/ready for send. Check the state
 * of the socket and take the appropriate action.
 */
static void mca_oob_tcp_peer_send_handler(int sd, short flags, void* user)
{
    mca_oob_tcp_peer_t* peer = user;
    OMPI_THREAD_LOCK(&peer->peer_lock);
    switch(peer->peer_state) {
    case MCA_OOB_TCP_CONNECTING:
        mca_oob_tcp_peer_complete_connect(peer);
        break;
    case MCA_OOB_TCP_CONNECTED:
        {
        do {
            /* complete the current send */
            mca_oob_tcp_msg_t* msg = peer->peer_send_msg;
            if(mca_oob_tcp_msg_send_handler(msg, peer)) {
                mca_oob_tcp_msg_complete(msg, &peer->peer_name);
            } else {
                break;
            }

            /* if current completed - progress any pending sends */
            peer->peer_send_msg = (mca_oob_tcp_msg_t*)
                ompi_list_remove_first(&peer->peer_send_queue);
        } while (NULL != peer->peer_send_msg);
        
        /* if nothing else to do unregister for send event notifications */
        if(NULL == peer->peer_send_msg) {
            ompi_event_del(&peer->peer_send_event);
        }
        break;
        }
    default:
        ompi_output(0, "mca_oob_tcp_peer_send_handler: invalid connection state (%d)",
            peer->peer_state);
        ompi_event_del(&peer->peer_send_event);
        break;
    }
    OMPI_THREAD_UNLOCK(&peer->peer_lock);
}
                                                                                                                       

/*
 * Routine for debugging to print the connection state and socket options
 */
static void mca_oob_tcp_peer_dump(mca_oob_tcp_peer_t* peer, const char* msg)
{
    char src[64];
    char dst[64];
    char buff[255];
    int sndbuf,rcvbuf,nodelay,flags;
    struct sockaddr_in inaddr;
    ompi_socklen_t optlen;
    ompi_socklen_t addrlen = sizeof(struct sockaddr_in);
                                                                                                            
    getsockname(peer->peer_sd, (struct sockaddr*)&inaddr, &addrlen);
    sprintf(src, "%s", inet_ntoa(inaddr.sin_addr));
    getpeername(peer->peer_sd, (struct sockaddr*)&inaddr, &addrlen);
    sprintf(dst, "%s", inet_ntoa(inaddr.sin_addr));
                                                                                                            
    if((flags = fcntl(peer->peer_sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "mca_oob_tcp_peer_dump: fcntl(F_GETFL) failed with errno=%d\n", errno);
    }
                                                                                                            
#if defined(SO_SNDBUF)
    optlen = sizeof(sndbuf);
    if(getsockopt(peer->peer_sd, SOL_SOCKET, SO_SNDBUF, (char *)&sndbuf, &optlen) < 0) {
        ompi_output(0, "mca_oob_tcp_peer_dump: SO_SNDBUF option: errno %d\n", errno);
    }
#else
    sndbuf = -1;
#endif
#if defined(SO_RCVBUF)
    optlen = sizeof(rcvbuf);
    if(getsockopt(peer->peer_sd, SOL_SOCKET, SO_RCVBUF, (char *)&rcvbuf, &optlen) < 0) {
        ompi_output(0, "mca_oob_tcp_peer_dump: SO_RCVBUF option: errno %d\n", errno);
    }
#else
    rcvbuf = -1;
#endif
#if defined(TCP_NODELAY)
    optlen = sizeof(nodelay);
    if(getsockopt(peer->peer_sd, IPPROTO_TCP, TCP_NODELAY, &nodelay, &optlen) < 0) {
        ompi_output(0, "mca_oob_tcp_peer_dump: TCP_NODELAY option: errno %d\n", errno);
    }
#else
    nodelay = 0;
#endif
                                                                                                            
    sprintf(buff, "%s: %s - %s nodelay %d sndbuf %d rcvbuf %d flags %08x\n",
        msg, src, dst, nodelay, sndbuf, rcvbuf, flags);
    ompi_output(0, buff);
}


/*
 *  Accept incoming connection - if not already connected.
 */

bool mca_oob_tcp_peer_accept(mca_oob_tcp_peer_t* peer, int sd)
{
    OMPI_THREAD_LOCK(&peer->peer_lock);
    if ((peer->peer_state == MCA_OOB_TCP_CLOSED) ||
        (peer->peer_state != MCA_OOB_TCP_CONNECTED &&
         mca_oob_tcp_process_name_compare(&peer->peer_name, MCA_OOB_NAME_SELF) < 0)) {
        mca_oob_tcp_peer_close(peer);
        peer->peer_sd = sd;
        mca_oob_tcp_peer_event_init(peer);

        if(mca_oob_tcp_peer_send_connect_ack(peer) != OMPI_SUCCESS) {
            mca_oob_tcp_peer_close(peer);
            OMPI_THREAD_UNLOCK(&peer->peer_lock);
            return false;
        }
        ompi_event_add(&peer->peer_recv_event, 0);
        mca_oob_tcp_peer_connected(peer);
#if OMPI_ENABLE_DEBUG
        mca_oob_tcp_peer_dump(peer, "accepted");
#endif
        OMPI_THREAD_UNLOCK(&peer->peer_lock);
        return true;
    }
    OMPI_THREAD_UNLOCK(&peer->peer_lock);
    return false;
}


/*
 * resolve process name to an actual internet address.
 */

int mca_oob_tcp_peer_name_lookup(mca_oob_tcp_peer_t* peer)
{
    if(mca_oob_tcp_process_name_compare(&peer->peer_name, MCA_OOB_NAME_SEED) == 0) {
        peer->peer_addr = mca_oob_tcp_component.tcp_seed_addr;
        return OMPI_SUCCESS;
    } else {
        ompi_registry_value_t *item;
        ompi_list_t* items;
        char *keys[3];
        char *uri = NULL;

        /* lookup the name in the registry */
        keys[0] = "tcp";
        keys[1] = ompi_name_server.get_proc_name_string(&peer->peer_name);
        keys[2] = NULL;
        items = ompi_registry.get(OMPI_REGISTRY_AND, "oob", keys);
        if(items == NULL || ompi_list_get_size(items) == 0)
            return OMPI_ERR_UNREACH;

        /* unpack the results into a uri string */
        item = (ompi_registry_value_t*)ompi_list_remove_first(items);
        if((uri = item->object) == NULL) 
            return OMPI_ERR_UNREACH;

        /* validate the result */
        if(mca_oob_tcp_parse_uri(uri, &peer->peer_addr) != OMPI_SUCCESS) {
            OBJ_RELEASE(item);
            return OMPI_ERR_UNREACH;
        }
        OBJ_RELEASE(item);
        return OMPI_SUCCESS;
    }
}


