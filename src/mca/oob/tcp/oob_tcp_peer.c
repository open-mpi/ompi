/*
 * $HEADER$
 */
#include <unistd.h>
#include <fcntl.h>
#include <sys/uio.h>
#include <sys/types.h>
#include <sys/errno.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "util/output.h"
#include "mca/gpr/base/base.h"
#include "mca/gpr/gpr.h"
#include "oob_tcp.h"
#include "oob_tcp_peer.h"


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
static void mca_oob_tcp_peer_timer_handler(int sd, short flags, void* user);
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
    memset(&peer->peer_timer_event, 0, sizeof(peer->peer_timer_event));
    ompi_evtimer_set(&peer->peer_timer_event, mca_oob_tcp_peer_timer_handler, peer);
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
    memset(&peer->peer_recv_event, 0, sizeof(peer->peer_recv_event));
    memset(&peer->peer_send_event, 0, sizeof(peer->peer_send_event));
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
    case MCA_OOB_TCP_RESOLVE:
        /*
         * queue the message and attempt to resolve the peer address
         */
        ompi_list_append(&peer->peer_send_queue, (ompi_list_item_t*)msg);
        if(peer->peer_state == MCA_OOB_TCP_CLOSED) {
            peer->peer_state = MCA_OOB_TCP_RESOLVE;
            OMPI_THREAD_UNLOCK(&peer->peer_lock);
            return mca_oob_tcp_resolve(peer);
        }
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
 * @retval      Pointer to the newly created struture or NULL on error.
 */
mca_oob_tcp_peer_t * mca_oob_tcp_peer_lookup(const ompi_process_name_t* name)
{
    int rc;
    mca_oob_tcp_peer_t * peer, *old;

    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    peer = (mca_oob_tcp_peer_t*)ompi_rb_tree_find(&mca_oob_tcp_component.tcp_peer_tree,
           (ompi_process_name_t *)  name);
    if(NULL != peer) {
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        return peer;
    }

    /* allocate from free list */
    MCA_OOB_TCP_PEER_ALLOC(peer, rc);
    if(NULL == peer) {
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        return NULL;
    }

    /* initialize peer state */
    peer->peer_name = *name;
    peer->peer_addr = NULL;
    peer->peer_sd = -1;
    peer->peer_state = MCA_OOB_TCP_CLOSED;
    peer->peer_recv_msg = NULL;
    peer->peer_send_msg = NULL;
    peer->peer_retries = 0;

    /* add to lookup table */
    if(OMPI_SUCCESS != ompi_rb_tree_insert(&mca_oob_tcp_component.tcp_peer_tree, &peer->peer_name, peer)) {
        MCA_OOB_TCP_PEER_RETURN(peer);
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        return NULL;
    }

    /* if the peer list is over the maximum size, remove one unsed peer */
    ompi_list_prepend(&mca_oob_tcp_component.tcp_peer_list, (ompi_list_item_t *) peer);
    if(mca_oob_tcp_component.tcp_peer_limit > 0 &&
       ompi_list_get_size(&mca_oob_tcp_component.tcp_peer_list) > mca_oob_tcp_component.tcp_peer_limit) {
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
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
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
    struct sockaddr_in inaddr;

    /* create socket */
    peer->peer_state = MCA_OOB_TCP_CONNECTING;
    peer->peer_sd = socket(AF_INET, SOCK_STREAM, 0);
    if (peer->peer_sd < 0) {
        struct timeval tv = { 1,0 };
        ompi_output(0, 
            "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_start_connect: socket() failed with errno=%d\n",
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            errno);
        mca_oob_tcp_peer_close(peer);
        ompi_evtimer_add(&peer->peer_timer_event, &tv);
        return OMPI_ERR_UNREACH;
    }

    /* setup event callbacks */
    mca_oob_tcp_peer_event_init(peer);

    /* setup the socket as non-blocking */
    if((flags = fcntl(peer->peer_sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_connect: fcntl(F_GETFL) failed with errno=%d\n", 
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            errno);
    } else {
       flags |= O_NONBLOCK;
        if(fcntl(peer->peer_sd, F_SETFL, flags) < 0)
            ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_connect: fcntl(F_SETFL) failed with errno=%d\n", 
                OMPI_NAME_ARGS(mca_oob_name_self),
                OMPI_NAME_ARGS(peer->peer_name),
                errno);
    }

    /* pick an address in round-robin fashion from the list exported by the peer */
    if((rc = mca_oob_tcp_addr_get_next(peer->peer_addr, &inaddr)) != OMPI_SUCCESS) {
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_start_connect: mca_oob_tcp_addr_get_next failed with error=%d",
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            rc);
        return rc;
    }

    if(mca_oob_tcp_component.tcp_debug > 2) {
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_start_connect: connecting to: %s:%d\n",
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            inet_ntoa(inaddr.sin_addr),
            ntohs(inaddr.sin_port));
    }

    /* start the connect - will likely fail with EINPROGRESS */
    if(connect(peer->peer_sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        /* non-blocking so wait for completion */
        if(errno == EINPROGRESS) {
            ompi_event_add(&peer->peer_send_event, 0);
            return OMPI_SUCCESS;
        }
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_start_connect: connect failed with errno=%d",
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            errno);
        mca_oob_tcp_peer_close(peer);
        return OMPI_ERR_UNREACH;
    }

    /* send our globally unique process identifier to the peer */
    if((rc = mca_oob_tcp_peer_send_connect_ack(peer)) == OMPI_SUCCESS) {
        peer->peer_state = MCA_OOB_TCP_CONNECT_ACK;
        ompi_event_add(&peer->peer_recv_event, 0);
    } else {
        ompi_output(0, 
            "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_start_connect: "
            "mca_oob_tcp_peer_send_connect_ack failed with errno=%d",
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            rc);
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
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_complete_connect: getsockopt() failed with errno=%d\n", 
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            errno);
        mca_oob_tcp_peer_close(peer);
        return;
    }
    if(so_error == EINPROGRESS) {
        ompi_event_add(&peer->peer_send_event, 0);
        return;
    } else if (so_error == ECONNREFUSED) {
        struct timeval tv = { 1,0 };
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_complete_connect: "
            "connection refused - retrying\n", 
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name));
        mca_oob_tcp_peer_close(peer);
        if(peer->peer_retries > mca_oob_tcp_component.tcp_peer_retries) {
           return;
        }
        ompi_evtimer_add(&peer->peer_timer_event, &tv);
        return;
    } else if(so_error != 0) {
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_complete_connect: connect() failed with errno=%d\n", 
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            so_error);
        mca_oob_tcp_peer_close(peer);
        return;
    }

    if(mca_oob_tcp_peer_send_connect_ack(peer) == OMPI_SUCCESS) {
        peer->peer_state = MCA_OOB_TCP_CONNECT_ACK;
        ompi_event_add(&peer->peer_recv_event, 0);
    } else {
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_complete_connect: unable to send connect ack.",
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name));
        mca_oob_tcp_peer_close(peer);
    }
}

/*
 *  Setup peer state to reflect that connection has been established,
 *  and start any pending sends.
 */
static void mca_oob_tcp_peer_connected(mca_oob_tcp_peer_t* peer)
{
    ompi_event_del(&peer->peer_timer_event);
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
    if(mca_oob_tcp_component.tcp_debug >= 2) {
        ompi_output(0, "[%d,%d,%d] closing peer [%d,%d,%d] sd %d state %d\n",
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            peer->peer_sd,
            peer->peer_state);
    }

    /* giving up and cleanup any pending messages */
    if(peer->peer_retries++ > mca_oob_tcp_component.tcp_peer_retries) {
        mca_oob_tcp_msg_t *msg = peer->peer_send_msg;
        while(msg != NULL) {
            msg->msg_rc = OMPI_ERR_UNREACH;
            mca_oob_tcp_msg_complete(msg, &peer->peer_name);
            msg = (mca_oob_tcp_msg_t*)ompi_list_remove_first(&peer->peer_send_queue);
        }
        peer->peer_send_msg = NULL;
    }

    if (peer->peer_state != MCA_OOB_TCP_CLOSED && peer->peer_sd >= 0) {
        ompi_event_del(&peer->peer_recv_event);
        ompi_event_del(&peer->peer_send_event);
        close(peer->peer_sd);
        peer->peer_sd = -1;
    } 
      
    ompi_event_del(&peer->peer_timer_event);
    peer->peer_state = MCA_OOB_TCP_CLOSED;
}

/*
 * Send the globally unique identifier for this process to a peer on
 * a newly connected socket.
 */
static int mca_oob_tcp_peer_send_connect_ack(mca_oob_tcp_peer_t* peer)
{
    /* send process identifier of self and peer - note that we may 
     * have assigned the peer a unique process name - if it came up
     * without one.
    */
    ompi_process_name_t guid[2];
    guid[0] = mca_oob_name_self;
    guid[1] = peer->peer_name;
    OMPI_PROCESS_NAME_HTON(guid[0]);
    OMPI_PROCESS_NAME_HTON(guid[1]);
    if(mca_oob_tcp_peer_send_blocking(peer, guid, sizeof(guid)) != sizeof(guid)) {
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
    ompi_process_name_t guid[2];
    if((mca_oob_tcp_peer_recv_blocking(peer, guid, sizeof(guid))) != sizeof(guid)) {
        return OMPI_ERR_UNREACH;
    }
    OMPI_PROCESS_NAME_NTOH(guid[0]);
    OMPI_PROCESS_NAME_NTOH(guid[1]);
                                                                                                            
    /* compare the peers name to the expected value */
    if(memcmp(&peer->peer_name, &guid[0], sizeof(ompi_process_name_t)) != 0) {
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_connect: "
            "received unexpected process identifier [%d,%d,%d]\n",
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
            OMPI_NAME_ARGS(guid[0]));
        mca_oob_tcp_peer_close(peer);
        return OMPI_ERR_UNREACH;
    }

    /* if we have a wildcard name - use the name returned by the peer */
    if(mca_oob_tcp_process_name_compare(&mca_oob_name_self, &mca_oob_name_any) == 0) {
        mca_oob_name_self = guid[1];
    }

    /* connected */
    mca_oob_tcp_peer_connected(peer);
    if(mca_oob_tcp_component.tcp_debug > 2) {
        mca_oob_tcp_peer_dump(peer, "connected");
    }
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
            if(mca_oob_tcp_component.tcp_debug > 3) {
                ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_recv_blocking: "
                    "peer closed connection: peer state %d",
                    OMPI_NAME_ARGS(mca_oob_name_self),
                    OMPI_NAME_ARGS(peer->peer_name),
                    peer->peer_state);
            }
            mca_oob_tcp_peer_close(peer);
            return -1;
        }

        /* socket is non-blocking so handle errors */
        if(retval < 0) {
            if(errno != EINTR && errno != EAGAIN && errno != EWOULDBLOCK) {
                ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_recv_blocking: recv() failed with errno=%d\n",
                    OMPI_NAME_ARGS(mca_oob_name_self),
                    OMPI_NAME_ARGS(peer->peer_name),
                    errno);
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
                ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_send_blocking: send() failed with errno=%d\n",
                    OMPI_NAME_ARGS(mca_oob_name_self),
                    OMPI_NAME_ARGS(peer->peer_name),
                    errno);
                mca_oob_tcp_peer_close(peer);
                return -1;
            }
            continue;
        }
        cnt += retval;
    }
    return cnt;
}


int mca_oob_tcp_peer_send_ident(mca_oob_tcp_peer_t* peer)
{
    mca_oob_tcp_hdr_t hdr;
    if(peer->peer_state != MCA_OOB_TCP_CONNECTED)
        return OMPI_SUCCESS;
    hdr.msg_src = mca_oob_name_self;
    hdr.msg_dst = peer->peer_name;
    hdr.msg_type = MCA_OOB_TCP_IDENT;
    hdr.msg_size = 0;
    hdr.msg_tag = 0;
    MCA_OOB_TCP_HDR_HTON(&hdr);
    if(mca_oob_tcp_peer_send_blocking(peer, &hdr, sizeof(hdr)) != sizeof(hdr))
        return OMPI_ERR_UNREACH;
    return OMPI_SUCCESS;
}


static void mca_oob_tcp_peer_recv_ident(mca_oob_tcp_peer_t* peer, mca_oob_tcp_hdr_t* hdr)
{
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    ompi_rb_tree_delete(&mca_oob_tcp_component.tcp_peer_tree, &peer->peer_name);
    peer->peer_name = hdr->msg_src;
    ompi_rb_tree_insert(&mca_oob_tcp_component.tcp_peer_tree, &peer->peer_name, peer);
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
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
            /* allocate a new message and setup for recv */
            if(NULL == peer->peer_recv_msg) {
                int rc;
                mca_oob_tcp_msg_t* msg;
                MCA_OOB_TCP_MSG_ALLOC(msg, rc);
                if(NULL == msg) {
                    ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_recv_handler: unable to allocate recv message\n",
                        OMPI_NAME_ARGS(mca_oob_name_self),
                        OMPI_NAME_ARGS(peer->peer_name));
                    return;
                }
                msg->msg_type = MCA_OOB_TCP_UNEXPECTED;
                msg->msg_rc = 0;
                msg->msg_flags = 0;
                msg->msg_peer = peer->peer_name;
                msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg,2);
                msg->msg_rwbuf = NULL;
                msg->msg_rwiov->iov_base = msg->msg_rwbuf;
                msg->msg_rwiov->iov_len = 1;
                msg->msg_rwcnt = msg->msg_rwnum = 1;
                msg->msg_rwptr = msg->msg_rwiov;
                msg->msg_rwiov[0].iov_base = &msg->msg_hdr;
                msg->msg_rwiov[0].iov_len = sizeof(msg->msg_hdr);
                peer->peer_recv_msg = msg;
            }

            if (peer->peer_recv_msg && 
                mca_oob_tcp_msg_recv_handler(peer->peer_recv_msg, peer)) {
               mca_oob_tcp_msg_t* msg = peer->peer_recv_msg;
               peer->peer_recv_msg = NULL;
               OMPI_THREAD_UNLOCK(&peer->peer_lock);
               mca_oob_tcp_msg_recv_complete(msg, peer);
               return;
            }
            break;
        }
        default: 
        {
            ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_recv_handler: invalid socket state(%d)", 
                    mca_oob_name_self.cellid, mca_oob_name_self.jobid, mca_oob_name_self.vpid, 
                    peer->peer_name.cellid, peer->peer_name.jobid, peer->peer_name.vpid,
                    peer->peer_state);
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
        while(peer->peer_send_msg != NULL) {

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
        }
        
        /* if nothing else to do unregister for send event notifications */
        if(NULL == peer->peer_send_msg) {
            ompi_event_del(&peer->peer_send_event);
        }
        break;
        }
    default:
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_send_handler: invalid connection state (%d)",
            OMPI_NAME_ARGS(mca_oob_name_self),
            OMPI_NAME_ARGS(peer->peer_name),
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
                                                                                                            
    sprintf(buff, "[%d,%d,%d]-[%d,%d,%d] %s: %s - %s nodelay %d sndbuf %d rcvbuf %d flags %08x\n",
        OMPI_NAME_ARGS(mca_oob_name_self),
        OMPI_NAME_ARGS(peer->peer_name),
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
        (peer->peer_state == MCA_OOB_TCP_RESOLVE) ||
        (peer->peer_state != MCA_OOB_TCP_CONNECTED &&
         mca_oob_tcp_process_name_compare(&peer->peer_name, MCA_OOB_NAME_SELF) < 0)) {

        if(peer->peer_state != MCA_OOB_TCP_CLOSED) {
            mca_oob_tcp_peer_close(peer);
        }
        peer->peer_sd = sd;
        mca_oob_tcp_peer_event_init(peer);

        if(mca_oob_tcp_peer_send_connect_ack(peer) != OMPI_SUCCESS) {
            ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_peer_accept: mca_oob_tcp_peer_send_blocking failed\n");
            mca_oob_tcp_peer_close(peer);
            OMPI_THREAD_UNLOCK(&peer->peer_lock);
            return false;
        }

        mca_oob_tcp_peer_connected(peer);
        ompi_event_add(&peer->peer_recv_event, 0);
        if(mca_oob_tcp_component.tcp_debug > 2) {
            mca_oob_tcp_peer_dump(peer, "accepted");
        }
        OMPI_THREAD_UNLOCK(&peer->peer_lock);
        return true;
    }
    OMPI_THREAD_UNLOCK(&peer->peer_lock);
    return false;
}


/*
 * resolve process name to an actual internet address.
 */

void mca_oob_tcp_peer_resolved(mca_oob_tcp_peer_t* peer, mca_oob_tcp_addr_t* addr)
{
    OMPI_THREAD_LOCK(&peer->peer_lock);
    peer->peer_addr = addr;
    if((peer->peer_state == MCA_OOB_TCP_RESOLVE) ||
       (peer->peer_state == MCA_OOB_TCP_CLOSED && ompi_list_get_size(&peer->peer_send_queue))) {
        mca_oob_tcp_peer_start_connect(peer);
    }
    OMPI_THREAD_UNLOCK(&peer->peer_lock);
}

/*
 * Callback on timeout - retry connection attempt.
 */

static void mca_oob_tcp_peer_timer_handler(int sd, short flags, void* user)
{
    /* start the connection to the peer */
    mca_oob_tcp_peer_t* peer = (mca_oob_tcp_peer_t*)user;
    OMPI_THREAD_LOCK(&peer->peer_lock);
    if(peer->peer_state == MCA_OOB_TCP_CLOSED)
        mca_oob_tcp_peer_start_connect(peer);
    OMPI_THREAD_UNLOCK(&peer->peer_lock);
}

/*
 * Remove any references to the indicated message.
 */

void mca_oob_tcp_peer_dequeue_msg(mca_oob_tcp_peer_t* peer, mca_oob_tcp_msg_t* msg)
{
    ompi_list_item_t* item;
    OMPI_THREAD_LOCK(&peer->peer_lock);
    if (peer->peer_send_msg == msg)
        peer->peer_send_msg = NULL;
    if (peer->peer_recv_msg == msg)
        peer->peer_recv_msg = NULL;

    for( item =  ompi_list_get_first(&peer->peer_send_queue);
         item != ompi_list_get_end(&peer->peer_send_queue);
         item = ompi_list_get_next(item)) {
        if(item == (ompi_list_item_t*)msg) {
            ompi_list_remove_item(&peer->peer_send_queue, item);
            break;
        }
    }
    OMPI_THREAD_UNLOCK(&peer->peer_lock);
}


