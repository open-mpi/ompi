#include <unistd.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/errno.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "util/output.h"
#include "mca/oob/tcp/oob_tcp_peer.h"


static int  mca_oob_tcp_peer_start_connect(mca_oob_tcp_peer_t* peer);
static int  mca_oob_tcp_peer_event_init(mca_oob_tcp_peer_t* peer, int sd);
static void mca_oob_tcp_peer_close(mca_oob_tcp_peer_t* peer);
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
    OBJ_CONSTRUCT(&(peer->peer_recv_queue), ompi_list_t);
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
    OBJ_DESTRUCT(&(peer->peer_send_queue));
    OBJ_DESTRUCT(&(peer->peer_recv_queue));
    OBJ_DESTRUCT(&(peer->peer_lock));
}

/*
 * Initialize events to be used by the peer instance for TCP select/poll callbacks.
 */

static int mca_oob_tcp_peer_event_init(mca_oob_tcp_peer_t* peer, int sd)
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
 *  
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
            if(mca_oob_tcp_msg_send_handler(msg, peer->peer_sd)) {
                OMPI_THREAD_UNLOCK(&peer->peer_lock);
                mca_oob_tcp_msg_complete(msg);
                return rc;
            } else {
                peer->peer_send_msg = msg;
                ompi_event_add(&peer->peer_send_event, 0);
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
    mca_oob_tcp_peer_t * peer;

    OMPI_THREAD_LOCK(&mca_oob_tcp_module.tcp_lock);
    peer = (mca_oob_tcp_peer_t*)ompi_rb_tree_find(&mca_oob_tcp_module.tcp_peer_tree, name);
    if(NULL != peer) {
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
        return peer;
    }

    MCA_OOB_TCP_PEER_ALLOC(peer, rc);
    if(NULL == peer) {
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
        return NULL;
    }

    peer->peer_name = *name;
    if(OMPI_SUCCESS != ompi_rb_tree_insert(&mca_oob_tcp_module.tcp_peer_tree, name, peer)) {
        MCA_OOB_TCP_PEER_RETURN(peer);
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
        return NULL;
    }
    ompi_list_prepend(&mca_oob_tcp_module.tcp_peer_list, (ompi_list_item_t *) peer);
    if(ompi_list_get_size(&mca_oob_tcp_module.tcp_peer_list) > 
       mca_oob_tcp_module.tcp_cache_size) {
       /* do something - remove LRU items from peer list (that aren't in use) */
    }
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_module.tcp_lock);
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
    struct sockaddr_in peer_addr;
                                                                                                                                 
    peer->peer_sd = socket(AF_INET, SOCK_STREAM, 0);
    if (peer->peer_sd < 0) {
        peer->peer_retries++;
        return OMPI_ERR_UNREACH;
    }

    /* setup event callbacks */
    mca_oob_tcp_peer_event_init(peer, peer->peer_sd);

    /* setup the socket as non-blocking */
    if((flags = fcntl(peer->peer_sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "mca_oob_tcp_peer_connect: fcntl(F_GETFL) failed with errno=%d\n", errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(peer->peer_sd, F_SETFL, flags) < 0)
            ompi_output(0, "mca_oob_tcp_peer_connect: fcntl(F_SETFL) failed with errno=%d\n", errno);
    }

    /* start the connect - will likely fail with EINPROGRESS */
    peer_addr = peer->peer_addr;
    if(connect(peer->peer_sd, (struct sockaddr*)&peer_addr, sizeof(peer_addr)) < 0) {
        /* non-blocking so wait for completion */
        if(errno == EINPROGRESS) {
            peer->peer_state = MCA_OOB_TCP_CONNECTING;
            ompi_event_add(&peer->peer_send_event, 0);
            return OMPI_SUCCESS;
        }
        mca_oob_tcp_peer_close(peer);
        peer->peer_retries++;
        return OMPI_ERR_UNREACH;
    }

    /* send our globally unique process identifier to the peer */
    if((rc = mca_oob_tcp_peer_send_connect_ack(peer)) == OMPI_SUCCESS) {
        peer->peer_state = MCA_OOB_TCP_CONNECT_ACK;
        ompi_event_add(&peer->peer_recv_event, 0);
    } else {
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
        ompi_output(0, "mca_ptl_tcp_peer_complete_connect: getsockopt() failed with errno=%d\n", errno);
        mca_oob_tcp_peer_close(peer);
        return;
    }
    if(so_error == EINPROGRESS) {
        ompi_event_add(&peer->peer_send_event, 0);
        return;
    }
    if(so_error != 0) {
        ompi_output(0, "mca_oob_tcp_peer_complete_connect: connect() failed with errno=%d\n", so_error);
        mca_oob_tcp_peer_close(peer);
        return;
    }
                                                                                                                       
    if(mca_oob_tcp_peer_send_connect_ack(peer) == OMPI_SUCCESS) {
        peer->peer_state = MCA_OOB_TCP_CONNECT_ACK;
        ompi_event_add(&peer->peer_recv_event, 0);
    } else {
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

static void mca_oob_tcp_peer_close(mca_oob_tcp_peer_t* peer)
{
    if(peer->peer_sd >= 0) {
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
    if(mca_oob_tcp_peer_send_blocking( peer, &mca_oob_base_self, sizeof(mca_oob_base_self)) !=
          sizeof(mca_oob_base_self)) {
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
        mca_oob_tcp_msg_t* msg = peer->peer_recv_msg;
        if(NULL == msg) {
            int rc;
            MCA_OOB_TCP_MSG_ALLOC(msg, rc);
            if(NULL == msg) {
                OMPI_THREAD_UNLOCK(&peer->peer_lock);
                return;
            }
            mca_oob_tcp_msg_init(msg, peer);
        }

        /* check for completion of non-blocking recv on the current fragment */
        if(mca_oob_tcp_msg_recv_handler(msg, sd) == false)
            peer->peer_recv_msg = msg;
        else
            peer->peer_recv_msg = 0;
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
        /* complete the current send */
        do {
            mca_oob_tcp_msg_t* msg = peer->peer_send_msg;
            if(mca_oob_tcp_msg_send_handler(msg, sd) == false) {
                break;
            }
                                                                                                                       
            /* if required - update request status and release fragment */
            OMPI_THREAD_UNLOCK(&peer->peer_lock);
            mca_oob_tcp_msg_complete(msg);
            OMPI_THREAD_LOCK(&peer->peer_lock);
                                                                                                                       
            /* progress any pending sends */
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

