/*
 * $HEADER$
 */
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "include/types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ns/base/base.h"
#include "ptl_tcp.h"
#include "ptl_tcp_addr.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_proc.h"
#include "ptl_tcp_sendfrag.h"


static void mca_ptl_tcp_peer_construct(mca_ptl_base_peer_t* ptl_peer);
static void mca_ptl_tcp_peer_destruct(mca_ptl_base_peer_t* ptl_peer);
static int  mca_ptl_tcp_peer_start_connect(mca_ptl_base_peer_t*);
static void mca_ptl_tcp_peer_connected(mca_ptl_base_peer_t*);
static void mca_ptl_tcp_peer_recv_handler(int sd, short flags, void* user);
static void mca_ptl_tcp_peer_send_handler(int sd, short flags, void* user);



ompi_class_t  mca_ptl_tcp_peer_t_class = {
    "mca_tcp_ptl_peer_t", 
    OBJ_CLASS(ompi_list_item_t),
    (ompi_construct_t)mca_ptl_tcp_peer_construct, 
    (ompi_destruct_t)mca_ptl_tcp_peer_destruct
};

/*
 * Initialize state of the peer instance.
 */

static void mca_ptl_tcp_peer_construct(mca_ptl_base_peer_t* ptl_peer)
{
    ptl_peer->peer_ptl = 0;
    ptl_peer->peer_proc = 0;
    ptl_peer->peer_addr = 0;
    ptl_peer->peer_sd = -1;
    ptl_peer->peer_send_frag = 0;
    ptl_peer->peer_recv_frag = 0;
    ptl_peer->peer_send_event.ev_flags = 0;
    ptl_peer->peer_recv_event.ev_flags = 0;
    ptl_peer->peer_state = MCA_PTL_TCP_CLOSED;
    ptl_peer->peer_retries = 0;
    OBJ_CONSTRUCT(&ptl_peer->peer_frags, ompi_list_t);
    OBJ_CONSTRUCT(&ptl_peer->peer_send_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&ptl_peer->peer_recv_lock, ompi_mutex_t);
}

/*
 * Cleanup any resources held by the peer.
 */

static void mca_ptl_tcp_peer_destruct(mca_ptl_base_peer_t* ptl_peer)
{
    mca_ptl_tcp_proc_remove(ptl_peer->peer_proc, ptl_peer);
    mca_ptl_tcp_peer_close(ptl_peer);
}

/*
 * diagnostics
 */

static void mca_ptl_tcp_peer_dump(mca_ptl_base_peer_t* ptl_peer, const char* msg)
{
    char src[64];
    char dst[64];
    char buff[255];
    int sndbuf,rcvbuf,nodelay,flags;
    struct sockaddr_in inaddr;
    ompi_socklen_t optlen;
    ompi_socklen_t addrlen = sizeof(struct sockaddr_in);

    getsockname(ptl_peer->peer_sd, (struct sockaddr*)&inaddr, &addrlen);
    sprintf(src, "%s", inet_ntoa(inaddr.sin_addr));
    getpeername(ptl_peer->peer_sd, (struct sockaddr*)&inaddr, &addrlen);
    sprintf(dst, "%s", inet_ntoa(inaddr.sin_addr));

    if((flags = fcntl(ptl_peer->peer_sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "mca_ptl_tcp_peer_connect: fcntl(F_GETFL) failed with errno=%d\n", errno);
    }

#if defined(SO_SNDBUF)
    optlen = sizeof(sndbuf);
    if(getsockopt(ptl_peer->peer_sd, SOL_SOCKET, SO_SNDBUF, (char *)&sndbuf, &optlen) < 0) {
        ompi_output(0, "mca_ptl_tcp_peer_dump: SO_SNDBUF option: errno %d\n", errno);
    }
#else
    sndbuf = -1;
#endif
#if defined(SO_RCVBUF)
    optlen = sizeof(rcvbuf);
    if(getsockopt(ptl_peer->peer_sd, SOL_SOCKET, SO_RCVBUF, (char *)&rcvbuf, &optlen) < 0) {
        ompi_output(0, "mca_ptl_tcp_peer_dump: SO_RCVBUF option: errno %d\n", errno);
    }
#else
    rcvbuf = -1;
#endif
#if defined(TCP_NODELAY)
    optlen = sizeof(nodelay);
    if(getsockopt(ptl_peer->peer_sd, IPPROTO_TCP, TCP_NODELAY, &nodelay, &optlen) < 0) {
        ompi_output(0, "mca_ptl_tcp_peer_dump: TCP_NODELAY option: errno %d\n", errno);
    }
#else
    nodelay = 0;
#endif

    sprintf(buff, "%s: %s - %s nodelay %d sndbuf %d rcvbuf %d flags %08x\n", 
        msg, src, dst, nodelay, sndbuf, rcvbuf, flags);
    ompi_output(0, buff);
}

/*
 * Initialize events to be used by the peer instance for TCP select/poll callbacks.
 */

static inline void mca_ptl_tcp_peer_event_init(mca_ptl_base_peer_t* ptl_peer, int sd)
{
    ompi_event_set(
        &ptl_peer->peer_recv_event, 
        ptl_peer->peer_sd, 
        OMPI_EV_READ|OMPI_EV_PERSIST, 
        mca_ptl_tcp_peer_recv_handler,
        ptl_peer);
    ompi_event_set(
        &ptl_peer->peer_send_event, 
        ptl_peer->peer_sd, 
        OMPI_EV_WRITE|OMPI_EV_PERSIST, 
        mca_ptl_tcp_peer_send_handler,
        ptl_peer);
}


/*
 * Attempt to send a fragment using a given peer. If the peer is not connected,
 * queue the fragment and start the connection as required.
 */

int mca_ptl_tcp_peer_send(mca_ptl_base_peer_t* ptl_peer, mca_ptl_tcp_send_frag_t* frag)
{
    int rc = OMPI_SUCCESS;
    OMPI_THREAD_LOCK(&ptl_peer->peer_send_lock);
    switch(ptl_peer->peer_state) {
    case MCA_PTL_TCP_CONNECTING:
    case MCA_PTL_TCP_CONNECT_ACK:
    case MCA_PTL_TCP_CLOSED:
        ompi_list_append(&ptl_peer->peer_frags, (ompi_list_item_t*)frag);
        if(ptl_peer->peer_state == MCA_PTL_TCP_CLOSED)
            rc = mca_ptl_tcp_peer_start_connect(ptl_peer);
        break;
    case MCA_PTL_TCP_FAILED:
        rc = OMPI_ERR_UNREACH;
        break;
    case MCA_PTL_TCP_CONNECTED:
        if (NULL != ptl_peer->peer_send_frag) {
            ompi_list_append(&ptl_peer->peer_frags, (ompi_list_item_t*)frag);
        } else {
            if(mca_ptl_tcp_send_frag_handler(frag, ptl_peer->peer_sd)) {
                OMPI_THREAD_UNLOCK(&ptl_peer->peer_send_lock);
                mca_ptl_tcp_send_frag_progress(frag);
                return rc;
            } else {
                ptl_peer->peer_send_frag = frag;
                ompi_event_add(&ptl_peer->peer_send_event, 0);
            }
        }
        break;
    }
    OMPI_THREAD_UNLOCK(&ptl_peer->peer_send_lock);
    return rc;
}


/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the peers endpoint.
 */
static int mca_ptl_tcp_peer_send_blocking(mca_ptl_base_peer_t* ptl_peer, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    while(cnt < size) {
        int retval = send(ptl_peer->peer_sd, ptr+cnt, size-cnt, 0);
        if(retval < 0) {
            if(errno != EINTR && errno != EAGAIN && errno != EWOULDBLOCK) {
                ompi_output(0, "mca_ptl_tcp_peer_send_blocking: send() failed with errno=%d\n",errno);
                mca_ptl_tcp_peer_close(ptl_peer);
                return -1;
            }
            continue;
        }
        cnt += retval;
    }
    return cnt;
}


/*
 * Send the globally unique identifier for this process to a peer on 
 * a newly connected socket.
 */

static int mca_ptl_tcp_peer_send_connect_ack(mca_ptl_base_peer_t* ptl_peer)
{
    /* send process identifier to remote peer */
    mca_ptl_tcp_proc_t* ptl_proc = mca_ptl_tcp_proc_local();
    if(mca_ptl_tcp_peer_send_blocking(ptl_peer, &ptl_proc->proc_guid, sizeof(ptl_proc->proc_guid)) != 
          sizeof(ptl_proc->proc_guid)) {
        return OMPI_ERR_UNREACH;
    }
    return OMPI_SUCCESS;
}

/*
 * Check the state of this peer. If the incoming connection request matches
 * our peers address, check the state of our connection:
 * (1) if a connection has not been attempted, accept the connection
 * (2) if a connection has not been established, and the peers process identifier
 *     is less than the local process, accept the connection
 * otherwise, reject the connection and continue with the current connection 
 */

bool mca_ptl_tcp_peer_accept(mca_ptl_base_peer_t* ptl_peer, struct sockaddr_in* addr, int sd)
{
    mca_ptl_tcp_addr_t* ptl_addr;
    mca_ptl_tcp_proc_t* this_proc = mca_ptl_tcp_proc_local();
    ompi_ns_cmp_bitmask_t mask = OMPI_NS_CMP_CELLID | OMPI_NS_CMP_JOBID | OMPI_NS_CMP_VPID;

    OMPI_THREAD_LOCK(&ptl_peer->peer_recv_lock);
    OMPI_THREAD_LOCK(&ptl_peer->peer_send_lock);
    if((ptl_addr = ptl_peer->peer_addr) != NULL  &&
        ptl_addr->addr_inet.s_addr == addr->sin_addr.s_addr) {
        mca_ptl_tcp_proc_t *peer_proc = ptl_peer->peer_proc;
        if((ptl_peer->peer_sd < 0) ||
           (ptl_peer->peer_state != MCA_PTL_TCP_CONNECTED &&
            ompi_name_server.compare(mask,
                                     &peer_proc->proc_ompi->proc_name,
                                     &this_proc->proc_ompi->proc_name) < 0)) {
            mca_ptl_tcp_peer_close(ptl_peer);
            ptl_peer->peer_sd = sd;
            if(mca_ptl_tcp_peer_send_connect_ack(ptl_peer) != OMPI_SUCCESS) {
                 mca_ptl_tcp_peer_close(ptl_peer);
                 OMPI_THREAD_UNLOCK(&ptl_peer->peer_send_lock);
                 OMPI_THREAD_UNLOCK(&ptl_peer->peer_recv_lock);
                 return false;
            }
            mca_ptl_tcp_peer_event_init(ptl_peer, sd);
            ompi_event_add(&ptl_peer->peer_recv_event, 0);
            mca_ptl_tcp_peer_connected(ptl_peer);
#if OMPI_ENABLE_DEBUG
            mca_ptl_tcp_peer_dump(ptl_peer, "accepted");
#endif
            OMPI_THREAD_UNLOCK(&ptl_peer->peer_send_lock);
            OMPI_THREAD_UNLOCK(&ptl_peer->peer_recv_lock);
            return true;
        }
    }
    OMPI_THREAD_UNLOCK(&ptl_peer->peer_send_lock);
    OMPI_THREAD_UNLOCK(&ptl_peer->peer_recv_lock);
    return false;
}


/*
 * Remove any event registrations associated with the socket
 * and update the peer state to reflect the connection has
 * been closed.
 */

void mca_ptl_tcp_peer_close(mca_ptl_base_peer_t* ptl_peer)
{
    if(ptl_peer->peer_sd >= 0) {
        ompi_event_del(&ptl_peer->peer_recv_event);
        ompi_event_del(&ptl_peer->peer_send_event);
        close(ptl_peer->peer_sd);
        ptl_peer->peer_sd = -1;
    }
    ptl_peer->peer_state = MCA_PTL_TCP_CLOSED;
    ptl_peer->peer_retries++;
}

/*
 *  Setup peer state to reflect that connection has been established,
 *  and start any pending sends.
 */

static void mca_ptl_tcp_peer_connected(mca_ptl_base_peer_t* ptl_peer)
{
    /* setup socket options */
    ptl_peer->peer_state = MCA_PTL_TCP_CONNECTED;
    ptl_peer->peer_retries = 0;
    if(ompi_list_get_size(&ptl_peer->peer_frags) > 0) {
        if(NULL == ptl_peer->peer_send_frag)
            ptl_peer->peer_send_frag = (mca_ptl_tcp_send_frag_t*)
                ompi_list_remove_first(&ptl_peer->peer_frags);
        ompi_event_add(&ptl_peer->peer_send_event, 0);
    }
}


/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the peers endpoint.
 */
static int mca_ptl_tcp_peer_recv_blocking(mca_ptl_base_peer_t* ptl_peer, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    while(cnt < size) {
        int retval = recv(ptl_peer->peer_sd, ptr+cnt, size-cnt, 0);

        /* remote closed connection */
        if(retval == 0) {
            mca_ptl_tcp_peer_close(ptl_peer);
            return -1;
        }

        /* socket is non-blocking so handle errors */
        if(retval < 0) {
            if(errno != EINTR && errno != EAGAIN && errno != EWOULDBLOCK) {
                ompi_output(0, "mca_ptl_tcp_peer_recv_blocking: recv() failed with errno=%d\n",errno);
                mca_ptl_tcp_peer_close(ptl_peer);
                return -1;
            }
            continue;
        }
        cnt += retval;
    }
    if((int)cnt == -1)
        ompi_output(0, "mca_ptl_tcp_peer_recv_blocking: invalid cnt\n");
    return cnt;
}



/*
 *  Receive the peers globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 */

static int mca_ptl_tcp_peer_recv_connect_ack(mca_ptl_base_peer_t* ptl_peer)
{
    ompi_process_name_t guid;
    mca_ptl_tcp_proc_t* ptl_proc = ptl_peer->peer_proc;

    if((mca_ptl_tcp_peer_recv_blocking(ptl_peer, &guid, sizeof(ompi_process_name_t))) != sizeof(ompi_process_name_t)) {
        return OMPI_ERR_UNREACH;
    }

    /* compare this to the expected values */
    if(memcmp(&ptl_proc->proc_guid, &guid, sizeof(ompi_process_name_t)) != 0) {
        ompi_output(0, "mca_ptl_tcp_peer_connect: received unexpected process identifier");
        mca_ptl_tcp_peer_close(ptl_peer);
        return OMPI_ERR_UNREACH;
    }

    /* connected */
    mca_ptl_tcp_peer_connected(ptl_peer);
#if OMPI_ENABLE_DEBUG
    mca_ptl_tcp_peer_dump(ptl_peer, "connected");
#endif
    return OMPI_SUCCESS;
}


void mca_ptl_tcp_set_socket_options(int sd)
{
    int optval;
#if defined(TCP_NODELAY)
    optval = 1;
    if(setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, &optval, sizeof(optval)) < 0) {
        ompi_output(0, 
            "mca_ptl_tcp_set_socket_options: setsockopt(TCP_NODELAY) failed with errno=%d\n", 
            errno);
    }
#endif
#if defined(SO_SNDBUF)
    if(mca_ptl_tcp_component.tcp_sndbuf > 0 &&
       setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (char *)&mca_ptl_tcp_component.tcp_sndbuf, sizeof(int)) < 0) {
        ompi_output(0, 
            "mca_ptl_tcp_set_socket_options: SO_SNDBUF option: errno %d\n", 
            errno);
    }
#endif
#if defined(SO_RCVBUF)
    if(mca_ptl_tcp_component.tcp_rcvbuf > 0 &&
       setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (char *)&mca_ptl_tcp_component.tcp_rcvbuf, sizeof(int)) < 0) {
        ompi_output(0, 
            "mca_ptl_tcp_set_socket_options: SO_RCVBUF option: errno %d\n", 
            errno);
    }
#endif
}



/*
 *  Start a connection to the peer. This will likely not complete,
 *  as the socket is set to non-blocking, so register for event
 *  notification of connect completion. On connection we send
 *  our globally unique process identifier to the peer and wait for
 *  the peers response.
 */

static int mca_ptl_tcp_peer_start_connect(mca_ptl_base_peer_t* ptl_peer)
{
    int rc,flags;
    struct sockaddr_in peer_addr;

    ptl_peer->peer_sd = socket(AF_INET, SOCK_STREAM, 0);
    if (ptl_peer->peer_sd < 0) {
        ptl_peer->peer_retries++;
        return OMPI_ERR_UNREACH;
    }

    /* setup socket buffer sizes */
    mca_ptl_tcp_set_socket_options(ptl_peer->peer_sd);

    /* setup event callbacks */
    mca_ptl_tcp_peer_event_init(ptl_peer, ptl_peer->peer_sd);

    /* setup the socket as non-blocking */
    if((flags = fcntl(ptl_peer->peer_sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "mca_ptl_tcp_peer_connect: fcntl(F_GETFL) failed with errno=%d\n", errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(ptl_peer->peer_sd, F_SETFL, flags) < 0)
            ompi_output(0, "mca_ptl_tcp_peer_connect: fcntl(F_SETFL) failed with errno=%d\n", errno);
    }
                                                                                                              
    /* start the connect - will likely fail with EINPROGRESS */
    peer_addr.sin_family = AF_INET;
    peer_addr.sin_addr = ptl_peer->peer_addr->addr_inet;
    peer_addr.sin_port = ptl_peer->peer_addr->addr_port;
    if(connect(ptl_peer->peer_sd, (struct sockaddr*)&peer_addr, sizeof(peer_addr)) < 0) {
        /* non-blocking so wait for completion */
        if(errno == EINPROGRESS) {
            ptl_peer->peer_state = MCA_PTL_TCP_CONNECTING;
            ompi_event_add(&ptl_peer->peer_send_event, 0);
            return OMPI_SUCCESS;
        }
        mca_ptl_tcp_peer_close(ptl_peer);
        ptl_peer->peer_retries++;
        return OMPI_ERR_UNREACH;
    }

    /* send our globally unique process identifier to the peer */
    if((rc = mca_ptl_tcp_peer_send_connect_ack(ptl_peer)) == OMPI_SUCCESS) {
        ptl_peer->peer_state = MCA_PTL_TCP_CONNECT_ACK;
        ompi_event_add(&ptl_peer->peer_recv_event, 0);
    } else {
        mca_ptl_tcp_peer_close(ptl_peer);
    }
    return rc;
}


/*
 * Check the status of the connection. If the connection failed, will retry
 * later. Otherwise, send this processes identifier to the peer on the 
 * newly connected socket.
 */

static void mca_ptl_tcp_peer_complete_connect(mca_ptl_base_peer_t* ptl_peer)
{
    int so_error = 0;
    ompi_socklen_t so_length = sizeof(so_error);

    /* unregister from receiving event notifications */
    ompi_event_del(&ptl_peer->peer_send_event);

    /* check connect completion status */
    if(getsockopt(ptl_peer->peer_sd, SOL_SOCKET, SO_ERROR, &so_error, &so_length) < 0) {
        ompi_output(0, "mca_ptl_tcp_peer_complete_connect: getsockopt() failed with errno=%d\n", errno);
        mca_ptl_tcp_peer_close(ptl_peer);
        return;
    }
    if(so_error == EINPROGRESS) {
        ompi_event_add(&ptl_peer->peer_send_event, 0);
        return;
    }
    if(so_error != 0) {
        ompi_output(0, "mca_ptl_tcp_peer_complete_connect: connect() failed with errno=%d\n", so_error);
        mca_ptl_tcp_peer_close(ptl_peer);
        return;
    }

    if(mca_ptl_tcp_peer_send_connect_ack(ptl_peer) == OMPI_SUCCESS) {
        ptl_peer->peer_state = MCA_PTL_TCP_CONNECT_ACK;
        ompi_event_add(&ptl_peer->peer_recv_event, 0);
    } else {
        mca_ptl_tcp_peer_close(ptl_peer);
    }
}


/*
 * A file descriptor is available/ready for recv. Check the state 
 * of the socket and take the appropriate action.
 */

static void mca_ptl_tcp_peer_recv_handler(int sd, short flags, void* user)
{
    mca_ptl_base_peer_t* ptl_peer = user;
    OMPI_THREAD_LOCK(&ptl_peer->peer_recv_lock);
    switch(ptl_peer->peer_state) {
    case MCA_PTL_TCP_CONNECT_ACK:
        {
        mca_ptl_tcp_peer_recv_connect_ack(ptl_peer);
        break;
        }
    case MCA_PTL_TCP_CONNECTED:
        {
        mca_ptl_tcp_recv_frag_t* recv_frag = ptl_peer->peer_recv_frag;
        if(NULL == recv_frag) {
            int rc;
            MCA_PTL_TCP_RECV_FRAG_ALLOC(recv_frag, rc);
            if(NULL == recv_frag) {
                OMPI_THREAD_UNLOCK(&ptl_peer->peer_recv_lock);
                return;
            }
            mca_ptl_tcp_recv_frag_init(recv_frag, ptl_peer);
        }

        /* check for completion of non-blocking recv on the current fragment */
        if(mca_ptl_tcp_recv_frag_handler(recv_frag, sd) == false)
            ptl_peer->peer_recv_frag = recv_frag;
        else
            ptl_peer->peer_recv_frag = 0;
        break;
        }
    default:
        {
        ompi_output(0, "mca_ptl_tcp_peer_recv_handler: invalid socket state(%d)", ptl_peer->peer_state);
        mca_ptl_tcp_peer_close(ptl_peer);
        break;
        }
    }
    OMPI_THREAD_UNLOCK(&ptl_peer->peer_recv_lock);
}


/*
 * A file descriptor is available/ready for send. Check the state 
 * of the socket and take the appropriate action.
 */

static void mca_ptl_tcp_peer_send_handler(int sd, short flags, void* user)
{
    mca_ptl_tcp_peer_t* ptl_peer = user;
    OMPI_THREAD_LOCK(&ptl_peer->peer_send_lock);
    switch(ptl_peer->peer_state) {
    case MCA_PTL_TCP_CONNECTING:
        mca_ptl_tcp_peer_complete_connect(ptl_peer);
        break;
    case MCA_PTL_TCP_CONNECTED: 
        {
        /* complete the current send */
        do {
            mca_ptl_tcp_send_frag_t* frag = ptl_peer->peer_send_frag;
            if(mca_ptl_tcp_send_frag_handler(frag, ptl_peer->peer_sd) == false) {
                break;
            }

            /* if required - update request status and release fragment */
            OMPI_THREAD_UNLOCK(&ptl_peer->peer_send_lock);
            mca_ptl_tcp_send_frag_progress(frag);
            OMPI_THREAD_LOCK(&ptl_peer->peer_send_lock);

            /* progress any pending sends */
            ptl_peer->peer_send_frag = (mca_ptl_tcp_send_frag_t*)
                ompi_list_remove_first(&ptl_peer->peer_frags);
        } while (NULL != ptl_peer->peer_send_frag);

        /* if nothing else to do unregister for send event notifications */
        if(NULL == ptl_peer->peer_send_frag) {
            ompi_event_del(&ptl_peer->peer_send_event);
        }
        break;
        }
    default:
        ompi_output(0, "mca_ptl_tcp_peer_send_handler: invalid connection state (%d)", 
            ptl_peer->peer_state);
        ompi_event_del(&ptl_peer->peer_send_event);
        break;
    }
    OMPI_THREAD_UNLOCK(&ptl_peer->peer_send_lock);
}



