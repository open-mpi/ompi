/*
 * $HEADER$
 */
#include <stdlib.h>
#include <unistd.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include "lam/types.h"
#include "ptl_tcp.h"
#include "ptl_tcp_addr.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_proc.h"


lam_class_info_t  mca_ptl_tcp_peer_cls = {
    "mca_tcp_ptl_peer_t", 
    &lam_list_cls,
    (class_init_t)mca_ptl_tcp_peer_init, 
    (class_destroy_t)mca_ptl_tcp_peer_destroy
};


static int  mca_ptl_tcp_peer_start_connect(mca_ptl_peer_t*);
static void mca_ptl_tcp_peer_close_i(mca_ptl_peer_t*);
static void mca_ptl_tcp_peer_connected(mca_ptl_peer_t*);
static void mca_ptl_tcp_peer_recv_handler(mca_ptl_peer_t*, int sd);
static void mca_ptl_tcp_peer_send_handler(mca_ptl_peer_t*, int sd);
static void mca_ptl_tcp_peer_except_handler(mca_ptl_peer_t*, int sd);



static lam_reactor_listener_t mca_ptl_tcp_peer_listener = {
     (lam_rl_recv_handler_fn_t)mca_ptl_tcp_peer_recv_handler,
     (lam_rl_send_handler_fn_t)mca_ptl_tcp_peer_send_handler,
     (lam_rl_except_handler_fn_t)mca_ptl_tcp_peer_except_handler
};


/*
 * Initialize state of the peer instance.
 */

void mca_ptl_tcp_peer_init(mca_ptl_peer_t* ptl_peer)
{
    SUPER_INIT(ptl_peer, &lam_list_cls);
    ptl_peer->peer_ptl = 0;
    ptl_peer->peer_proc = 0;
    ptl_peer->peer_addr = 0;
    ptl_peer->peer_sd = -1;
    ptl_peer->peer_send_frag = 0;
    ptl_peer->peer_recv_frag = 0;
    ptl_peer->peer_state = MCA_PTL_TCP_CLOSED;
    ptl_peer->peer_retries = 0;
    STATIC_INIT(ptl_peer->peer_frags, &lam_list_cls);
    lam_mutex_init(&ptl_peer->peer_lock);
}


/*
 * Cleanup any resources held by the peer.
 */

void mca_ptl_tcp_peer_destroy(mca_ptl_peer_t* ptl_peer)
{
    mca_ptl_tcp_proc_remove(ptl_peer->peer_proc, ptl_peer);
    mca_ptl_tcp_peer_close_i(ptl_peer);
    SUPER_DESTROY(ptl_peer, &lam_list_cls);
}


/*
 * Attempt to send a fragment using a given peer. If the peer is not connected,
 * queue the fragment and start the connection as required.
 */

int mca_ptl_tcp_peer_send(mca_ptl_peer_t* ptl_peer, mca_ptl_tcp_send_frag_t* frag)
{
    int rc = LAM_SUCCESS;
    THREAD_LOCK(&ptl_peer->peer_lock);
    switch(ptl_peer->peer_state) {
    case MCA_PTL_TCP_CONNECTING:
    case MCA_PTL_TCP_CONNECT_ACK:
    case MCA_PTL_TCP_CLOSED:
        lam_list_append(&ptl_peer->peer_frags, (lam_list_item_t*)frag);
        if(ptl_peer->peer_state == MCA_PTL_TCP_CLOSED)
            rc = mca_ptl_tcp_peer_start_connect(ptl_peer);
        break;
    case MCA_PTL_TCP_FAILED:
        rc = LAM_ERR_UNREACH;
        break;
    case MCA_PTL_TCP_CONNECTED:
        if (NULL != ptl_peer->peer_send_frag) 
            lam_list_append(&ptl_peer->peer_frags, (lam_list_item_t*)frag);
        else {
            if(mca_ptl_tcp_send_frag_handler(frag, ptl_peer->peer_sd) == false) {
                ptl_peer->peer_send_frag = frag;
                rc = lam_reactor_insert(
                    &mca_ptl_tcp_module.tcp_reactor,
                     ptl_peer->peer_sd,
                     &mca_ptl_tcp_peer_listener,
                     ptl_peer,
                     LAM_REACTOR_NOTIFY_SEND);
            }
        }
        break;
    }
    THREAD_UNLOCK(&ptl_peer->peer_lock);
    return rc;
}

/*
 * Check the state of this peer. If the incoming connection request matches
 * our peers address, check the state of our connection:
 * (1) if a connection has not been attempted, accept the connection
 * (2) if a connection has not been established, and the peers process identifier
 *     is less than the local process, accept the connection
 * otherwise, reject the connection and continue with the current connection 
 */

bool mca_ptl_tcp_peer_accept(mca_ptl_peer_t* ptl_peer, struct sockaddr_in* addr, int sd)
{
    mca_ptl_tcp_addr_t* ptl_addr;
    mca_ptl_tcp_proc_t* this_proc = mca_ptl_tcp_proc_local();
    THREAD_LOCK(&ptl_peer->peer_lock);
    if((ptl_addr = ptl_peer->peer_addr) != NULL  &&
        ptl_addr->addr_inet.s_addr == addr->sin_addr.s_addr) {
        mca_ptl_tcp_proc_t *peer_proc = ptl_peer->peer_proc;
        if((ptl_peer->peer_sd < 0) ||
           (ptl_peer->peer_state != MCA_PTL_TCP_CONNECTED &&
            peer_proc->proc_lam->proc_vpid < this_proc->proc_lam->proc_vpid)) {
            mca_ptl_tcp_peer_close_i(ptl_peer);
            ptl_peer->peer_sd = sd;
            mca_ptl_tcp_peer_connected(ptl_peer);
            THREAD_UNLOCK(&ptl_peer->peer_lock);
            return true;
        }
    }
    THREAD_UNLOCK(&ptl_peer->peer_lock);
    return false;
}

/*
 * An external I/F to close a peer. Called in the event of failure
 * on read or write. Note that this must acquire the peer lock
 * prior to delegating to the internal routine.
 */

void mca_ptl_tcp_peer_close(mca_ptl_peer_t* ptl_peer)
{
    THREAD_LOCK(&ptl_peer->peer_lock);
    mca_ptl_tcp_peer_close_i(ptl_peer);
    THREAD_UNLOCK(&ptl_peer->peer_lock);
}

/*
 * Remove the socket descriptor from the reactor, close it,
 * and update the peer state to reflect the connection has
 * been closed.
 */

static void mca_ptl_tcp_peer_close_i(mca_ptl_peer_t* ptl_peer)
{
    if(ptl_peer->peer_sd >= 0) {
        lam_reactor_remove(
            &mca_ptl_tcp_module.tcp_reactor,
             ptl_peer->peer_sd,
             LAM_REACTOR_NOTIFY_ALL);
        close(ptl_peer->peer_sd);
        ptl_peer->peer_sd = -1;
    }
    ptl_peer->peer_state = MCA_PTL_TCP_CLOSED;
    ptl_peer->peer_retries++;
}

/*
 * 
 */

static void mca_ptl_tcp_peer_connected(mca_ptl_peer_t* ptl_peer)
{
    int flags = LAM_REACTOR_NOTIFY_RECV|LAM_REACTOR_NOTIFY_EXCEPT;
    ptl_peer->peer_state = MCA_PTL_TCP_CONNECTED;
    ptl_peer->peer_retries = 0;
    if(lam_list_get_size(&ptl_peer->peer_frags) > 0) {
        if(NULL == ptl_peer->peer_send_frag)
            ptl_peer->peer_send_frag = (mca_ptl_tcp_send_frag_t*)lam_list_remove_first(&ptl_peer->peer_frags);
        flags |= LAM_REACTOR_NOTIFY_SEND;
    }
    lam_reactor_insert(
        &mca_ptl_tcp_module.tcp_reactor,
        ptl_peer->peer_sd,
        &mca_ptl_tcp_peer_listener,
        ptl_peer,
        flags);
}


/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the peers endpoint.
 */
static int mca_ptl_tcp_peer_recv_blocking(mca_ptl_peer_t* ptl_peer, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    while(cnt < size) {
        int retval = recv(ptl_peer->peer_sd, ptr+cnt, size-cnt, 0);

        /* remote closed connection */
        if(retval == 0) {
            mca_ptl_tcp_peer_close_i(ptl_peer);
            return -1;
        }

        /* socket is non-blocking so handle errors */
        if(retval < 0) {
            if(errno == EINTR)
                continue;
            if(errno != EAGAIN && errno != EWOULDBLOCK) {
                lam_output(0, "mca_ptl_tcp_peer_recv_blocking: recv() failed with errno=%d\n",errno);
                mca_ptl_tcp_peer_close_i(ptl_peer);
                return -1;
            }
        }
        cnt += retval;
    }
    return cnt;
}


/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the peers endpoint.
 */
static int mca_ptl_tcp_peer_send_blocking(mca_ptl_peer_t* ptl_peer, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    while(cnt < size) {
        int retval = send(ptl_peer->peer_sd, ptr+cnt, size-cnt, 0);
        if(retval < 0) {
            if(errno == EINTR)
                continue;
            if(errno != EAGAIN && errno != EWOULDBLOCK) {
                lam_output(0, "mca_ptl_tcp_peer_send_blocking: send() failed with errno=%d\n",errno);
                mca_ptl_tcp_peer_close_i(ptl_peer);
                return -1;
            }
        }
        cnt += retval;
    }
    return cnt;
}


/*
 *  Receive the peers globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 */

static int mca_ptl_tcp_peer_recv_connect_ack(mca_ptl_peer_t* ptl_peer)
{
    uint32_t size_n, size_h;
    void* guid;
    mca_ptl_tcp_proc_t* ptl_proc = ptl_peer->peer_proc;

    if(mca_ptl_tcp_peer_recv_blocking(ptl_peer, &size_n, sizeof(size_n)) != size_n)
        return LAM_ERR_UNREACH;
    size_h = ntohl(size_n);
    guid = LAM_MALLOC(size_h);
    if(NULL == guid)
        return LAM_ERR_OUT_OF_RESOURCE;

    if(mca_ptl_tcp_peer_recv_blocking(ptl_peer, guid, size_h) != size_h) {
        LAM_FREE(guid);
        return LAM_ERR_UNREACH;
    }

    /* compare this to the expected values */
    if(size_h != ptl_proc->proc_guid_size || memcmp(ptl_proc->proc_guid, guid, size_h) != 0) {
        lam_output(0, "mca_ptl_tcp_peer_connect: received unexpected process identifier");
        mca_ptl_tcp_peer_close_i(ptl_peer);
        return LAM_ERR_UNREACH;
    }

    /* connected */
    mca_ptl_tcp_peer_connected(ptl_peer);
    return LAM_SUCCESS;
}


/*
 * Send the globally unique identifier for this process to a peer on 
 * a newly connected socket.
 */

static int mca_ptl_tcp_peer_send_connect_ack(mca_ptl_peer_t* ptl_peer)
{
    /* send process identifier to remote peer */
    mca_ptl_tcp_proc_t* ptl_proc = mca_ptl_tcp_proc_local();
    uint32_t size_n = htonl(ptl_proc->proc_guid_size);
    if(mca_ptl_tcp_peer_send_blocking(ptl_peer, &size_n, sizeof(size_n)) != sizeof(size_n) ||
       mca_ptl_tcp_peer_send_blocking(ptl_peer, ptl_proc->proc_guid, ptl_proc->proc_guid_size) != 
          ptl_proc->proc_guid_size) {
        return LAM_ERR_UNREACH;
    }
    return LAM_SUCCESS;
}


/*
 *  Start a connection to the peer. This will likely not complete,
 *  as the socket is set to non-blocking, so register with the reactor
 *  for notification of connect completion. On connection we send
 *  our globally unique process identifier to the peer and wait for
 *  the peers response.
 */

static int mca_ptl_tcp_peer_start_connect(mca_ptl_peer_t* ptl_peer)
{
    int rc;
    ptl_peer->peer_sd = socket(AF_INET, SOCK_STREAM, 0);
    if (ptl_peer->peer_sd < 0) {
        ptl_peer->peer_retries++;
        return LAM_ERR_UNREACH;
    }

    /* setup the socket as non-blocking */
    int flags;
    if((flags = fcntl(ptl_peer->peer_sd, F_GETFL, 0)) < 0) {
        lam_output(0, "mca_ptl_tcp_peer_connect: fcntl(F_GETFL) failed with errno=%d\n", errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(ptl_peer->peer_sd, F_SETFL, flags) < 0)
            lam_output(0, "mca_ptl_tcp_peer_connect: fcntl(F_SETFL) failed with errno=%d\n", errno);
    }
                                                                                                              
    /* start the connect - will likely fail with EINPROGRESS */
    if(connect(ptl_peer->peer_sd, (struct sockaddr*)&ptl_peer->peer_addr->addr_inet, 
       sizeof(struct sockaddr_in)) < 0) {
        /* non-blocking so wait for completion */
        if(errno == EINPROGRESS) {
            ptl_peer->peer_state = MCA_PTL_TCP_CONNECTING;
            rc = lam_reactor_insert(
                &mca_ptl_tcp_module.tcp_reactor,
                ptl_peer->peer_sd,
                &mca_ptl_tcp_peer_listener,
                ptl_peer,
                LAM_REACTOR_NOTIFY_SEND|LAM_REACTOR_NOTIFY_EXCEPT);
            return rc;
        }
        mca_ptl_tcp_peer_close_i(ptl_peer);
        ptl_peer->peer_retries++;
        return LAM_ERR_UNREACH;
    }

    /* send our globally unique process identifier to the peer */
    if((rc = mca_ptl_tcp_peer_send_connect_ack(ptl_peer)) == LAM_SUCCESS) {
        ptl_peer->peer_state = MCA_PTL_TCP_CONNECT_ACK;
        rc = lam_reactor_insert(
            &mca_ptl_tcp_module.tcp_reactor,
            ptl_peer->peer_sd,
            &mca_ptl_tcp_peer_listener,
            ptl_peer,
            LAM_REACTOR_NOTIFY_RECV|LAM_REACTOR_NOTIFY_EXCEPT);
    } else {
        mca_ptl_tcp_peer_close_i(ptl_peer);
    }
    return rc;
}


/*
 * Check the status of the connection. If the connection failed, will retry
 * later. Otherwise, send this processes identifier to the peer on the 
 * newly connected socket.
 */

static void mca_ptl_tcp_peer_complete_connect(mca_ptl_peer_t* ptl_peer)
{
    int so_error = 0;
    lam_socklen_t so_length = sizeof(so_error);

    /* unregister for event notifications */
    lam_reactor_remove(&mca_ptl_tcp_module.tcp_reactor, ptl_peer->peer_sd, LAM_REACTOR_NOTIFY_ALL);

    /* check connect completion status */
    if(getsockopt(ptl_peer->peer_sd, SOL_SOCKET, SO_ERROR, &so_error, &so_length) < 0) {
        lam_output(0, "mca_ptl_tcp_peer_complete_connect: getsockopt() failed with errno=%d\n", errno);
        mca_ptl_tcp_peer_close_i(ptl_peer);
        return;
    }
    if(so_error == EINPROGRESS) {
        lam_reactor_insert(
            &mca_ptl_tcp_module.tcp_reactor,
            ptl_peer->peer_sd,
            &mca_ptl_tcp_peer_listener,
            ptl_peer,
            LAM_REACTOR_NOTIFY_SEND|LAM_REACTOR_NOTIFY_EXCEPT);
        return;
    }
    if(so_error != 0) {
        lam_output(0, "mca_ptl_tcp_peer_complete_connect: connect() failedd with errno=%d\n", so_error);
        mca_ptl_tcp_peer_close_i(ptl_peer);
        return;
    }

    if(mca_ptl_tcp_peer_send_connect_ack(ptl_peer) == LAM_SUCCESS) {
        ptl_peer->peer_state = MCA_PTL_TCP_CONNECT_ACK;
        lam_reactor_insert(
            &mca_ptl_tcp_module.tcp_reactor,
            ptl_peer->peer_sd,
            &mca_ptl_tcp_peer_listener,
            ptl_peer,
            LAM_REACTOR_NOTIFY_RECV|LAM_REACTOR_NOTIFY_EXCEPT);
    } else {
        mca_ptl_tcp_peer_close_i(ptl_peer);
    }
}


/*
 * A file descriptor is available/ready for recv. Check the state 
 * of the socket and take the appropriate action.
 */

static void mca_ptl_tcp_peer_recv_handler(mca_ptl_peer_t* ptl_peer, int sd)
{
    THREAD_LOCK(&ptl_peer->peer_lock);
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
            recv_frag = (mca_ptl_tcp_recv_frag_t*)lam_free_list_get(&mca_ptl_tcp_module.tcp_recv_frags, &rc);
            if(recv_frag == 0) {
                THREAD_UNLOCK(&ptl_peer->peer_lock);
                return;
            }
            mca_ptl_tcp_recv_frag_reinit(recv_frag, ptl_peer);
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
        lam_output(0, "mca_ptl_tcp_peer_recv_handler: invalid socket state(%d)", ptl_peer->peer_state);
        mca_ptl_tcp_peer_close_i(ptl_peer);
        break;
        }
    }
    THREAD_UNLOCK(&ptl_peer->peer_lock);
}


/*
 * A file descriptor is available/ready for send. Check the state 
 * of the socket and take the appropriate action.
 */

static void mca_ptl_tcp_peer_send_handler(mca_ptl_peer_t* ptl_peer, int sd)
{
    THREAD_LOCK(&ptl_peer->peer_lock);
    switch(ptl_peer->peer_state) {
    case MCA_PTL_TCP_CONNECTING:
        mca_ptl_tcp_peer_complete_connect(ptl_peer);
        break;
    case MCA_PTL_TCP_CONNECTED:
        /* complete the current send */
        do {
            if(mca_ptl_tcp_send_frag_handler(ptl_peer->peer_send_frag, ptl_peer->peer_sd) == false)
                break;
            /* progress any pending sends */
            ptl_peer->peer_send_frag = (mca_ptl_tcp_send_frag_t*)
                lam_list_remove_first(&ptl_peer->peer_frags);
        } while (NULL != ptl_peer->peer_send_frag);

        /* if nothing else to do unregister for send event notifications */
        if(NULL == ptl_peer->peer_send_frag) {
            lam_reactor_remove(&mca_ptl_tcp_module.tcp_reactor, sd, LAM_REACTOR_NOTIFY_SEND);
        }
        break;
    default:
        lam_output(0, "mca_ptl_tcp_peer_send_handler: invalid connection state (%d)", 
            ptl_peer->peer_state);
        lam_reactor_remove(&mca_ptl_tcp_module.tcp_reactor, sd, LAM_REACTOR_NOTIFY_SEND);
        break;
    }
    THREAD_UNLOCK(&ptl_peer->peer_lock);
}


/*
 * A file descriptor is in an erroneous state. Close the connection
 * and update the peers state.
 */
static void mca_ptl_tcp_peer_except_handler(mca_ptl_peer_t* ptl_peer, int sd)
{
    lam_output(0, "mca_ptl_tcp_peer_except_handler: closing connection");
    mca_ptl_tcp_peer_close_i(ptl_peer);
}


