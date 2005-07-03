/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include "include/ompi_socket_errno.h"
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETINET_TCP_H
#include <netinet/tcp.h>
#endif

#include "class/ompi_proc_table.h"
#include "util/output.h"

#include "mca/gpr/gpr.h"
#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "oob_tcp.h"
#include "oob_tcp_peer.h"

#define IMPORTANT_WINDOWS_COMMENT() \
            /* In windows, many of the socket functions return an EWOULDBLOCK instead of \
               things like EAGAIN, EINPROGRESS, etc. It has been verified that this will \
               not conflict with other error codes that are returned by these functions \
               under UNIX/Linux environments */

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
    opal_list_item_t,
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
    OBJ_CONSTRUCT(&(peer->peer_send_queue), opal_list_t);
    OBJ_CONSTRUCT(&(peer->peer_lock), opal_mutex_t);
    memset(&peer->peer_send_event, 0, sizeof(peer->peer_send_event));
    memset(&peer->peer_recv_event, 0, sizeof(peer->peer_recv_event));
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
    mca_oob_tcp_peer_shutdown(peer); 
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
    OPAL_THREAD_LOCK(&peer->peer_lock);
    switch(peer->peer_state) {
    case MCA_OOB_TCP_CONNECTING:
    case MCA_OOB_TCP_CONNECT_ACK:
    case MCA_OOB_TCP_CLOSED:
    case MCA_OOB_TCP_RESOLVE:
        /*
         * queue the message and attempt to resolve the peer address
         */
        opal_list_append(&peer->peer_send_queue, (opal_list_item_t*)msg);
        if(peer->peer_state == MCA_OOB_TCP_CLOSED) {
            peer->peer_state = MCA_OOB_TCP_RESOLVE;
            OPAL_THREAD_UNLOCK(&peer->peer_lock);
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
            opal_list_append(&peer->peer_send_queue, (opal_list_item_t*)msg);
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
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
    return rc;
}

/*
 * Lookup a peer by name, create one if it doesn't exist.
 * @param name  Peers globally unique identifier.
 * @retval      Pointer to the newly created struture or NULL on error.
 */
mca_oob_tcp_peer_t * mca_oob_tcp_peer_lookup(const orte_process_name_t* name)
{
    int rc;
    mca_oob_tcp_peer_t * peer, *old;
    opal_list_item_t* item;

    if (NULL == name) { /* can't look this one up */
        return NULL;
    }
    
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    peer = (mca_oob_tcp_peer_t*)opal_hash_table_get_proc(
       &mca_oob_tcp_component.tcp_peers, name);
    if(NULL != peer && memcmp(&peer->peer_name,name,sizeof(peer->peer_name)) == 0) {
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        return peer;
    }

    /* search the peer list - if we find it here this is a bug in the tree */
    for(item =  opal_list_get_first(&mca_oob_tcp_component.tcp_peer_list);
        item != opal_list_get_end(&mca_oob_tcp_component.tcp_peer_list);
        item =  opal_list_get_next(item)) {
        peer = (mca_oob_tcp_peer_t*)item;
        if (memcmp(&peer->peer_name, name, sizeof(peer->peer_name)) == 0) {
            OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
            return peer;
        }
    }

    /* allocate from free list */
    MCA_OOB_TCP_PEER_ALLOC(peer, rc);
    if(NULL == peer) {
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
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
    if(OMPI_SUCCESS != opal_hash_table_set_proc(&mca_oob_tcp_component.tcp_peers, 
        &peer->peer_name, peer)) {
        MCA_OOB_TCP_PEER_RETURN(peer);
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        return NULL;
    }

    /* if the peer list is over the maximum size, remove one unsed peer */
    opal_list_prepend(&mca_oob_tcp_component.tcp_peer_list, (opal_list_item_t *) peer);
    if(mca_oob_tcp_component.tcp_peer_limit > 0 &&
       (int)opal_list_get_size(&mca_oob_tcp_component.tcp_peer_list) > 
       mca_oob_tcp_component.tcp_peer_limit) {
        old = (mca_oob_tcp_peer_t *) 
              opal_list_get_last(&mca_oob_tcp_component.tcp_peer_list);
        while(1) {
            if(0 == opal_list_get_size(&(old->peer_send_queue)) &&
               NULL == peer->peer_recv_msg) { 
                opal_list_remove_item(&mca_oob_tcp_component.tcp_peer_list, 
                                      (opal_list_item_t *) old);
                MCA_OOB_TCP_PEER_RETURN(old);
                break;
            } else {
                old = (mca_oob_tcp_peer_t *) opal_list_get_prev(old);
                if(opal_list_get_begin(&mca_oob_tcp_component.tcp_peer_list) == (opal_list_item_t*)old) {
                    /* we tried, but we couldn't find one that was valid to get rid
                     * of. Oh well. */
                    break;
                }
            }
        }
    }
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
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
            "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_start_connect: socket() failed with errno=%d\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            ompi_socket_errno);
        mca_oob_tcp_peer_shutdown(peer);
        ompi_evtimer_add(&peer->peer_timer_event, &tv);
        return OMPI_ERR_UNREACH;
    }

    /* setup event callbacks */
    mca_oob_tcp_peer_event_init(peer);

    /* setup the socket as non-blocking */
    if((flags = fcntl(peer->peer_sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_connect: fcntl(F_GETFL) failed with errno=%d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            ompi_socket_errno);
    } else {
       flags |= O_NONBLOCK;
        if(fcntl(peer->peer_sd, F_SETFL, flags) < 0)
            ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_connect: fcntl(F_SETFL) failed with errno=%d\n", 
                ORTE_NAME_ARGS(orte_process_info.my_name),
                ORTE_NAME_ARGS(&(peer->peer_name)),
                ompi_socket_errno);
    }

    /* pick an address in round-robin fashion from the list exported by the peer */
    if((rc = mca_oob_tcp_addr_get_next(peer->peer_addr, &inaddr)) != OMPI_SUCCESS) {
        ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_start_connect: mca_oob_tcp_addr_get_next failed with error=%d",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            rc);
        return rc;
    }

    if(mca_oob_tcp_component.tcp_debug > 0) {
        ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_start_connect: connecting port %d to: %s:%d\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            ntohs(mca_oob_tcp_component.tcp_listen_port),
            inet_ntoa(inaddr.sin_addr),
            ntohs(inaddr.sin_port));
    }

    /* start the connect - will likely fail with EINPROGRESS */
    if(connect(peer->peer_sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        /* non-blocking so wait for completion */
        IMPORTANT_WINDOWS_COMMENT();
        if(ompi_socket_errno == EINPROGRESS || ompi_socket_errno == EWOULDBLOCK) {
            ompi_event_add(&peer->peer_send_event, 0);
            return OMPI_SUCCESS;
        }
        ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_start_connect: connect to %s:%d failed with errno=%d",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            inet_ntoa(inaddr.sin_addr),
            ntohs(inaddr.sin_port),
            ompi_socket_errno);
        mca_oob_tcp_peer_close(peer);
        return OMPI_ERR_UNREACH;
    }

    /* send our globally unique process identifier to the peer */
    if((rc = mca_oob_tcp_peer_send_connect_ack(peer)) == OMPI_SUCCESS) {
        peer->peer_state = MCA_OOB_TCP_CONNECT_ACK;
        ompi_event_add(&peer->peer_recv_event, 0);
    } else {
        ompi_output(0, 
            "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_start_connect: "
            "mca_oob_tcp_peer_send_connect_ack to %s:%d failed with errno=%d",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            inet_ntoa(inaddr.sin_addr),
            ntohs(inaddr.sin_port),
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
    if(getsockopt(peer->peer_sd, SOL_SOCKET, SO_ERROR, (char *)&so_error, &so_length) < 0) {
        ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_complete_connect: getsockopt() failed with errno=%d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            ompi_socket_errno);
        mca_oob_tcp_peer_close(peer);
        return;
    }
    if(so_error == EINPROGRESS) {
        ompi_event_add(&peer->peer_send_event, 0);
        return;
    } else if (so_error == ECONNREFUSED || so_error == ETIMEDOUT) {
        struct timeval tv = { 1,0 };
        ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_complete_connect: "
            "connection failed (errno=%d) - retrying (pid=%d)\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            so_error, getpid());
        mca_oob_tcp_peer_shutdown(peer);
        ompi_evtimer_add(&peer->peer_timer_event, &tv);
        return;
    } else if(so_error != 0) {
        ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_complete_connect: connect() failed with errno=%d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            so_error);
        mca_oob_tcp_peer_close(peer);
        return;
    }

    if(mca_oob_tcp_peer_send_connect_ack(peer) == OMPI_SUCCESS) {
        peer->peer_state = MCA_OOB_TCP_CONNECT_ACK;
        ompi_event_add(&peer->peer_recv_event, 0);
    } else {
        ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_complete_connect: unable to send connect ack.",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)));
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
    if(opal_list_get_size(&peer->peer_send_queue) > 0) {
        if(NULL == peer->peer_send_msg)
            peer->peer_send_msg = (mca_oob_tcp_msg_t*)
                opal_list_remove_first(&peer->peer_send_queue);
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
    if(mca_oob_tcp_component.tcp_debug > 0) {
        ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_close(%p) sd %d state %d\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            peer,
            peer->peer_sd,
            peer->peer_state);
    }

    /* if we lose the connection to the seed - abort */
    if(memcmp(&peer->peer_name,&mca_oob_name_seed,sizeof(mca_oob_name_seed)) == 0) {
        orte_errmgr.abort();
    }

    mca_oob_tcp_peer_shutdown(peer);
}

void mca_oob_tcp_peer_shutdown(mca_oob_tcp_peer_t* peer)
{
    /* giving up and cleanup any pending messages */
    if(peer->peer_retries++ > mca_oob_tcp_component.tcp_peer_retries) {
        mca_oob_tcp_msg_t *msg = peer->peer_send_msg;
        while(msg != NULL) {
            msg->msg_rc = OMPI_ERR_UNREACH;
            mca_oob_tcp_msg_complete(msg, &peer->peer_name);
            msg = (mca_oob_tcp_msg_t*)opal_list_remove_first(&peer->peer_send_queue);
        }
        peer->peer_send_msg = NULL;
    }

    if (peer->peer_sd >= 0) {
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
    mca_oob_tcp_hdr_t hdr;
    memset(&hdr,0,sizeof(hdr));
    if (NULL == orte_process_info.my_name) {  /* my name isn't defined yet */
        hdr.msg_src = *MCA_OOB_NAME_ANY;
    } else {
        hdr.msg_src = *(orte_process_info.my_name);
    }
    hdr.msg_dst = peer->peer_name;
    hdr.msg_type = MCA_OOB_TCP_CONNECT;
    MCA_OOB_TCP_HDR_HTON(&hdr);
    if(mca_oob_tcp_peer_send_blocking(peer, &hdr, sizeof(hdr)) != sizeof(hdr)) {
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
    mca_oob_tcp_hdr_t hdr;
    if((mca_oob_tcp_peer_recv_blocking(peer, &hdr, sizeof(hdr))) != sizeof(hdr)) {
        mca_oob_tcp_peer_close(peer);
        return OMPI_ERR_UNREACH;
    }
    MCA_OOB_TCP_HDR_NTOH(&hdr);
    if(hdr.msg_type != MCA_OOB_TCP_CONNECT) {
        ompi_output(0, "mca_oob_tcp_peer_recv_connect_ack: invalid header type: %d\n", hdr.msg_type);
        mca_oob_tcp_peer_close(peer);
        return OMPI_ERR_UNREACH;
    }
                                                                                                            
    /* compare the peers name to the expected value */
    if(memcmp(&peer->peer_name, &hdr.msg_src, sizeof(orte_process_name_t)) != 0) {
        ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_recv_connect_ack: "
            "received unexpected process identifier [%d,%d,%d]\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            ORTE_NAME_ARGS(&(hdr.msg_src)));
        mca_oob_tcp_peer_close(peer);
        return OMPI_ERR_UNREACH;
    }

    /* if we have a wildcard name - use the name returned by the peer */
    if(orte_process_info.my_name == NULL) {
        orte_ns.create_process_name(&orte_process_info.my_name, 
            hdr.msg_dst.cellid, hdr.msg_dst.jobid, hdr.msg_dst.vpid);
    } else if(orte_ns.compare(ORTE_NS_CMP_ALL, orte_process_info.my_name, &mca_oob_name_any) == 0) {
        *orte_process_info.my_name = hdr.msg_dst;
    }

    /* connected */
    mca_oob_tcp_peer_connected(peer);
    if(mca_oob_tcp_component.tcp_debug > 0) {
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
        int retval = recv(peer->peer_sd,(char *)ptr+cnt, size-cnt, 0);

        /* remote closed connection */
        if(retval == 0) {
            if(mca_oob_tcp_component.tcp_debug > 0) {
                ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_recv_blocking: "
                    "peer closed connection: peer state %d",
                    ORTE_NAME_ARGS(orte_process_info.my_name),
                    ORTE_NAME_ARGS(&(peer->peer_name)),
                    peer->peer_state);
            }
            mca_oob_tcp_peer_close(peer);
            return -1;
        }

        /* socket is non-blocking so handle errors */
        if(retval < 0) {
            IMPORTANT_WINDOWS_COMMENT();
            if(ompi_socket_errno != EINTR && ompi_socket_errno != EAGAIN && ompi_socket_errno != EWOULDBLOCK) {
                ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_recv_blocking: recv() failed with errno=%d\n",
                    ORTE_NAME_ARGS(orte_process_info.my_name),
                    ORTE_NAME_ARGS(&(peer->peer_name)),
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
        int retval = send(peer->peer_sd, (char *)ptr+cnt, size-cnt, 0);
        if(retval < 0) {
            IMPORTANT_WINDOWS_COMMENT();
            if(ompi_socket_errno != EINTR && ompi_socket_errno != EAGAIN && ompi_socket_errno != EWOULDBLOCK) {
                ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_send_blocking: send() failed with errno=%d\n",
                    ORTE_NAME_ARGS(orte_process_info.my_name),
                    ORTE_NAME_ARGS(&(peer->peer_name)),
                    ompi_socket_errno);
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
    hdr.msg_src = *orte_process_info.my_name;
    hdr.msg_dst = peer->peer_name;
    hdr.msg_type = MCA_OOB_TCP_IDENT;
    hdr.msg_size = 0;
    hdr.msg_tag = 0;
    MCA_OOB_TCP_HDR_HTON(&hdr);
    if(mca_oob_tcp_peer_send_blocking(peer, &hdr, sizeof(hdr)) != sizeof(hdr))
        return OMPI_ERR_UNREACH;
    return OMPI_SUCCESS;
}


/* static void mca_oob_tcp_peer_recv_ident(mca_oob_tcp_peer_t* peer, mca_oob_tcp_hdr_t* hdr) */
/* { */
/*     OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock); */
/*     ompi_rb_tree_delete(&mca_oob_tcp_component.tcp_peer_tree, &peer->peer_name); */
/*     peer->peer_name = hdr->msg_src; */
/*     ompi_rb_tree_insert(&mca_oob_tcp_component.tcp_peer_tree, &peer->peer_name, peer); */
/*     OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock); */
/* } */


/*
 * Dispatch to the appropriate action routine based on the state
 * of the connection with the peer.
 */

static void mca_oob_tcp_peer_recv_handler(int sd, short flags, void* user)
{
    mca_oob_tcp_peer_t* peer = (mca_oob_tcp_peer_t *)user;
    OPAL_THREAD_LOCK(&peer->peer_lock);
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
                    ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_recv_handler: unable to allocate recv message\n",
                        ORTE_NAME_ARGS(orte_process_info.my_name),
                        ORTE_NAME_ARGS(&(peer->peer_name)));
                    return;
                }
                msg->msg_type = MCA_OOB_TCP_UNEXPECTED;
                msg->msg_rc = 0;
                msg->msg_flags = 0;
                msg->msg_peer = peer->peer_name;
                msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg,2);
                msg->msg_rwbuf = NULL;
                msg->msg_rwiov->iov_base = (ompi_iov_base_ptr_t)msg->msg_rwbuf;
                msg->msg_rwiov->iov_len = 1;
                msg->msg_rwcnt = msg->msg_rwnum = 1;
                msg->msg_rwptr = msg->msg_rwiov;
                msg->msg_rwiov[0].iov_base = (ompi_iov_base_ptr_t)&msg->msg_hdr;
                msg->msg_rwiov[0].iov_len = sizeof(msg->msg_hdr);
                peer->peer_recv_msg = msg;
            }

            if (peer->peer_recv_msg && 
                mca_oob_tcp_msg_recv_handler(peer->peer_recv_msg, peer)) {
               mca_oob_tcp_msg_t* msg = peer->peer_recv_msg;
               peer->peer_recv_msg = NULL;
               OPAL_THREAD_UNLOCK(&peer->peer_lock);
               mca_oob_tcp_msg_recv_complete(msg, peer);
               return;
            }
            break;
        }
        default: 
        {
            ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_recv_handler: invalid socket state(%d)", 
                    ORTE_NAME_ARGS(orte_process_info.my_name),
                    ORTE_NAME_ARGS(&(peer->peer_name)),
                    peer->peer_state);
            mca_oob_tcp_peer_close(peer);
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
}

/*
 * A file descriptor is available/ready for send. Check the state
 * of the socket and take the appropriate action.
 */
static void mca_oob_tcp_peer_send_handler(int sd, short flags, void* user)
{
    mca_oob_tcp_peer_t* peer = (mca_oob_tcp_peer_t *)user;
    OPAL_THREAD_LOCK(&peer->peer_lock);
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
                opal_list_remove_first(&peer->peer_send_queue);
        }
        
        /* if nothing else to do unregister for send event notifications */
        if(NULL == peer->peer_send_msg) {
            ompi_event_del(&peer->peer_send_event);
        }
        break;
        }
    default:
        ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_send_handler: invalid connection state (%d)",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(&(peer->peer_name)),
            peer->peer_state);
        ompi_event_del(&peer->peer_send_event);
        break;
    }
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
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
        ompi_output(0, "mca_oob_tcp_peer_dump: fcntl(F_GETFL) failed with errno=%d\n", ompi_socket_errno);
    }
                                                                                                            
#if defined(SO_SNDBUF)
    optlen = sizeof(sndbuf);
    if(getsockopt(peer->peer_sd, SOL_SOCKET, SO_SNDBUF, (char *)&sndbuf, &optlen) < 0) {
        ompi_output(0, "mca_oob_tcp_peer_dump: SO_SNDBUF option: errno %d\n", ompi_socket_errno);
    }
#else
    sndbuf = -1;
#endif
#if defined(SO_RCVBUF)
    optlen = sizeof(rcvbuf);
    if(getsockopt(peer->peer_sd, SOL_SOCKET, SO_RCVBUF, (char *)&rcvbuf, &optlen) < 0) {
        ompi_output(0, "mca_oob_tcp_peer_dump: SO_RCVBUF option: errno %d\n", ompi_socket_errno);
    }
#else
    rcvbuf = -1;
#endif
#if defined(TCP_NODELAY)
    optlen = sizeof(nodelay);
    if(getsockopt(peer->peer_sd, IPPROTO_TCP, TCP_NODELAY, (char *)&nodelay, &optlen) < 0) {
        ompi_output(0, "mca_oob_tcp_peer_dump: TCP_NODELAY option: errno %d\n", ompi_socket_errno);
    }
#else
    nodelay = 0;
#endif

    sprintf(buff, "[%lu,%lu,%lu]-[%lu,%lu,%lu] %s: %s - %s nodelay %d sndbuf %d rcvbuf %d flags %08x\n",
        ORTE_NAME_ARGS(orte_process_info.my_name),
        ORTE_NAME_ARGS(&(peer->peer_name)),
        msg, src, dst, nodelay, sndbuf, rcvbuf, flags);
    ompi_output(0, buff);
}


/*
 *  Accept incoming connection - if not already connected.
 */

bool mca_oob_tcp_peer_accept(mca_oob_tcp_peer_t* peer, int sd)
{
    int cmpval;
    OPAL_THREAD_LOCK(&peer->peer_lock);
    cmpval = orte_ns.compare(ORTE_NS_CMP_ALL, &peer->peer_name, orte_process_info.my_name);
    if ((peer->peer_state == MCA_OOB_TCP_CLOSED) ||
        (peer->peer_state == MCA_OOB_TCP_RESOLVE) ||
        (peer->peer_state != MCA_OOB_TCP_CONNECTED &&
         cmpval < 0)) {

        if(peer->peer_state != MCA_OOB_TCP_CLOSED) {
            mca_oob_tcp_peer_close(peer);
        }
        peer->peer_sd = sd;
        mca_oob_tcp_peer_event_init(peer);

        if(mca_oob_tcp_peer_send_connect_ack(peer) != OMPI_SUCCESS) {
            ompi_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_peer_accept: "
                "mca_oob_tcp_peer_send_connect_ack failed\n",
                ORTE_NAME_ARGS(orte_process_info.my_name),
                ORTE_NAME_ARGS(&(peer->peer_name)));
            mca_oob_tcp_peer_close(peer);
            OPAL_THREAD_UNLOCK(&peer->peer_lock);
            return false;
        }

        mca_oob_tcp_peer_connected(peer);
        ompi_event_add(&peer->peer_recv_event, 0);
        if(mca_oob_tcp_component.tcp_debug > 0) {
            mca_oob_tcp_peer_dump(peer, "accepted");
        }
        OPAL_THREAD_UNLOCK(&peer->peer_lock);
        return true;
    }
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
    return false;
}


/*
 * resolve process name to an actual internet address.
 */

void mca_oob_tcp_peer_resolved(mca_oob_tcp_peer_t* peer, mca_oob_tcp_addr_t* addr)
{
    OPAL_THREAD_LOCK(&peer->peer_lock);
    peer->peer_addr = addr;
    if((peer->peer_state == MCA_OOB_TCP_RESOLVE) ||
       (peer->peer_state == MCA_OOB_TCP_CLOSED && opal_list_get_size(&peer->peer_send_queue))) {
        mca_oob_tcp_peer_start_connect(peer);
    }
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
}

/*
 * Callback on timeout - retry connection attempt.
 */

static void mca_oob_tcp_peer_timer_handler(int sd, short flags, void* user)
{
    /* start the connection to the peer */
    mca_oob_tcp_peer_t* peer = (mca_oob_tcp_peer_t*)user;
    ompi_output(0, "mca_oob_tcp_peer_timer_handler\n");
    OPAL_THREAD_LOCK(&peer->peer_lock);
    if(peer->peer_state == MCA_OOB_TCP_CLOSED)
        mca_oob_tcp_peer_start_connect(peer);
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
}

/*
 * Remove any references to the indicated message.
 */

void mca_oob_tcp_peer_dequeue_msg(mca_oob_tcp_peer_t* peer, mca_oob_tcp_msg_t* msg)
{
    opal_list_item_t* item;
    OPAL_THREAD_LOCK(&peer->peer_lock);
    if (peer->peer_send_msg == msg)
        peer->peer_send_msg = NULL;
    if (peer->peer_recv_msg == msg)
        peer->peer_recv_msg = NULL;

    for( item =  opal_list_get_first(&peer->peer_send_queue);
         item != opal_list_get_end(&peer->peer_send_queue);
         item = opal_list_get_next(item)) {
        if(item == (opal_list_item_t*)msg) {
            opal_list_remove_item(&peer->peer_send_queue, item);
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
}


