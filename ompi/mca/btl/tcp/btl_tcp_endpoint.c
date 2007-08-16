/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * In windows, many of the socket functions return an EWOULDBLOCK
 * instead of \ things like EAGAIN, EINPROGRESS, etc. It has been
 * verified that this will \ not conflict with other error codes that
 * are returned by these functions \ under UNIX/Linux environments 
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "opal/opal_socket_errno.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_NETINET_TCP_H
#include <netinet/tcp.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#ifdef HAVE_TIME_H
#include <time.h>
#endif  /* HAVE_TIME_H */

#include "ompi/types.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_tcp.h"
#include "btl_tcp_endpoint.h" 
#include "btl_tcp_proc.h"
#include "btl_tcp_frag.h"
#include "btl_tcp_addr.h"


/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_btl_tcp_endpoint_construct(mca_btl_tcp_endpoint_t* endpoint)
{
    endpoint->endpoint_btl = NULL;
    endpoint->endpoint_proc = NULL;
    endpoint->endpoint_addr = NULL;
    endpoint->endpoint_sd = -1;
    endpoint->endpoint_send_frag = 0;
    endpoint->endpoint_recv_frag = 0;
    endpoint->endpoint_send_event.ev_flags = 0;
    endpoint->endpoint_recv_event.ev_flags = 0;
    endpoint->endpoint_state = MCA_BTL_TCP_CLOSED;
    endpoint->endpoint_retries = 0;
    endpoint->endpoint_nbo = false;
#if MCA_BTL_TCP_ENDPOINT_CACHE
    endpoint->endpoint_cache        = NULL;
    endpoint->endpoint_cache_pos    = NULL;
    endpoint->endpoint_cache_length = 0;
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */
    OBJ_CONSTRUCT(&endpoint->endpoint_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_send_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_recv_lock, opal_mutex_t);
}

/*
 * Destroy a endpoint
 *
 */


static void mca_btl_tcp_endpoint_destruct(mca_btl_tcp_endpoint_t* endpoint)
{
    mca_btl_tcp_proc_remove(endpoint->endpoint_proc, endpoint);
    mca_btl_tcp_endpoint_close(endpoint);
    OBJ_DESTRUCT(&endpoint->endpoint_frags);
    OBJ_DESTRUCT(&endpoint->endpoint_send_lock);
    OBJ_DESTRUCT(&endpoint->endpoint_recv_lock);
}

OBJ_CLASS_INSTANCE(
    mca_btl_tcp_endpoint_t, 
    opal_list_item_t, 
    mca_btl_tcp_endpoint_construct, 
    mca_btl_tcp_endpoint_destruct);


static void mca_btl_tcp_endpoint_construct(mca_btl_base_endpoint_t* btl_endpoint);
static void mca_btl_tcp_endpoint_destruct(mca_btl_base_endpoint_t* btl_endpoint);
static int  mca_btl_tcp_endpoint_start_connect(mca_btl_base_endpoint_t*);
static void mca_btl_tcp_endpoint_connected(mca_btl_base_endpoint_t*);
static void mca_btl_tcp_endpoint_recv_handler(int sd, short flags, void* user);
static void mca_btl_tcp_endpoint_send_handler(int sd, short flags, void* user);

/*
 * Diagnostics: change this to "1" to enable the function
 * mca_btl_tcp_endpoint_dump(), below
 */
#define WANT_PEER_DUMP 0
/*
 * diagnostics
 */

#if WANT_PEER_DUMP
static void mca_btl_tcp_endpoint_dump(mca_btl_base_endpoint_t* btl_endpoint, const char* msg)
{
    char src[64];
    char dst[64];
    int sndbuf,rcvbuf,nodelay,flags;
    struct sockaddr_in inaddr;
    opal_socklen_t obtlen;
    opal_socklen_t addrlen = sizeof(struct sockaddr_in);

    getsockname(btl_endpoint->endpoint_sd, (struct sockaddr*)&inaddr, &addrlen);
    sprintf(src, "%s", inet_ntoa(inaddr.sin_addr));
    getpeername(btl_endpoint->endpoint_sd, (struct sockaddr*)&inaddr, &addrlen);
    sprintf(dst, "%s", inet_ntoa(inaddr.sin_addr));

    if((flags = fcntl(btl_endpoint->endpoint_sd, F_GETFL, 0)) < 0) {
        BTL_ERROR(("fcntl(F_GETFL) failed with errno=%d", opal_socket_errno));
    }

#if defined(SO_SNDBUF)
    obtlen = sizeof(sndbuf);
    if(getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_SNDBUF, (char *)&sndbuf, &obtlen) < 0) {
        BTL_ERROR(("SO_SNDBUF option: errno %d", opal_socket_errno));
    }
#else
    sndbuf = -1;
#endif
#if defined(SO_RCVBUF)
    obtlen = sizeof(rcvbuf);
    if(getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_RCVBUF, (char *)&rcvbuf, &obtlen) < 0) {
        BTL_ERROR(("SO_RCVBUF option: errno %d", opal_socket_errno));
    }
#else
    rcvbuf = -1;
#endif
#if defined(TCP_NODELAY)
    obtlen = sizeof(nodelay);
    if(getsockopt(btl_endpoint->endpoint_sd, IPPROTO_TCP, TCP_NODELAY, (char *)&nodelay, &obtlen) < 0) {
        BTL_ERROR(("TCP_NODELAY option: errno %d", opal_socket_errno));
    }
#else
    nodelay = 0;
#endif

    BTL_DEBUG(("%s: %s - %s nodelay %d sndbuf %d rcvbuf %d flags %08x", 
        msg, src, dst, nodelay, sndbuf, rcvbuf, flags));
}
#endif

/*
 * Initialize events to be used by the endpoint instance for TCP select/poll callbacks.
 */

static inline void mca_btl_tcp_endpoint_event_init(mca_btl_base_endpoint_t* btl_endpoint, int sd)
{
#if MCA_BTL_TCP_ENDPOINT_CACHE
    btl_endpoint->endpoint_cache     = (char*)malloc(mca_btl_tcp_component.tcp_endpoint_cache);
    btl_endpoint->endpoint_cache_pos = btl_endpoint->endpoint_cache;
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */

    opal_event_set( &btl_endpoint->endpoint_recv_event, 
		    btl_endpoint->endpoint_sd, 
		    OPAL_EV_READ|OPAL_EV_PERSIST, 
		    mca_btl_tcp_endpoint_recv_handler,
		    btl_endpoint );
    opal_event_set( &btl_endpoint->endpoint_send_event, 
		    btl_endpoint->endpoint_sd, 
		    OPAL_EV_WRITE|OPAL_EV_PERSIST, 
		    mca_btl_tcp_endpoint_send_handler,
		    btl_endpoint);
}


/*
 * Attempt to send a fragment using a given endpoint. If the endpoint is not connected,
 * queue the fragment and start the connection as required.
 */

int mca_btl_tcp_endpoint_send(mca_btl_base_endpoint_t* btl_endpoint, mca_btl_tcp_frag_t* frag)
{
    int rc = OMPI_SUCCESS;
    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
    switch(btl_endpoint->endpoint_state) {
    case MCA_BTL_TCP_CONNECTING:
    case MCA_BTL_TCP_CONNECT_ACK:
    case MCA_BTL_TCP_CLOSED:
        opal_list_append(&btl_endpoint->endpoint_frags, (opal_list_item_t*)frag);
        if(btl_endpoint->endpoint_state == MCA_BTL_TCP_CLOSED)
            rc = mca_btl_tcp_endpoint_start_connect(btl_endpoint);
        break;
    case MCA_BTL_TCP_FAILED:
        rc = OMPI_ERR_UNREACH;
        break;
    case MCA_BTL_TCP_CONNECTED:
        if (btl_endpoint->endpoint_send_frag == NULL) {
            if(frag->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY &&
               mca_btl_tcp_frag_send(frag, btl_endpoint->endpoint_sd)) {
                OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, frag->rc);
                return OMPI_SUCCESS;
            } else {
                btl_endpoint->endpoint_send_frag = frag;
                opal_event_add(&btl_endpoint->endpoint_send_event, 0);
            }
        } else {
            opal_list_append(&btl_endpoint->endpoint_frags, (opal_list_item_t*)frag);
        }
        break;
    case MCA_BTL_TCP_SHUTDOWN:
        rc = OMPI_ERROR;
        break;
    }
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
    return rc;
}


/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the endpoints endpoint.
 */
static int mca_btl_tcp_endpoint_send_blocking(mca_btl_base_endpoint_t* btl_endpoint, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    while(cnt < size) {
        int retval = send(btl_endpoint->endpoint_sd, (const char *)ptr+cnt, size-cnt, 0);
        if(retval < 0) {
            if(opal_socket_errno != EINTR && opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                BTL_ERROR(("send() failed with errno=%d",opal_socket_errno));
                mca_btl_tcp_endpoint_close(btl_endpoint);
                return -1;
            }
            continue;
        }
        cnt += retval;
    }
    return cnt;
}


/*
 * Send the globally unique identifier for this process to a endpoint on 
 * a newly connected socket.
 */

static int mca_btl_tcp_endpoint_send_connect_ack(mca_btl_base_endpoint_t* btl_endpoint)
{
    /* send process identifier to remote endpoint */
    mca_btl_tcp_proc_t* btl_proc = mca_btl_tcp_proc_local();
    orte_process_name_t guid = btl_proc->proc_name;

    ORTE_PROCESS_NAME_HTON(guid);
    if(mca_btl_tcp_endpoint_send_blocking(btl_endpoint, &guid, sizeof(guid)) != 
          sizeof(guid)) {
        return OMPI_ERR_UNREACH;
    }
    return OMPI_SUCCESS;
}

/*
 * Check the state of this endpoint. If the incoming connection request matches
 * our endpoints address, check the state of our connection:
 * (1) if a connection has not been attempted, accept the connection
 * (2) if a connection has not been established, and the endpoints process identifier
 *     is less than the local process, accept the connection
 * otherwise, reject the connection and continue with the current connection 
 */

bool mca_btl_tcp_endpoint_accept(mca_btl_base_endpoint_t* btl_endpoint, struct sockaddr_in* addr, int sd)
{
    mca_btl_tcp_addr_t* btl_addr;
    mca_btl_tcp_proc_t* this_proc = mca_btl_tcp_proc_local();
    orte_ns_cmp_bitmask_t mask = ORTE_NS_CMP_ALL;
    int cmpval;

    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_recv_lock);
    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
    if((btl_addr = btl_endpoint->endpoint_addr) != NULL  &&
        btl_addr->addr_inet.s_addr == addr->sin_addr.s_addr) {
        mca_btl_tcp_proc_t *endpoint_proc = btl_endpoint->endpoint_proc;
        cmpval = orte_ns.compare_fields(mask, 
                                 &endpoint_proc->proc_ompi->proc_name,
                                 &this_proc->proc_ompi->proc_name);
        if((btl_endpoint->endpoint_sd < 0) ||
           (btl_endpoint->endpoint_state != MCA_BTL_TCP_CONNECTED &&
            cmpval < 0)) {
            mca_btl_tcp_endpoint_close(btl_endpoint);
            btl_endpoint->endpoint_sd = sd;
            if(mca_btl_tcp_endpoint_send_connect_ack(btl_endpoint) != OMPI_SUCCESS) {
                 mca_btl_tcp_endpoint_close(btl_endpoint);
                 OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                 OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
                 return false;
            }
            mca_btl_tcp_endpoint_event_init(btl_endpoint, sd);
            opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
            mca_btl_tcp_endpoint_connected(btl_endpoint);
#if OMPI_ENABLE_DEBUG && WANT_PEER_DUMP
            mca_btl_tcp_endpoint_dump(btl_endpoint, "accepted");
#endif
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
            return true;
        }
    }
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
    return false;
}


/*
 * Remove any event registrations associated with the socket
 * and update the endpoint state to reflect the connection has
 * been closed.
 */

void mca_btl_tcp_endpoint_close(mca_btl_base_endpoint_t* btl_endpoint)
{
    if(btl_endpoint->endpoint_sd >= 0) {
        opal_event_del(&btl_endpoint->endpoint_recv_event);
        opal_event_del(&btl_endpoint->endpoint_send_event);
        CLOSE_THE_SOCKET(btl_endpoint->endpoint_sd);
        btl_endpoint->endpoint_sd = -1;
#if MCA_BTL_TCP_ENDPOINT_CACHE
        free( btl_endpoint->endpoint_cache );
        btl_endpoint->endpoint_cache        = NULL;
        btl_endpoint->endpoint_cache_pos    = NULL;
        btl_endpoint->endpoint_cache_length = 0;
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */
    }
    btl_endpoint->endpoint_state = MCA_BTL_TCP_CLOSED;
    btl_endpoint->endpoint_retries++;
}

void mca_btl_tcp_endpoint_shutdown(mca_btl_base_endpoint_t* btl_endpoint)
{
    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_recv_lock);
    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
    mca_btl_tcp_endpoint_close(btl_endpoint);
    btl_endpoint->endpoint_state = MCA_BTL_TCP_SHUTDOWN;
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
}


/*
 *  Setup endpoint state to reflect that connection has been established,
 *  and start any pending sends.
 */

static void mca_btl_tcp_endpoint_connected(mca_btl_base_endpoint_t* btl_endpoint)
{
    /* setup socket options */
    btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECTED;
    btl_endpoint->endpoint_retries = 0;
    if(opal_list_get_size(&btl_endpoint->endpoint_frags) > 0) {
        if(NULL == btl_endpoint->endpoint_send_frag)
            btl_endpoint->endpoint_send_frag = (mca_btl_tcp_frag_t*)
                opal_list_remove_first(&btl_endpoint->endpoint_frags);
        opal_event_add(&btl_endpoint->endpoint_send_event, 0);
    }
}


/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the endpoints endpoint.
 */
static int mca_btl_tcp_endpoint_recv_blocking(mca_btl_base_endpoint_t* btl_endpoint, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    while(cnt < size) {
        int retval = recv(btl_endpoint->endpoint_sd, (char *)ptr+cnt, size-cnt, 0);

        /* remote closed connection */
        if(retval == 0) {
            mca_btl_tcp_endpoint_close(btl_endpoint);
            return -1;
        }

        /* socket is non-blocking so handle errors */
        if(retval < 0) {
            if(opal_socket_errno != EINTR && opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                BTL_ERROR(("recv() failed with errno=%d",opal_socket_errno));
                mca_btl_tcp_endpoint_close(btl_endpoint);
                return -1;
            }
            continue;
        }
        cnt += retval;
    }
    return cnt;
}



/*
 *  Receive the endpoints globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 */

static int mca_btl_tcp_endpoint_recv_connect_ack(mca_btl_base_endpoint_t* btl_endpoint)
{
    orte_process_name_t guid;
    mca_btl_tcp_proc_t* btl_proc = btl_endpoint->endpoint_proc;

    if((mca_btl_tcp_endpoint_recv_blocking(btl_endpoint, &guid, sizeof(orte_process_name_t))) != sizeof(orte_process_name_t)) {
        return OMPI_ERR_UNREACH;
    }
    ORTE_PROCESS_NAME_NTOH(guid);

    /* compare this to the expected values */
    if(memcmp(&btl_proc->proc_name, &guid, sizeof(orte_process_name_t)) != 0) {
        BTL_ERROR(("received unexpected process identifier [%lu,%lu,%lu]", 
            ORTE_NAME_ARGS(&guid)));
        mca_btl_tcp_endpoint_close(btl_endpoint);
        return OMPI_ERR_UNREACH;
    }

    /* connected */
    mca_btl_tcp_endpoint_connected(btl_endpoint);
#if OMPI_ENABLE_DEBUG && WANT_PEER_DUMP
    mca_btl_tcp_endpoint_dump(btl_endpoint, "connected");
#endif
    return OMPI_SUCCESS;
}


void mca_btl_tcp_set_socket_options(int sd)
{
    int optval;
#if defined(TCP_NODELAY)
    optval = 1;
    if(setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (char *)&optval, sizeof(optval)) < 0) {
        BTL_ERROR(("setsockopt(TCP_NODELAY) failed with errno=%d", opal_socket_errno));
    }
#endif
#if defined(SO_SNDBUF)
    if(mca_btl_tcp_component.tcp_sndbuf > 0 &&
       setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (char *)&mca_btl_tcp_component.tcp_sndbuf, sizeof(int)) < 0) {
        BTL_ERROR(("setsockopt(SO_SNDBUF) failed with errno %d", opal_socket_errno));
    }
#endif
#if defined(SO_RCVBUF)
    if(mca_btl_tcp_component.tcp_rcvbuf > 0 &&
       setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (char *)&mca_btl_tcp_component.tcp_rcvbuf, sizeof(int)) < 0) {
        BTL_ERROR(("setsockopt(SO_RCVBUF) failed with errno %d", opal_socket_errno));
    }
#endif
}



/*
 *  Start a connection to the endpoint. This will likely not complete,
 *  as the socket is set to non-blocking, so register for event
 *  notification of connect completion. On connection we send
 *  our globally unique process identifier to the endpoint and wait for
 *  the endpoints response.
 */

static int mca_btl_tcp_endpoint_start_connect(mca_btl_base_endpoint_t* btl_endpoint)
{
    int rc,flags;
    struct sockaddr_in endpoint_addr;

    btl_endpoint->endpoint_sd = socket(AF_INET, SOCK_STREAM, 0);
    if (btl_endpoint->endpoint_sd < 0) {
        btl_endpoint->endpoint_retries++;
        return OMPI_ERR_UNREACH;
    }

    /* setup socket buffer sizes */
    mca_btl_tcp_set_socket_options(btl_endpoint->endpoint_sd);

    /* setup event callbacks */
    mca_btl_tcp_endpoint_event_init(btl_endpoint, btl_endpoint->endpoint_sd);

    /* setup the socket as non-blocking */
    if((flags = fcntl(btl_endpoint->endpoint_sd, F_GETFL, 0)) < 0) {
        BTL_ERROR(("fcntl(F_GETFL) failed with errno=%d", opal_socket_errno));
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(btl_endpoint->endpoint_sd, F_SETFL, flags) < 0)
            BTL_ERROR(("fcntl(F_SETFL) failed with errno=%d", opal_socket_errno));
    }

    /* start the connect - will likely fail with EINPROGRESS */
    endpoint_addr.sin_family = AF_INET;
    endpoint_addr.sin_addr = btl_endpoint->endpoint_addr->addr_inet;
    endpoint_addr.sin_port = btl_endpoint->endpoint_addr->addr_port;
    if(connect(btl_endpoint->endpoint_sd, (struct sockaddr*)&endpoint_addr, sizeof(endpoint_addr)) < 0) {
        /* non-blocking so wait for completion */
        if(opal_socket_errno == EINPROGRESS || opal_socket_errno == EWOULDBLOCK) {
            btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECTING;
            opal_event_add(&btl_endpoint->endpoint_send_event, 0);
            return OMPI_SUCCESS;
        }
        mca_btl_tcp_endpoint_close(btl_endpoint);
        btl_endpoint->endpoint_retries++;
        return OMPI_ERR_UNREACH;
    }

    /* send our globally unique process identifier to the endpoint */
    if((rc = mca_btl_tcp_endpoint_send_connect_ack(btl_endpoint)) == OMPI_SUCCESS) {
        btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECT_ACK;
        opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
    } else {
        mca_btl_tcp_endpoint_close(btl_endpoint);
    }
    return rc;
}


/*
 * Check the status of the connection. If the connection failed, will retry
 * later. Otherwise, send this processes identifier to the endpoint on the 
 * newly connected socket.
 */

static void mca_btl_tcp_endpoint_complete_connect(mca_btl_base_endpoint_t* btl_endpoint)
{
    int so_error = 0;
    opal_socklen_t so_length = sizeof(so_error);

    /* unregister from receiving event notifications */
    opal_event_del(&btl_endpoint->endpoint_send_event);

    /* check connect completion status */
    if(getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_ERROR, (char *)&so_error, &so_length) < 0) {
        BTL_ERROR(("getsockopt() failed with errno=%d", opal_socket_errno));
        mca_btl_tcp_endpoint_close(btl_endpoint);
        return;
    }
    if(so_error == EINPROGRESS || so_error == EWOULDBLOCK) {
        opal_event_add(&btl_endpoint->endpoint_send_event, 0);
        return;
    }
    if(so_error != 0) {
        BTL_ERROR(("connect() failed with errno=%d", so_error));
        mca_btl_tcp_endpoint_close(btl_endpoint);
        return;
    }

    if(mca_btl_tcp_endpoint_send_connect_ack(btl_endpoint) == OMPI_SUCCESS) {
        btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECT_ACK;
        opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
    } else {
        mca_btl_tcp_endpoint_close(btl_endpoint);
    }
}


/*
 * A file descriptor is available/ready for recv. Check the state 
 * of the socket and take the appropriate action.
 */

static void mca_btl_tcp_endpoint_recv_handler(int sd, short flags, void* user)
{
    mca_btl_base_endpoint_t* btl_endpoint = (mca_btl_base_endpoint_t *)user;
    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_recv_lock);
    switch(btl_endpoint->endpoint_state) {
    case MCA_BTL_TCP_CONNECT_ACK:
        {
            mca_btl_tcp_endpoint_recv_connect_ack(btl_endpoint);
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
            break;
        }
    case MCA_BTL_TCP_CONNECTED:
        {
            mca_btl_tcp_frag_t* frag;

            frag = btl_endpoint->endpoint_recv_frag;
            if(NULL == frag) {
                int rc;
                if(mca_btl_tcp_module.super.btl_max_send_size > 
                   mca_btl_tcp_module.super.btl_eager_limit) { 
                    MCA_BTL_TCP_FRAG_ALLOC_MAX(frag, rc);
                } else { 
                    MCA_BTL_TCP_FRAG_ALLOC_EAGER(frag, rc);
                }
                
                if(NULL == frag) {
                    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
                    return;
                }
                MCA_BTL_TCP_FRAG_INIT_DST(frag, btl_endpoint);
            }

#if MCA_BTL_TCP_ENDPOINT_CACHE
            assert( 0 == btl_endpoint->endpoint_cache_length );
        data_still_pending_on_endpoint:
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */
            /* check for completion of non-blocking recv on the current fragment */
            if(mca_btl_tcp_frag_recv(frag, sd) == false) {
                btl_endpoint->endpoint_recv_frag = frag;
            } else {
                btl_endpoint->endpoint_recv_frag = NULL;
		if( MCA_BTL_TCP_HDR_TYPE_SEND == frag->hdr.type ) {
		  mca_btl_base_recv_reg_t* reg = frag->btl->tcp_reg + frag->hdr.base.tag;
		  reg->cbfunc(&frag->btl->super, frag->hdr.base.tag, &frag->base, reg->cbdata);
                }
#if MCA_BTL_TCP_ENDPOINT_CACHE
                if( 0 != btl_endpoint->endpoint_cache_length ) {
		    /* If the cache still contain some data we can reuse the same fragment
		     * until we flush it completly.
		     */
                    MCA_BTL_TCP_FRAG_INIT_DST(frag, btl_endpoint);
                    goto data_still_pending_on_endpoint;
                }
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */
                MCA_BTL_TCP_FRAG_RETURN(frag);
            }
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
#if MCA_BTL_TCP_ENDPOINT_CACHE
            assert( 0 == btl_endpoint->endpoint_cache_length );
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */
            break;
        }
    case MCA_BTL_TCP_SHUTDOWN:
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        break;
    default:
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        BTL_ERROR(("invalid socket state(%d)", btl_endpoint->endpoint_state));
        mca_btl_tcp_endpoint_close(btl_endpoint);
        break;
    }
}


/*
 * A file descriptor is available/ready for send. Check the state
 * of the socket and take the appropriate action.
 */

static void mca_btl_tcp_endpoint_send_handler(int sd, short flags, void* user)
{
    mca_btl_tcp_endpoint_t* btl_endpoint = (mca_btl_tcp_endpoint_t *)user;
    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
    switch(btl_endpoint->endpoint_state) {
    case MCA_BTL_TCP_CONNECTING:
        mca_btl_tcp_endpoint_complete_connect(btl_endpoint);
        break;
    case MCA_BTL_TCP_CONNECTED:
        {
        /* complete the current send */
        do {
            mca_btl_tcp_frag_t* frag = btl_endpoint->endpoint_send_frag;
            if(mca_btl_tcp_frag_send(frag, btl_endpoint->endpoint_sd) == false) {
                break;
            }
            /* progress any pending sends */
            btl_endpoint->endpoint_send_frag = (mca_btl_tcp_frag_t*)
                opal_list_remove_first(&btl_endpoint->endpoint_frags);

            /* if required - update request status and release fragment */
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
            frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, frag->rc);
            OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);

        } while (NULL != btl_endpoint->endpoint_send_frag);

        /* if nothing else to do unregister for send event notifications */
        if(NULL == btl_endpoint->endpoint_send_frag) {
            opal_event_del(&btl_endpoint->endpoint_send_event);
        }
        break;
        }
    default:
        BTL_ERROR(("invalid connection state (%d)",
            btl_endpoint->endpoint_state));
        opal_event_del(&btl_endpoint->endpoint_send_event);
        break;
    }
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
}



