/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "opal_config.h"

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

#include "opal/mca/event/event.h"
#include "opal/util/net.h"
#include "opal/util/show_help.h"
#include "opal/util/proc.h"
#include "opal/mca/btl/base/btl_base_error.h"

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
    endpoint->endpoint_sd_next = -1;
    endpoint->endpoint_send_frag = 0;
    endpoint->endpoint_recv_frag = 0;
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
    mca_btl_tcp_endpoint_close(endpoint);
    mca_btl_tcp_proc_remove(endpoint->endpoint_proc, endpoint);
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

#define DEBUG_LENGTH  1024
/**
 * The lack of protection in the mca_btl_tcp_endpoint_dump function is voluntary
 * so that it can be called regardless of the state of the mutexes. As a result,
 * when multiple threads work on the same endpoint not only the information
 * displayed might be inacurate, but when we manipulate the pending fragments we
 * might access freed memory. Thus, the caller should lock the endpoint prior
 * to the call.
 */
static void
mca_btl_tcp_endpoint_dump(int level,
                          const char* fname,
                          int lineno,
                          const char* funcname,
                          mca_btl_base_endpoint_t* btl_endpoint,
                          bool full_info,
                          const char* msg)
{
    char outmsg[DEBUG_LENGTH];
    int sndbuf, rcvbuf, nodelay, flags, used = 0;
#if OPAL_ENABLE_IPV6
    struct sockaddr_storage inaddr;
#else
    struct sockaddr_in inaddr;
#endif
    opal_socklen_t obtlen;
    opal_socklen_t addrlen = sizeof(inaddr);
    mca_btl_tcp_frag_t* item;
    mca_btl_tcp_proc_t* this_proc = mca_btl_tcp_proc_local();

    used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "%s: ", msg);
    if (used >= DEBUG_LENGTH) goto out;

    getsockname(btl_endpoint->endpoint_sd, (struct sockaddr*)&inaddr, &addrlen);
#if OPAL_ENABLE_IPV6
    {
        char *address;
        address = (char *) opal_net_get_hostname((struct sockaddr*) &inaddr);
        if (NULL != address) {
            used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "%s -", address);
            if (used >= DEBUG_LENGTH) goto out;
        }
    }
#else
    used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "%s -", inet_ntoa(inaddr.sin_addr));
    if (used >= DEBUG_LENGTH) goto out;
#endif
    getpeername(btl_endpoint->endpoint_sd, (struct sockaddr*)&inaddr, &addrlen);
#if OPAL_ENABLE_IPV6
    {
        char *address;
        address = (char *) opal_net_get_hostname ((struct sockaddr*) &inaddr);
        if (NULL != address) {
            used += snprintf(&outmsg[used], DEBUG_LENGTH - used, " %s", address);
            if (used >= DEBUG_LENGTH) goto out;
        }
    }
#else
    used += snprintf(&outmsg[used], DEBUG_LENGTH - used, " %s", inet_ntoa(inaddr.sin_addr));
    if (used >= DEBUG_LENGTH) goto out;
#endif

    used = snprintf(outmsg, DEBUG_LENGTH, "[%d", btl_endpoint->endpoint_sd);
    if (used >= DEBUG_LENGTH) goto out;
    switch(btl_endpoint->endpoint_state) {
    case MCA_BTL_TCP_CONNECTING:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "connecting");
        if (used >= DEBUG_LENGTH) goto out;
        break;
    case MCA_BTL_TCP_CONNECT_ACK:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "ack");
        if (used >= DEBUG_LENGTH) goto out;
        break;
    case MCA_BTL_TCP_CLOSED:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "close");
        if (used >= DEBUG_LENGTH) goto out;
        break;
    case MCA_BTL_TCP_FAILED:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "failed");
        if (used >= DEBUG_LENGTH) goto out;
        break;
    case MCA_BTL_TCP_CONNECTED:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "connected");
        if (used >= DEBUG_LENGTH) goto out;
        break;
    default:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "unknown");
        if (used >= DEBUG_LENGTH) goto out;
        break;
    }

    if( full_info ) {
        if((flags = fcntl(btl_endpoint->endpoint_sd, F_GETFL, 0)) < 0) {
            BTL_ERROR(("fcntl(F_GETFL) failed: %s (%d)",
                       strerror(opal_socket_errno), opal_socket_errno));
        }

#if defined(SO_SNDBUF)
        obtlen = sizeof(sndbuf);
        if(getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_SNDBUF, (char *)&sndbuf, &obtlen) < 0) {
            BTL_ERROR(("SO_SNDBUF option: %s (%d)",
                       strerror(opal_socket_errno), opal_socket_errno));
        }
#else
        sndbuf = -1;
#endif
#if defined(SO_RCVBUF)
        obtlen = sizeof(rcvbuf);
        if(getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_RCVBUF, (char *)&rcvbuf, &obtlen) < 0) {
            BTL_ERROR(("SO_RCVBUF option: %s (%d)",
                       strerror(opal_socket_errno), opal_socket_errno));
        }
#else
        rcvbuf = -1;
#endif
#if defined(TCP_NODELAY)
        obtlen = sizeof(nodelay);
        if(getsockopt(btl_endpoint->endpoint_sd, IPPROTO_TCP, TCP_NODELAY, (char *)&nodelay, &obtlen) < 0) {
            BTL_ERROR(("TCP_NODELAY option: %s (%d)",
                       strerror(opal_socket_errno), opal_socket_errno));
        }
#else
        nodelay = 0;
#endif
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, " nodelay %d sndbuf %d rcvbuf %d flags %08x",
                         nodelay, sndbuf, rcvbuf, flags);
        if (used >= DEBUG_LENGTH) goto out;
#if MCA_BTL_TCP_ENDPOINT_CACHE
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "\n\t[cache %p used %lu/%lu]",
                         btl_endpoint->endpoint_cache, btl_endpoint->endpoint_cache_pos - btl_endpoint->endpoint_cache,
                         btl_endpoint->endpoint_cache_length);
        if (used >= DEBUG_LENGTH) goto out;
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "{%s - retries %d}",
                         (btl_endpoint->endpoint_nbo ? "NBO" : ""), (int)btl_endpoint->endpoint_retries);
        if (used >= DEBUG_LENGTH) goto out;
    }
    used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "\n");
    if (used >= DEBUG_LENGTH) goto out;

    if( NULL != btl_endpoint->endpoint_recv_frag )
        used += mca_btl_tcp_frag_dump(btl_endpoint->endpoint_recv_frag, "active recv",
                                      &outmsg[used], DEBUG_LENGTH - used);
    if (used >= DEBUG_LENGTH) goto out;

    if( NULL != btl_endpoint->endpoint_send_frag )
        used += mca_btl_tcp_frag_dump(btl_endpoint->endpoint_send_frag, "active send (inaccurate iov)",
                                      &outmsg[used], DEBUG_LENGTH - used);
    if (used >= DEBUG_LENGTH) goto out;
    OPAL_LIST_FOREACH(item, &btl_endpoint->endpoint_frags, mca_btl_tcp_frag_t) {
        used += mca_btl_tcp_frag_dump(item, "pending send", &outmsg[used], DEBUG_LENGTH - used);
        if (used >= DEBUG_LENGTH) goto out;
    }
out:
    outmsg[ used >= DEBUG_LENGTH ? (DEBUG_LENGTH-1) : used ] = '\0';
    opal_output_verbose(level, opal_btl_base_framework.framework_output,
                        "[%s:%d:%s][%s -> %s] %s",
                        fname, lineno, funcname,
                        (NULL != this_proc ? OPAL_NAME_PRINT(mca_btl_tcp_proc_local()->proc_opal->proc_name) : "unknown"),
                        (NULL != btl_endpoint->endpoint_proc ? OPAL_NAME_PRINT(btl_endpoint->endpoint_proc->proc_opal->proc_name) : "unknown remote"),
                        outmsg);
}
#endif  /* WANT_PEER_DUMP */

#if OPAL_ENABLE_DEBUG && WANT_PEER_DUMP
#define MCA_BTL_TCP_ENDPOINT_DUMP(LEVEL, ENDPOINT, INFO, MSG) mca_btl_tcp_endpoint_dump((LEVEL), __FILE__, __LINE__, __func__, (ENDPOINT), (INFO), (MSG))
#else
#define MCA_BTL_TCP_ENDPOINT_DUMP(LEVEL, ENDPOINT, INFO, MSG)
#endif  /* OPAL_ENABLE_DEBUG && WANT_PEER_DUMP */

/*
 * Initialize events to be used by the endpoint instance for TCP select/poll callbacks.
 */

static inline void mca_btl_tcp_endpoint_event_init(mca_btl_base_endpoint_t* btl_endpoint)
{
#if MCA_BTL_TCP_ENDPOINT_CACHE
    assert(NULL == btl_endpoint->endpoint_cache);
    btl_endpoint->endpoint_cache     = (char*)malloc(mca_btl_tcp_component.tcp_endpoint_cache);
    btl_endpoint->endpoint_cache_pos = btl_endpoint->endpoint_cache;
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */

    opal_event_set(opal_event_base, &btl_endpoint->endpoint_recv_event,
                    btl_endpoint->endpoint_sd,
                    OPAL_EV_READ|OPAL_EV_PERSIST,
                    mca_btl_tcp_endpoint_recv_handler,
                    btl_endpoint );
    /**
     * The send event should be non persistent until the endpoint is
     * completely connected. This means, when the event is created it
     * will be fired only once, and when the endpoint is marked as
     * CONNECTED the event should be recreated with the correct flags.
     */
    opal_event_set(opal_event_base, &btl_endpoint->endpoint_send_event,
                    btl_endpoint->endpoint_sd,
                    OPAL_EV_WRITE,
                    mca_btl_tcp_endpoint_send_handler,
                    btl_endpoint);
}


/*
 * Attempt to send a fragment using a given endpoint. If the endpoint is not connected,
 * queue the fragment and start the connection as required.
 */

int mca_btl_tcp_endpoint_send(mca_btl_base_endpoint_t* btl_endpoint, mca_btl_tcp_frag_t* frag)
{
    int rc = OPAL_SUCCESS;

    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
    switch(btl_endpoint->endpoint_state) {
    case MCA_BTL_TCP_CONNECTING:
    case MCA_BTL_TCP_CONNECT_ACK:
    case MCA_BTL_TCP_CLOSED:
        opal_list_append(&btl_endpoint->endpoint_frags, (opal_list_item_t*)frag);
        frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
        if(btl_endpoint->endpoint_state == MCA_BTL_TCP_CLOSED)
            rc = mca_btl_tcp_endpoint_start_connect(btl_endpoint);
        break;
    case MCA_BTL_TCP_FAILED:
        rc = OPAL_ERR_UNREACH;
        break;
    case MCA_BTL_TCP_CONNECTED:
        if (NULL == btl_endpoint->endpoint_send_frag) {
            if(frag->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY &&
               mca_btl_tcp_frag_send(frag, btl_endpoint->endpoint_sd)) {
                int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

                OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                if( frag->base.des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK ) {
                    frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, frag->rc);
                }
                if( btl_ownership ) {
                    MCA_BTL_TCP_FRAG_RETURN(frag);
                }
                MCA_BTL_TCP_ENDPOINT_DUMP(50, btl_endpoint, true, "complete send fragment [endpoint_send]");
                return 1;
            } else {
                btl_endpoint->endpoint_send_frag = frag;
                MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "event_add(send) [endpoint_send]");
                opal_event_add(&btl_endpoint->endpoint_send_event, 0);
                frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
            }
        } else {
            MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "send fragment enqueued [endpoint_send]");
            frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
            opal_list_append(&btl_endpoint->endpoint_frags, (opal_list_item_t*)frag);
        }
        break;
    }
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
    return rc;
}


/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the endpoints endpoint.
 */
static int
mca_btl_tcp_endpoint_send_blocking(mca_btl_base_endpoint_t* btl_endpoint,
                                   void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    while(cnt < size) {
        int retval = send(btl_endpoint->endpoint_sd, (const char *)ptr+cnt, size-cnt, 0);
        if(retval < 0) {
            if(opal_socket_errno != EINTR && opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                BTL_ERROR(("send(%d, %p, %lu/%lu) failed: %s (%d)",
                           btl_endpoint->endpoint_sd, data, cnt, size,
                           strerror(opal_socket_errno), opal_socket_errno));
                btl_endpoint->endpoint_state = MCA_BTL_TCP_FAILED;
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
    opal_process_name_t guid = btl_proc->proc_opal->proc_name;

    OPAL_PROCESS_NAME_HTON(guid);
    if(mca_btl_tcp_endpoint_send_blocking(btl_endpoint, &guid, sizeof(guid)) !=
          sizeof(guid)) {
        return OPAL_ERR_UNREACH;
    }
    return OPAL_SUCCESS;
}

static void *mca_btl_tcp_endpoint_complete_accept(int fd, int flags, void *context)
{
    mca_btl_base_endpoint_t* btl_endpoint = (mca_btl_base_endpoint_t*)context;
    mca_btl_tcp_proc_t* this_proc = mca_btl_tcp_proc_local();
    struct timeval now = {0, 0};
    int cmpval;

    if( OPAL_THREAD_TRYLOCK(&btl_endpoint->endpoint_recv_lock) ) {
        opal_event_add(&btl_endpoint->endpoint_accept_event, &now);
        return NULL;
    }
    if( OPAL_THREAD_TRYLOCK(&btl_endpoint->endpoint_send_lock) ) {
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        opal_event_add(&btl_endpoint->endpoint_accept_event, &now);
        return NULL;
    }

    if(NULL == btl_endpoint->endpoint_addr) {
        CLOSE_THE_SOCKET(btl_endpoint->endpoint_sd_next); /* No further use of this socket. Close it */
        btl_endpoint->endpoint_sd_next = -1;
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        if( NULL != btl_endpoint->endpoint_btl->tcp_error_cb ) {
            btl_endpoint->endpoint_btl->tcp_error_cb(
                &btl_endpoint->endpoint_btl->super, MCA_BTL_ERROR_FLAGS_NONFATAL,
                btl_endpoint->endpoint_proc->proc_opal,
                "The endpoint addr is set to NULL (unsettling)");
        }
        return NULL;
    }

    cmpval = opal_compare_proc(btl_endpoint->endpoint_proc->proc_opal->proc_name,
                               this_proc->proc_opal->proc_name);
    if((btl_endpoint->endpoint_sd < 0) ||
       (btl_endpoint->endpoint_state != MCA_BTL_TCP_CONNECTED &&
        cmpval < 0)) {
        mca_btl_tcp_endpoint_close(btl_endpoint);
        btl_endpoint->endpoint_sd = btl_endpoint->endpoint_sd_next;
        btl_endpoint->endpoint_sd_next = -1;
        if(mca_btl_tcp_endpoint_send_connect_ack(btl_endpoint) != OPAL_SUCCESS) {
            MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, true, " [endpoint_accept]");
            btl_endpoint->endpoint_state = MCA_BTL_TCP_FAILED;
            mca_btl_tcp_endpoint_close(btl_endpoint);
            goto unlock_and_return;
        }
        mca_btl_tcp_endpoint_event_init(btl_endpoint);
        MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "event_add(recv) [endpoint_accept]");
        opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
        mca_btl_tcp_endpoint_connected(btl_endpoint);

        MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "accepted");
        goto unlock_and_return;
    }
    CLOSE_THE_SOCKET(btl_endpoint->endpoint_sd_next); /* No further use of this socket. Close it */
    btl_endpoint->endpoint_sd_next = -1;
  unlock_and_return:
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
    return NULL;
}

/*
 * Check the state of this endpoint. If the incoming connection request matches
 * our endpoints address, check the state of our connection:
 * (1) if a connection has not been attempted, accept the connection
 * (2) if a connection has not been established, and the endpoints process identifier
 *     is less than the local process, accept the connection
 * otherwise, reject the connection and continue with the current connection
 */

void mca_btl_tcp_endpoint_accept(mca_btl_base_endpoint_t* btl_endpoint,
                                 struct sockaddr* addr, int sd)
{
    struct timeval now = {0, 0};

    assert(btl_endpoint->endpoint_sd_next == -1);
    btl_endpoint->endpoint_sd_next = sd;

    opal_event_evtimer_set(opal_event_base, &btl_endpoint->endpoint_accept_event,
                           mca_btl_tcp_endpoint_complete_accept, btl_endpoint);
    opal_event_add(&btl_endpoint->endpoint_accept_event, &now);
}


/*
 * Remove any event registrations associated with the socket
 * and update the endpoint state to reflect the connection has
 * been closed.
 */
void mca_btl_tcp_endpoint_close(mca_btl_base_endpoint_t* btl_endpoint)
{
    if(btl_endpoint->endpoint_sd < 0)
        return;
    btl_endpoint->endpoint_retries++;
    MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, false, "event_del(recv) [close]");
    opal_event_del(&btl_endpoint->endpoint_recv_event);
    MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, false, "event_del(send) [close]");
    opal_event_del(&btl_endpoint->endpoint_send_event);
    CLOSE_THE_SOCKET(btl_endpoint->endpoint_sd);
    btl_endpoint->endpoint_sd = -1;
#if MCA_BTL_TCP_ENDPOINT_CACHE
    free( btl_endpoint->endpoint_cache );
    btl_endpoint->endpoint_cache        = NULL;
    btl_endpoint->endpoint_cache_pos    = NULL;
    btl_endpoint->endpoint_cache_length = 0;
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */
    /**
     * If we keep failing to connect to the peer let the caller know about
     * this situation by triggering all the pending fragments callback and
     * reporting the error.
     */
    if( MCA_BTL_TCP_FAILED == btl_endpoint->endpoint_state ) {
        mca_btl_tcp_frag_t* frag = btl_endpoint->endpoint_send_frag;
        if( NULL == frag )
            frag = (mca_btl_tcp_frag_t*)opal_list_remove_first(&btl_endpoint->endpoint_frags);
        while(NULL != frag) {
            frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, OPAL_ERR_UNREACH);

            frag = (mca_btl_tcp_frag_t*)opal_list_remove_first(&btl_endpoint->endpoint_frags);
        }
    }
    btl_endpoint->endpoint_state = MCA_BTL_TCP_CLOSED;
}

/*
 *  Setup endpoint state to reflect that connection has been established,
 *  and start any pending sends. This function should be called with the
 *  send lock locked.
 */

static void mca_btl_tcp_endpoint_connected(mca_btl_base_endpoint_t* btl_endpoint)
{
    /* setup socket options */
    assert( MCA_BTL_TCP_CONNECTED != btl_endpoint->endpoint_state );
    btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECTED;
    btl_endpoint->endpoint_retries = 0;
    MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, true, "READY [endpoint_connected]");

    /* Create the send event in a persistent manner. */
    opal_event_set(opal_event_base, &btl_endpoint->endpoint_send_event,
                    btl_endpoint->endpoint_sd,
                    OPAL_EV_WRITE | OPAL_EV_PERSIST,
                    mca_btl_tcp_endpoint_send_handler,
                    btl_endpoint );

    if(opal_list_get_size(&btl_endpoint->endpoint_frags) > 0) {
        if(NULL == btl_endpoint->endpoint_send_frag)
            btl_endpoint->endpoint_send_frag = (mca_btl_tcp_frag_t*)
                opal_list_remove_first(&btl_endpoint->endpoint_frags);
        MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "event_add(send) [endpoint_connected]");
        opal_event_add(&btl_endpoint->endpoint_send_event, 0);
    }
}


/*
 * A blocking recv on a non-blocking socket. Used to receive the small
 * amount of connection information that identifies the remote endpoint (guid).
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
            return cnt;
        }

        /* socket is non-blocking so handle errors */
        if(retval < 0) {
            if(opal_socket_errno != EINTR && opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                BTL_ERROR(("recv(%d, %lu/%lu) failed: %s (%d)",
                           btl_endpoint->endpoint_sd, cnt, size, strerror(opal_socket_errno), opal_socket_errno));
                btl_endpoint->endpoint_state = MCA_BTL_TCP_FAILED;
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
    size_t s;
    opal_process_name_t guid;
    mca_btl_tcp_proc_t* btl_proc = btl_endpoint->endpoint_proc;

    s = mca_btl_tcp_endpoint_recv_blocking(btl_endpoint,
                                           &guid, sizeof(opal_process_name_t));
    if (s != sizeof(opal_process_name_t)) {
        if (0 == s) {
            /* If we get zero bytes, the peer closed the socket. This
               can happen when the two peers started the connection
               protocol simultaneously. Just report the problem
               upstream. */
            return OPAL_ERROR;
        }
        opal_show_help("help-mpi-btl-tcp.txt", "client handshake fail",
                       true, opal_process_info.nodename,
                       getpid(),
                       "did not receive entire connect ACK from peer");
        return OPAL_ERR_UNREACH;
    }
    OPAL_PROCESS_NAME_NTOH(guid);
    /* compare this to the expected values */
    /* TODO: this deserve a little bit more thinking as we are not supposed
     * to be able to exchange the opal_process_name_t over the network.
     */
    if (0 != opal_compare_proc(btl_proc->proc_opal->proc_name, guid)) {
        BTL_ERROR(("received unexpected process identifier %s",
                   OPAL_NAME_PRINT(guid)));
        mca_btl_tcp_endpoint_close(btl_endpoint);
        return OPAL_ERR_UNREACH;
    }

    return OPAL_SUCCESS;
}


void mca_btl_tcp_set_socket_options(int sd)
{
#if defined(TCP_NODELAY)
    int optval;
    optval = !mca_btl_tcp_component.tcp_not_use_nodelay;
    if(setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (char *)&optval, sizeof(optval)) < 0) {
        BTL_ERROR(("setsockopt(TCP_NODELAY) failed: %s (%d)",
                   strerror(opal_socket_errno), opal_socket_errno));
    }
#endif
#if defined(SO_SNDBUF)
    if(mca_btl_tcp_component.tcp_sndbuf > 0 &&
       setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (char *)&mca_btl_tcp_component.tcp_sndbuf, sizeof(int)) < 0) {
        BTL_ERROR(("setsockopt(SO_SNDBUF) failed: %s (%d)",
                   strerror(opal_socket_errno), opal_socket_errno));
    }
#endif
#if defined(SO_RCVBUF)
    if(mca_btl_tcp_component.tcp_rcvbuf > 0 &&
       setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (char *)&mca_btl_tcp_component.tcp_rcvbuf, sizeof(int)) < 0) {
        BTL_ERROR(("setsockopt(SO_RCVBUF) failed: %s (%d)",
                   strerror(opal_socket_errno), opal_socket_errno));
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
    struct sockaddr_storage endpoint_addr;
    /* By default consider a IPv4 connection */
    uint16_t af_family = AF_INET;
    opal_socklen_t addrlen = sizeof(struct sockaddr_in);

#if OPAL_ENABLE_IPV6
    if (AF_INET6 == btl_endpoint->endpoint_addr->addr_family) {
        af_family = AF_INET6;
        addrlen = sizeof (struct sockaddr_in6);
    }
#endif
    assert( btl_endpoint->endpoint_sd < 0 );
    btl_endpoint->endpoint_sd = socket(af_family, SOCK_STREAM, 0);
    if (btl_endpoint->endpoint_sd < 0) {
        btl_endpoint->endpoint_retries++;
        return OPAL_ERR_UNREACH;
    }

    /* setup socket buffer sizes */
    mca_btl_tcp_set_socket_options(btl_endpoint->endpoint_sd);

    /* setup event callbacks */
    mca_btl_tcp_endpoint_event_init(btl_endpoint);

    /* setup the socket as non-blocking */
    if((flags = fcntl(btl_endpoint->endpoint_sd, F_GETFL, 0)) < 0) {
        BTL_ERROR(("fcntl(F_GETFL) failed: %s (%d)",
                   strerror(opal_socket_errno), opal_socket_errno));
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(btl_endpoint->endpoint_sd, F_SETFL, flags) < 0)
            BTL_ERROR(("fcntl(F_SETFL) failed: %s (%d)",
                       strerror(opal_socket_errno), opal_socket_errno));
    }

    /* start the connect - will likely fail with EINPROGRESS */
    mca_btl_tcp_proc_tosocks(btl_endpoint->endpoint_addr, &endpoint_addr);

    opal_output_verbose(20, opal_btl_base_framework.framework_output,
                        "btl: tcp: attempting to connect() to %s address %s on port %d",
                        OPAL_NAME_PRINT(btl_endpoint->endpoint_proc->proc_opal->proc_name),
                        opal_net_get_hostname((struct sockaddr*) &endpoint_addr),
                        ntohs(btl_endpoint->endpoint_addr->addr_port));

    if(0 == connect(btl_endpoint->endpoint_sd, (struct sockaddr*)&endpoint_addr, addrlen)) {
        /* send our globally unique process identifier to the endpoint */
        if((rc = mca_btl_tcp_endpoint_send_connect_ack(btl_endpoint)) == OPAL_SUCCESS) {
            btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECT_ACK;
            MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "event_add(recv) [start_connect]");
            opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
            return OPAL_SUCCESS;
        }
        /* We connected to the peer, but he close the socket before we got a chance to send our guid */
        MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, true, "dropped connection [start_connect]");
    } else {
        /* non-blocking so wait for completion */
        if(opal_socket_errno == EINPROGRESS || opal_socket_errno == EWOULDBLOCK) {
            btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECTING;
            MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "event_add(send) [start_connect]");
            opal_event_add(&btl_endpoint->endpoint_send_event, 0);
            return OPAL_SUCCESS;
        }
    }
    btl_endpoint->endpoint_retries++;
    {
        char *address;
        address = opal_net_get_hostname((struct sockaddr*) &endpoint_addr);
        BTL_PEER_ERROR( btl_endpoint->endpoint_proc->proc_opal,
                      ( "Unable to connect to the peer %s on port %d: %s\n",
                        address,
                        btl_endpoint->endpoint_addr->addr_port, strerror(opal_socket_errno) ) );
    }
    btl_endpoint->endpoint_state = MCA_BTL_TCP_FAILED;
    mca_btl_tcp_endpoint_close(btl_endpoint);
    return OPAL_ERR_UNREACH;
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
    struct sockaddr_storage endpoint_addr;

    mca_btl_tcp_proc_tosocks(btl_endpoint->endpoint_addr, &endpoint_addr);

    /* check connect completion status */
    if(getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_ERROR, (char *)&so_error, &so_length) < 0) {
        BTL_ERROR(("getsockopt() to %s failed: %s (%d)",
                   opal_net_get_hostname((struct sockaddr*) &endpoint_addr),
                   strerror(opal_socket_errno), opal_socket_errno));
        mca_btl_tcp_endpoint_close(btl_endpoint);
        return;
    }
    if(so_error == EINPROGRESS || so_error == EWOULDBLOCK) {
        return;
    }
    if(so_error != 0) {
        BTL_ERROR(("connect() to %s failed: %s (%d)",
                   opal_net_get_hostname((struct sockaddr*) &endpoint_addr),
                   strerror(so_error), so_error));
        mca_btl_tcp_endpoint_close(btl_endpoint);
        return;
    }

    /* Do not unregister from receiving send event notifications, instead
     * leave the event to trigger once more, and then it will get automatically
     * deleted as no send fragments are available.
     */

     if(mca_btl_tcp_endpoint_send_connect_ack(btl_endpoint) == OPAL_SUCCESS) {
        btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECT_ACK;
        opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
        MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, false, "event_add(recv) [complete_connect]");
        return;
    }
    MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, false, " [complete_connect]");
    btl_endpoint->endpoint_state = MCA_BTL_TCP_FAILED;
    mca_btl_tcp_endpoint_close(btl_endpoint);
}


/*
 * A file descriptor is available/ready for recv. Check the state
 * of the socket and take the appropriate action.
 */

static void mca_btl_tcp_endpoint_recv_handler(int sd, short flags, void* user)
{
    mca_btl_base_endpoint_t* btl_endpoint = (mca_btl_base_endpoint_t *)user;

    /* Make sure we don't have a race between a thread that remove the
     * recv event, and one event already scheduled.
     */
    if( sd != btl_endpoint->endpoint_sd )
        return;

    /**
     * There is an extremely rare race condition here, that can only be
     * triggered during the initialization. If the two processes start their
     * connection in same time, one of the processes will have to close it's
     * previous endpoint (the one opened from the local send). As a result it
     * might go in btl_endpoint_close and try to delete the recv_event. This
     * call will go back in the libevent, and in a multithreaded case will try
     * to lock the event. If another thread noticed the active event (and this
     * is possible as during the initialization there will be 2 sockets), one
     * thread might get stuck trying to lock the endpoint_recv_lock (while
     * holding the event_base lock) while the other thread will try to lock the
     * event_base lock (while holding the endpoint_recv lock).
     *
     * If we can't lock this mutex, it is OK to cancel the receive operation, it
     * will be eventually triggered again shorthly.
     */
    if( OPAL_THREAD_TRYLOCK(&btl_endpoint->endpoint_recv_lock) )
        return;

    switch(btl_endpoint->endpoint_state) {
    case MCA_BTL_TCP_CONNECT_ACK:
        {
            int rc = mca_btl_tcp_endpoint_recv_connect_ack(btl_endpoint);
            if( OPAL_SUCCESS == rc ) {
                /* we are now connected. Start sending the data */
                OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
                mca_btl_tcp_endpoint_connected(btl_endpoint);
                OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "connected");
            }
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
            return;
        }
    case MCA_BTL_TCP_CONNECTED:
        {
            mca_btl_tcp_frag_t* frag;

            frag = btl_endpoint->endpoint_recv_frag;
            if(NULL == frag) {
                if(mca_btl_tcp_module.super.btl_max_send_size >
                   mca_btl_tcp_module.super.btl_eager_limit) {
                    MCA_BTL_TCP_FRAG_ALLOC_MAX(frag);
                } else {
                    MCA_BTL_TCP_FRAG_ALLOC_EAGER(frag);
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
            if(mca_btl_tcp_frag_recv(frag, btl_endpoint->endpoint_sd) == false) {
                btl_endpoint->endpoint_recv_frag = frag;
            } else {
                btl_endpoint->endpoint_recv_frag = NULL;
                if( MCA_BTL_TCP_HDR_TYPE_SEND == frag->hdr.type ) {
                    mca_btl_active_message_callback_t* reg;
                    reg = mca_btl_base_active_message_trigger + frag->hdr.base.tag;
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
#if MCA_BTL_TCP_ENDPOINT_CACHE
            assert( 0 == btl_endpoint->endpoint_cache_length );
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
            break;
        }
    case MCA_BTL_TCP_CLOSED:
        /* This is a thread-safety issue. As multiple threads are allowed
         * to generate events (in the lib event) we endup with several
         * threads executing the receive callback, when we reach the end
         * of the MPI_Finalize. The first one will close the connections,
         * and all others will complain.
         */
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        break;
    default:
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        BTL_ERROR(("invalid socket state(%d)", btl_endpoint->endpoint_state));
        btl_endpoint->endpoint_state = MCA_BTL_TCP_FAILED;
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

    /* if another thread is already here, give up */
    if( OPAL_THREAD_TRYLOCK(&btl_endpoint->endpoint_send_lock) )
        return;

    switch(btl_endpoint->endpoint_state) {
    case MCA_BTL_TCP_CONNECTING:
        mca_btl_tcp_endpoint_complete_connect(btl_endpoint);
        break;
    case MCA_BTL_TCP_CONNECTED:
        /* complete the current send */
        while (NULL != btl_endpoint->endpoint_send_frag) {
            mca_btl_tcp_frag_t* frag = btl_endpoint->endpoint_send_frag;
            int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

            if(mca_btl_tcp_frag_send(frag, btl_endpoint->endpoint_sd) == false) {
                break;
            }
            /* progress any pending sends */
            btl_endpoint->endpoint_send_frag = (mca_btl_tcp_frag_t*)
                opal_list_remove_first(&btl_endpoint->endpoint_frags);

            /* if required - update request status and release fragment */
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
            assert( frag->base.des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK );
            frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, frag->rc);
            if( btl_ownership ) {
                MCA_BTL_TCP_FRAG_RETURN(frag);
            }
            /* if we fail to take the lock simply return. In the worst case the
             * send_handler will be triggered once more, and as there will be
             * nothing to send the handler will be deleted.
             */
            if( OPAL_THREAD_TRYLOCK(&btl_endpoint->endpoint_send_lock) )
                return;
        }

        /* if nothing else to do unregister for send event notifications */
        if(NULL == btl_endpoint->endpoint_send_frag) {
            MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, false, "event_del(send) [endpoint_send_handler]");
            opal_event_del(&btl_endpoint->endpoint_send_event);
        }
        break;
    default:
        BTL_ERROR(("invalid connection state (%d)", btl_endpoint->endpoint_state));
        MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, true, "event_del(send) [endpoint_send_handler:error]");
        opal_event_del(&btl_endpoint->endpoint_send_event);
        break;
    }
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
}
