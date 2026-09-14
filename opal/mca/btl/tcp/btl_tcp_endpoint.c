/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2013-2022 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2020 Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 */

#include "opal_config.h"

#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include "opal/opal_socket_errno.h"
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#endif
#ifdef HAVE_NETINET_TCP_H
#    include <netinet/tcp.h>
#endif
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */
#include <time.h>

#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/util/event.h"
#include "opal/util/net.h"
#include "opal/util/printf.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/util/string_copy.h"

#include "btl_tcp.h"
#include "btl_tcp_addr.h"
#include "btl_tcp_endpoint.h"
#include "btl_tcp_frag.h"
#include "btl_tcp_proc.h"

/*
 * Magic ID string send during connect/accept handshake
 */

const char mca_btl_tcp_magic_id_string[MCA_BTL_TCP_MAGIC_STRING_LENGTH] = "OPAL-TCP-BTL";

/*
 * Initialize state of the endpoint instance.
 *
 */
static void mca_btl_tcp_endpoint_construct(mca_btl_tcp_endpoint_t *endpoint)
{
    endpoint->endpoint_btl = NULL;
    endpoint->endpoint_proc = NULL;
    endpoint->endpoint_addr = NULL;
    endpoint->endpoint_sd = -1;
    endpoint->endpoint_send_frag = 0;
    endpoint->endpoint_recv_frag = 0;
    endpoint->endpoint_state = MCA_BTL_TCP_CLOSED;
    endpoint->endpoint_retries = 0;
    endpoint->endpoint_nbo = false;
#if MCA_BTL_TCP_ENDPOINT_CACHE
    endpoint->endpoint_cache = NULL;
    endpoint->endpoint_cache_pos = NULL;
    endpoint->endpoint_cache_length = 0;
#endif /* MCA_BTL_TCP_ENDPOINT_CACHE */
    OBJ_CONSTRUCT(&endpoint->endpoint_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_send_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_recv_lock, opal_mutex_t);
}

/*
 * Destroy a endpoint
 *
 */
static void mca_btl_tcp_endpoint_destruct(mca_btl_tcp_endpoint_t *endpoint)
{
    mca_btl_tcp_endpoint_close(endpoint);
    mca_btl_tcp_proc_remove(endpoint->endpoint_proc, endpoint);
    OBJ_DESTRUCT(&endpoint->endpoint_frags);
    OBJ_DESTRUCT(&endpoint->endpoint_send_lock);
    OBJ_DESTRUCT(&endpoint->endpoint_recv_lock);
}

OBJ_CLASS_INSTANCE(mca_btl_tcp_endpoint_t, opal_list_item_t, mca_btl_tcp_endpoint_construct,
                   mca_btl_tcp_endpoint_destruct);

static void mca_btl_tcp_endpoint_construct(mca_btl_base_endpoint_t *btl_endpoint);
static void mca_btl_tcp_endpoint_destruct(mca_btl_base_endpoint_t *btl_endpoint);
static int mca_btl_tcp_endpoint_start_connect(mca_btl_base_endpoint_t *);
static void mca_btl_tcp_endpoint_connected(mca_btl_base_endpoint_t *);
static void mca_btl_tcp_endpoint_recv_handler(int sd, short flags, void *user);
static void mca_btl_tcp_endpoint_send_handler(int sd, short flags, void *user);

/*
 * diagnostics
 */

#if OPAL_ENABLE_DEBUG && WANT_PEER_DUMP

#    define DEBUG_LENGTH 1024
/**
 * The lack of protection in the mca_btl_tcp_endpoint_dump function is voluntary
 * so that it can be called regardless of the state of the mutexes. As a result,
 * when multiple threads work on the same endpoint not only the information
 * displayed might be inaccurate, but when we manipulate the pending fragments we
 * might access freed memory. Thus, the caller should lock the endpoint prior
 * to the call.
 */
void mca_btl_tcp_endpoint_dump(int level, const char *fname, int lineno, const char *funcname,
                               mca_btl_base_endpoint_t *btl_endpoint, bool full_info,
                               const char *msg)
{
    char outmsg[DEBUG_LENGTH];
    int sndbuf, rcvbuf, nodelay, flags, used = 0;
#    if OPAL_ENABLE_IPV6
    struct sockaddr_storage inaddr;
#    else
    struct sockaddr_in inaddr;
#    endif
    opal_socklen_t obtlen;
    opal_socklen_t addrlen = sizeof(inaddr);
    mca_btl_tcp_frag_t *item;

    used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "%s: ", msg);
    if (used >= DEBUG_LENGTH)
        goto out;

    getsockname(btl_endpoint->endpoint_sd, (struct sockaddr *) &inaddr, &addrlen);
#    if OPAL_ENABLE_IPV6
    {
        char *address;
        address = (char *) opal_net_get_hostname((struct sockaddr *) &inaddr);
        if (NULL != address) {
            used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "%s -", address);
            if (used >= DEBUG_LENGTH)
                goto out;
        }
    }
#    else
    {
        char ep_addr[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &inaddr.sin_addr, ep_addr, sizeof(ep_addr));
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "%s -", ep_addr);
    }
    if (used >= DEBUG_LENGTH)
        goto out;
#    endif
    getpeername(btl_endpoint->endpoint_sd, (struct sockaddr *) &inaddr, &addrlen);
#    if OPAL_ENABLE_IPV6
    {
        char *address;
        address = (char *) opal_net_get_hostname((struct sockaddr *) &inaddr);
        if (NULL != address) {
            used += snprintf(&outmsg[used], DEBUG_LENGTH - used, " %s", address);
            if (used >= DEBUG_LENGTH)
                goto out;
        }
    }
#    else
    {
        char ep_addr[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &inaddr.sin_addr, ep_addr, sizeof(ep_addr));
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, " %s", ep_addr);
    }
    if (used >= DEBUG_LENGTH)
        goto out;
#    endif

    used = snprintf(outmsg, DEBUG_LENGTH, "[%d", btl_endpoint->endpoint_sd);
    if (used >= DEBUG_LENGTH)
        goto out;
    switch (btl_endpoint->endpoint_state) {
    case MCA_BTL_TCP_CONNECTING:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "connecting");
        if (used >= DEBUG_LENGTH)
            goto out;
        break;
    case MCA_BTL_TCP_CONNECT_ACK:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "ack");
        if (used >= DEBUG_LENGTH)
            goto out;
        break;
    case MCA_BTL_TCP_CLOSED:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "close");
        if (used >= DEBUG_LENGTH)
            goto out;
        break;
    case MCA_BTL_TCP_FAILED:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "failed");
        if (used >= DEBUG_LENGTH)
            goto out;
        break;
    case MCA_BTL_TCP_CONNECTED:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "connected");
        if (used >= DEBUG_LENGTH)
            goto out;
        break;
    default:
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, ":%s]", "unknown");
        if (used >= DEBUG_LENGTH)
            goto out;
        break;
    }

    if (full_info) {
        if ((flags = fcntl(btl_endpoint->endpoint_sd, F_GETFL, 0)) < 0) {
            BTL_ERROR(
                ("fcntl(F_GETFL) failed: %s (%d)", strerror(opal_socket_errno), opal_socket_errno));
        }

#    if defined(SO_SNDBUF)
        obtlen = sizeof(sndbuf);
        if (getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_SNDBUF, (char *) &sndbuf, &obtlen)
            < 0) {
            BTL_ERROR(
                ("SO_SNDBUF option: %s (%d)", strerror(opal_socket_errno), opal_socket_errno));
        }
#    else
        sndbuf = -1;
#    endif
#    if defined(SO_RCVBUF)
        obtlen = sizeof(rcvbuf);
        if (getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_RCVBUF, (char *) &rcvbuf, &obtlen)
            < 0) {
            BTL_ERROR(
                ("SO_RCVBUF option: %s (%d)", strerror(opal_socket_errno), opal_socket_errno));
        }
#    else
        rcvbuf = -1;
#    endif
#    if defined(TCP_NODELAY)
        obtlen = sizeof(nodelay);
        if (getsockopt(btl_endpoint->endpoint_sd, IPPROTO_TCP, TCP_NODELAY, (char *) &nodelay,
                       &obtlen)
            < 0) {
            BTL_ERROR(
                ("TCP_NODELAY option: %s (%d)", strerror(opal_socket_errno), opal_socket_errno));
        }
#    else
        nodelay = 0;
#    endif
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used,
                         " nodelay %d sndbuf %d rcvbuf %d flags %08x", nodelay, sndbuf, rcvbuf,
                         flags);
        if (used >= DEBUG_LENGTH)
            goto out;
#    if MCA_BTL_TCP_ENDPOINT_CACHE
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "\n\t[cache %p used %lu/%lu]",
                         (void *) btl_endpoint->endpoint_cache,
                         btl_endpoint->endpoint_cache_pos - btl_endpoint->endpoint_cache,
                         btl_endpoint->endpoint_cache_length);
        if (used >= DEBUG_LENGTH)
            goto out;
#    endif /* MCA_BTL_TCP_ENDPOINT_CACHE */
        used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "{%s - retries %d}",
                         (btl_endpoint->endpoint_nbo ? "NBO" : ""),
                         (int) btl_endpoint->endpoint_retries);
        if (used >= DEBUG_LENGTH)
            goto out;
    }
    used += snprintf(&outmsg[used], DEBUG_LENGTH - used, "\n");
    if (used >= DEBUG_LENGTH)
        goto out;

    if (NULL != btl_endpoint->endpoint_recv_frag)
        used += mca_btl_tcp_frag_dump(btl_endpoint->endpoint_recv_frag, "active recv",
                                      &outmsg[used], DEBUG_LENGTH - used);
    if (used >= DEBUG_LENGTH)
        goto out;

    if (NULL != btl_endpoint->endpoint_send_frag)
        used += mca_btl_tcp_frag_dump(btl_endpoint->endpoint_send_frag,
                                      "active send (inaccurate iov)", &outmsg[used],
                                      DEBUG_LENGTH - used);
    if (used >= DEBUG_LENGTH)
        goto out;
    OPAL_LIST_FOREACH (item, &btl_endpoint->endpoint_frags, mca_btl_tcp_frag_t) {
        used += mca_btl_tcp_frag_dump(item, "pending send", &outmsg[used], DEBUG_LENGTH - used);
        if (used >= DEBUG_LENGTH)
            goto out;
    }
out:
    outmsg[used >= DEBUG_LENGTH ? (DEBUG_LENGTH - 1) : used] = '\0';
    opal_output_verbose(level, opal_btl_base_framework.framework_output, "[%s:%d:%s][%s -> %s] %s",
                        fname, lineno, funcname, OPAL_NAME_PRINT(opal_proc_local_get()->proc_name),
                        (NULL != btl_endpoint->endpoint_proc
                             ? OPAL_NAME_PRINT(btl_endpoint->endpoint_proc->proc_opal->proc_name)
                             : "unknown remote"),
                        outmsg);
}
#endif /* OPAL_ENABLE_DEBUG && WANT_PEER_DUMP */

/*
 * Initialize events to be used by the endpoint instance for TCP select/poll callbacks.
 */

static inline void mca_btl_tcp_endpoint_event_init(mca_btl_base_endpoint_t *btl_endpoint)
{
#if MCA_BTL_TCP_ENDPOINT_CACHE
    assert(NULL == btl_endpoint->endpoint_cache);
    btl_endpoint->endpoint_cache = (char *) malloc(mca_btl_tcp_component.tcp_endpoint_cache);
    btl_endpoint->endpoint_cache_pos = btl_endpoint->endpoint_cache;
#endif /* MCA_BTL_TCP_ENDPOINT_CACHE */

    opal_event_set(mca_btl_tcp_event_base, &btl_endpoint->endpoint_recv_event,
                   btl_endpoint->endpoint_sd, OPAL_EV_READ | OPAL_EV_PERSIST,
                   mca_btl_tcp_endpoint_recv_handler, btl_endpoint);
    /**
     * In the multi-threaded case, the send event must be persistent in order
     * to avoid missing the connection notification in send_handler due to
     * a local handling of the peer process (which holds the lock).
     */
    opal_event_set(mca_btl_tcp_event_base, &btl_endpoint->endpoint_send_event,
                   btl_endpoint->endpoint_sd, OPAL_EV_WRITE | OPAL_EV_PERSIST,
                   mca_btl_tcp_endpoint_send_handler, btl_endpoint);
}

/*
 * Attempt to send a fragment using a given endpoint. If the endpoint is not connected,
 * queue the fragment and start the connection as required.
 */

int mca_btl_tcp_endpoint_send(mca_btl_base_endpoint_t *btl_endpoint, mca_btl_tcp_frag_t *frag)
{
    int rc = OPAL_SUCCESS;

    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
    switch (btl_endpoint->endpoint_state) {
    case MCA_BTL_TCP_CONNECTING:
    case MCA_BTL_TCP_CONNECT_ACK:
    case MCA_BTL_TCP_CLOSED:
        opal_list_append(&btl_endpoint->endpoint_frags, (opal_list_item_t *) frag);
        frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
        if (btl_endpoint->endpoint_state == MCA_BTL_TCP_CLOSED) {
            rc = mca_btl_tcp_endpoint_start_connect(btl_endpoint);
        }
        break;
    case MCA_BTL_TCP_FAILED:
        rc = OPAL_ERR_UNREACH;
        break;
    case MCA_BTL_TCP_CONNECTED:
        if (NULL == btl_endpoint->endpoint_send_frag) {
            if (frag->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY
                && mca_btl_tcp_frag_send(frag, btl_endpoint->endpoint_sd)) {
                int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

                OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                if (frag->base.des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
                    frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, frag->rc);
                }
                if (btl_ownership) {
                    MCA_BTL_TCP_FRAG_RETURN(frag);
                }
                MCA_BTL_TCP_ENDPOINT_DUMP(50, btl_endpoint, true,
                                          "complete send fragment [endpoint_send]");
                return 1;
            } else {
                btl_endpoint->endpoint_send_frag = frag;
                MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true,
                                          "event_add(send) [endpoint_send]");
                frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
                MCA_BTL_TCP_ACTIVATE_EVENT(&btl_endpoint->endpoint_send_event, 0);
            }
        } else {
            MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true,
                                      "send fragment enqueued [endpoint_send]");
            frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
            opal_list_append(&btl_endpoint->endpoint_frags, (opal_list_item_t *) frag);
        }
        break;
    }
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
    return rc;
}

/*
 * Send the globally unique identifier for this process to a endpoint on
 * a newly connected socket.
 */
static int mca_btl_tcp_endpoint_send_connect_ack(mca_btl_base_endpoint_t *btl_endpoint)
{
    opal_process_name_t guid = opal_proc_local_get()->proc_name;
    OPAL_PROCESS_NAME_HTON(guid);

    mca_btl_tcp_endpoint_hs_msg_t hs_msg;
    opal_string_copy(hs_msg.magic_id, mca_btl_tcp_magic_id_string, sizeof(hs_msg.magic_id));
    hs_msg.guid = guid;

    if (sizeof(hs_msg)
        != mca_btl_tcp_send_blocking(btl_endpoint->endpoint_sd, &hs_msg, sizeof(hs_msg))) {
        opal_show_help("help-mpi-btl-tcp.txt", "client handshake fail", true,
                       opal_process_info.nodename, sizeof(hs_msg),
                       "connect ACK failed to send magic-id and guid");
        return OPAL_ERR_UNREACH;
    }
    return OPAL_SUCCESS;
}

/*
 * Tell the peer that this socket is going away on purpose, so that its
 * end of it reads as a clean close rather than as a process that died.
 * Best effort: a failed write means the peer has already gone, which is
 * what the message would have told it.
 */
static void mca_btl_tcp_endpoint_send_fin(mca_btl_base_endpoint_t *btl_endpoint)
{
    mca_btl_tcp_hdr_t fin_msg = {
        .base.tag = 0,
        .type = MCA_BTL_TCP_HDR_TYPE_FIN,
        .count = 0,
        .size = 0,
    };

    (void) mca_btl_tcp_send_blocking(btl_endpoint->endpoint_sd, &fin_msg, sizeof(fin_msg));
}

/*
 * Has the peer already abandoned this incoming connection?
 *
 * Between the handshake the component consumed and the ack we are about
 * to write, a dialer says nothing: mca_btl_tcp_endpoint_send() queues
 * fragments until MCA_BTL_TCP_CONNECTED, which is what our ack brings
 * about. So anything readable here is the peer letting go of this socket
 * -- its goodbye, or the bare eof of one it dropped during a
 * simultaneous-connect duel before we taught it to say so, or a reset.
 * Adopting one of those yields a connection that dies without ever
 * carrying anything, which every path below reads as a failed peer.
 */
static bool mca_btl_tcp_endpoint_abandoned(int sd)
{
    char byte;
    ssize_t rc;

    do {
        rc = recv(sd, &byte, 1, MSG_PEEK);
    } while ((rc < 0) && (EINTR == opal_socket_errno));

    if (0 <= rc) {
        return true;
    }
    return (EWOULDBLOCK != opal_socket_errno) && (EAGAIN != opal_socket_errno);
}

/*
 * Take an inbound connection, or decline it, and say which:
 * (1) if a connection has not been attempted, take it;
 * (2) if a connection has not been established, and the peer's process
 *     identifier is lower than ours, take it and drop our own dial;
 * otherwise keep the connection we have and decline this one.
 *
 * The return says who must close the socket, which is not quite the same
 * question as whether the connection came up. OPAL_SUCCESS means it is no
 * longer the caller's to close or to offer again; every other return
 * leaves it with the caller. On all but one path success also means the
 * endpoint is connected on it -- the exception is a handshake that fails
 * after the socket has been taken, marked where it happens below.
 *
 * Runs on the event base and must never block: an endpoint busy in its
 * own send or recv path is OPAL_ERR_RESOURCE_BUSY, to be come back for
 * rather than stalled behind.
 *
 * OPAL_ERR_IN_PROCESS is the other premature return, kept distinct from
 * busy because it ends differently: a dial of our own is in flight and
 * may yet win or die. The caller keeps the socket for both, but only busy
 * may eventually be given up on -- contention might never clear, while a
 * dial always resolves, so a caller that treats the two alike closes the
 * socket while the outcome is still unknown.
 */
int mca_btl_tcp_endpoint_adopt(mca_btl_base_endpoint_t *btl_endpoint, int sd)
{
    int cmpval, rc = OPAL_ERR_NOT_AVAILABLE;

    /* Before the locks, which this question does not need: endpoint_addr
     * is written once, where mca_btl_tcp_proc_insert() records the
     * endpoint on its proc, and is never cleared. Nothing to race with,
     * which is what lets mca_btl_tcp_endpoint_dial() ask it the same way.
     */
    if (NULL == btl_endpoint->endpoint_addr) {
        if (NULL != btl_endpoint->endpoint_btl->tcp_error_cb) {
            btl_endpoint->endpoint_btl
                ->tcp_error_cb(&btl_endpoint->endpoint_btl->super, MCA_BTL_ERROR_FLAGS_NONFATAL,
                               btl_endpoint->endpoint_proc->proc_opal,
                               "The endpoint addr is set to NULL (unsettling)");
        }
        return OPAL_ERR_NOT_AVAILABLE;
    }

    if (OPAL_THREAD_TRYLOCK(&btl_endpoint->endpoint_recv_lock)) {
        return OPAL_ERR_RESOURCE_BUSY;
    }
    if (OPAL_THREAD_TRYLOCK(&btl_endpoint->endpoint_send_lock)) {
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        return OPAL_ERR_RESOURCE_BUSY;
    }

    cmpval = opal_compare_proc(btl_endpoint->endpoint_proc->proc_opal->proc_name,
                               opal_proc_local_get()->proc_name);
    if ((btl_endpoint->endpoint_sd < 0)
        || (btl_endpoint->endpoint_state != MCA_BTL_TCP_CONNECTED && cmpval < 0)) {
        if (mca_btl_tcp_endpoint_abandoned(sd)) {
            MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "discarded, peer let go [adopt]");
            /* Declined rather than taken, so we may now have nothing at
             * all. The peer is not coming back for one -- it just let go --
             * so go back to CLOSED, and dial if we have something waiting,
             * which is the work adopting would have restarted. */
            if (btl_endpoint->endpoint_sd < 0) {
                btl_endpoint->endpoint_state = MCA_BTL_TCP_CLOSED;
                if (!opal_list_is_empty(&btl_endpoint->endpoint_frags)) {
                    (void) mca_btl_tcp_endpoint_start_connect(btl_endpoint);
                }
            }
            goto unlock_and_return;
        }
        if (MCA_BTL_TCP_CONNECT_ACK == btl_endpoint->endpoint_state) {
            /* Our own dial, dropped here in the peer's favour. Our handshake
             * is on it, so the peer may be holding it as a queued duplicate
             * and may still adopt it; silence would read there as a process
             * that died. */
            mca_btl_tcp_endpoint_send_fin(btl_endpoint);
        }
        mca_btl_tcp_endpoint_close(btl_endpoint);
        btl_endpoint->endpoint_sd = sd;
        if (mca_btl_tcp_endpoint_send_connect_ack(btl_endpoint) != OPAL_SUCCESS) {
            MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, true, " [adopt]");
            btl_endpoint->endpoint_state = MCA_BTL_TCP_FAILED;
            mca_btl_tcp_endpoint_close(btl_endpoint);
            /* The one place success does not mean connected. This
             * connection is dead, but the socket became ours above and
             * close() has already retired it, so the caller must neither
             * close it again nor come back with it -- and OPAL_SUCCESS is
             * the only return that says both. The outcome is carried by
             * the endpoint state and by the error callback close() has
             * raised. A caller that has to tell this from a real adoption
             * needs a return code of its own, not this one. */
            rc = OPAL_SUCCESS;
            goto unlock_and_return;
        }
        mca_btl_tcp_endpoint_event_init(btl_endpoint);
        MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "event_add(recv) [adopt]");
        opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
        if (mca_btl_tcp_event_base == opal_sync_event_base) {
            /* If no progress thread then raise the awareness of the default progress engine */
            opal_progress_event_users_increment();
        }
        mca_btl_tcp_endpoint_connected(btl_endpoint);

        MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "adopted");
        rc = OPAL_SUCCESS;
    } else if (MCA_BTL_TCP_CONNECTED != btl_endpoint->endpoint_state) {
        /* A "not yet", not a "no": a dial of our own has not landed.
         * Declining would close this socket on the strength of a
         * connection that is still hypothetical, and if ours turns out to
         * be dead the peer is left waiting on a socket we threw away, with
         * nothing to make it dial again.
         *
         * So keep it and be asked again. Either our dial lands, the peer
         * closes this one as the duplicate it then is, and the next offer
         * retires it through the abandoned() path above; or our dial dies,
         * the endpoint falls back to CLOSED, and the next offer takes this
         * socket. Both are reached without guessing, at the cost of
         * holding one descriptor for a tick or two.
         */
        MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "held, our dial is out [adopt]");
        rc = OPAL_ERR_IN_PROCESS;
    }

unlock_and_return:
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
    return rc;
}

/*
 * Make good on a connection this peer made to us that we let go of.
 *
 * Closing an inbound socket mid-handshake already means something
 * specific to the peer: mca_btl_tcp_endpoint_recv_connect_ack() reads the
 * eof as us having dialled in the other direction, so it drops its own
 * attempt and waits for ours, keeping whatever it had queued. That
 * reading is what makes the drop safe, and this is what makes it true.
 *
 * A CLOSED endpoint is dialled and a CONNECTED one is already everything
 * the peer was waiting for, so either settles the debt. In between does
 * not: a connection on its way may still die, and settling against it is
 * the same guess mca_btl_tcp_endpoint_adopt() refuses to make about its
 * own dial. OPAL_ERR_IN_PROCESS says to look again, which costs an entry
 * and no descriptor.
 *
 * Takes only the send lock, and only to try: the caller is on the event
 * base. OPAL_ERR_RESOURCE_BUSY again means come back.
 *
 * OPAL_ERR_NOT_AVAILABLE means there is no address to dial, which no
 * later turn will find either, so the caller reports the debt as unpayable
 * rather than retrying.
 */
int mca_btl_tcp_endpoint_dial(mca_btl_base_endpoint_t *btl_endpoint)
{
    int rc = OPAL_SUCCESS;

    if (NULL == btl_endpoint->endpoint_addr) {
        return OPAL_ERR_NOT_AVAILABLE;
    }
    if (OPAL_THREAD_TRYLOCK(&btl_endpoint->endpoint_send_lock)) {
        return OPAL_ERR_RESOURCE_BUSY;
    }
    if (MCA_BTL_TCP_CLOSED == btl_endpoint->endpoint_state) {
        MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "dialling back [dial]");
        rc = mca_btl_tcp_endpoint_start_connect(btl_endpoint);
    } else if (MCA_BTL_TCP_CONNECTED != btl_endpoint->endpoint_state) {
        MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "dial owed, one is out [dial]");
        rc = OPAL_ERR_IN_PROCESS;
    }
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
    return rc;
}

/*
 * Remove any event registrations associated with the socket
 * and update the endpoint state to reflect the connection has
 * been closed.
 */
void mca_btl_tcp_endpoint_close(mca_btl_base_endpoint_t *btl_endpoint)
{
    MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, false, "[close]");
    if (btl_endpoint->endpoint_sd < 0) {
        return;
    }
    btl_endpoint->endpoint_retries++;
    MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, false, "event_del(recv) [close]");
    opal_event_del(&btl_endpoint->endpoint_recv_event);
    if (mca_btl_tcp_event_base == opal_sync_event_base) {
        /* If no progress thread then lower the awareness of the default progress engine */
        opal_progress_event_users_decrement();
    }
    MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, false, "event_del(send) [close]");
    opal_event_del(&btl_endpoint->endpoint_send_event);

#if MCA_BTL_TCP_ENDPOINT_CACHE
    free(btl_endpoint->endpoint_cache);
    btl_endpoint->endpoint_cache = NULL;
    btl_endpoint->endpoint_cache_pos = NULL;
    btl_endpoint->endpoint_cache_length = 0;
#endif /* MCA_BTL_TCP_ENDPOINT_CACHE */

    /* send a message before closing to differentiate between failures and
     * clean disconnect during finalize */
    if (MCA_BTL_TCP_CONNECTED == btl_endpoint->endpoint_state) {
        mca_btl_tcp_endpoint_send_fin(btl_endpoint);
    }

    CLOSE_THE_SOCKET(btl_endpoint->endpoint_sd);
    btl_endpoint->endpoint_sd = -1;
    /**
     * If we keep failing to connect to the peer let the caller know about
     * this situation by triggering the callback on all pending fragments and
     * reporting the error. The upper layer has then the opportunity to
     * re-route or re-schedule the fragments.
     */
    if (MCA_BTL_TCP_FAILED == btl_endpoint->endpoint_state) {
        mca_btl_tcp_frag_t *frag = btl_endpoint->endpoint_send_frag;
        if (NULL == frag) {
            frag = (mca_btl_tcp_frag_t *) opal_list_remove_first(&btl_endpoint->endpoint_frags);
        }
        while (NULL != frag) {
            frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, OPAL_ERR_UNREACH);
            if (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP) {
                MCA_BTL_TCP_FRAG_RETURN(frag);
            }
            frag = (mca_btl_tcp_frag_t *) opal_list_remove_first(&btl_endpoint->endpoint_frags);
        }
        btl_endpoint->endpoint_send_frag = NULL;
        /* Let's report the error upstream */
        if (NULL != btl_endpoint->endpoint_btl->tcp_error_cb) {
            btl_endpoint->endpoint_btl
                ->tcp_error_cb((mca_btl_base_module_t *) btl_endpoint->endpoint_btl, 0,
                               btl_endpoint->endpoint_proc->proc_opal, "Socket closed");
        }
    } else {
        btl_endpoint->endpoint_state = MCA_BTL_TCP_CLOSED;
    }
}

/*
 *  Setup endpoint state to reflect that connection has been established,
 *  and start any pending sends. This function should be called with the
 *  send lock locked.
 */

static void mca_btl_tcp_endpoint_connected(mca_btl_base_endpoint_t *btl_endpoint)
{
    /* setup socket options */
    assert(MCA_BTL_TCP_CONNECTED != btl_endpoint->endpoint_state);
    btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECTED;
    btl_endpoint->endpoint_retries = 0;
    MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, true, "READY [endpoint_connected]");

    if (opal_list_get_size(&btl_endpoint->endpoint_frags) > 0) {
        if (NULL == btl_endpoint->endpoint_send_frag) {
            btl_endpoint->endpoint_send_frag = (mca_btl_tcp_frag_t *) opal_list_remove_first(
                &btl_endpoint->endpoint_frags);
        }
        MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "event_add(send) [endpoint_connected]");
        opal_event_add(&btl_endpoint->endpoint_send_event, 0);
    }
}

/*
 *  Receive the endpoints globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 *
 *  NOTE: The return codes from this function are checked in
 *  mca_btl_tcp_endpoint_recv_handler().  Don't change them here
 *  without also changing the handling in _recv_handler()!
 */
static int mca_btl_tcp_endpoint_recv_connect_ack(mca_btl_base_endpoint_t *btl_endpoint)
{
    size_t retval, len = strlen(mca_btl_tcp_magic_id_string);
    mca_btl_tcp_proc_t *btl_proc = btl_endpoint->endpoint_proc;
    opal_process_name_t guid;

    mca_btl_tcp_endpoint_hs_msg_t hs_msg;
    retval = mca_btl_tcp_recv_blocking(btl_endpoint->endpoint_sd, &hs_msg, sizeof(hs_msg));

    if (sizeof(hs_msg) != retval) {
        mca_btl_tcp_endpoint_close(btl_endpoint);
        if (0 == retval) {
            /* If we get zero bytes, the peer closed the socket. This
               can happen when the two peers started the connection
               protocol simultaneously. Just report the problem
               upstream. */
            return OPAL_ERROR;
        }
        opal_show_help("help-mpi-btl-tcp.txt", "client handshake fail", true,
                       opal_process_info.nodename, getpid(),
                       "did not receive entire connect ACK from peer");
        return OPAL_ERR_BAD_PARAM;
    }
    if (0 != strncmp(hs_msg.magic_id, mca_btl_tcp_magic_id_string, len)) {
        mca_btl_tcp_endpoint_close(btl_endpoint);
        opal_show_help("help-mpi-btl-tcp.txt", "server did not receive magic string", true,
                       opal_process_info.nodename, getpid(), "client", hs_msg.magic_id,
                       "string value");
        return OPAL_ERR_BAD_PARAM;
    }

    guid = hs_msg.guid;
    OPAL_PROCESS_NAME_NTOH(guid);
    /* compare this to the expected values */
    /* TODO: this deserve a little bit more thinking as we are not supposed
     * to be able to exchange the opal_process_name_t over the network.
     */
    if (0 != opal_compare_proc(btl_proc->proc_opal->proc_name, guid)) {
        BTL_ERROR(("received unexpected process identifier: got %s expected %s",
                   OPAL_NAME_PRINT(guid), OPAL_NAME_PRINT(btl_proc->proc_opal->proc_name)));
        btl_endpoint->endpoint_state = MCA_BTL_TCP_FAILED;
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
    if (setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (char *) &optval, sizeof(optval)) < 0) {
        BTL_ERROR(("setsockopt(TCP_NODELAY) failed: %s (%d)", strerror(opal_socket_errno),
                   opal_socket_errno));
    }
#endif
#if defined(SO_SNDBUF)
    if (mca_btl_tcp_component.tcp_sndbuf > 0
        && setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (char *) &mca_btl_tcp_component.tcp_sndbuf,
                      sizeof(int))
               < 0) {
        BTL_ERROR(("setsockopt(SO_SNDBUF) failed: %s (%d)", strerror(opal_socket_errno),
                   opal_socket_errno));
    }
#endif
#if defined(SO_RCVBUF)
    if (mca_btl_tcp_component.tcp_rcvbuf > 0
        && setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (char *) &mca_btl_tcp_component.tcp_rcvbuf,
                      sizeof(int))
               < 0) {
        BTL_ERROR(("setsockopt(SO_RCVBUF) failed: %s (%d)", strerror(opal_socket_errno),
                   opal_socket_errno));
    }
#endif
#if defined(SO_NOSIGPIPE)
    /* Some BSD flavors generate EPIPE when we write to a disconnected peer. We need
     * the prevent this signal to be able to trap socket shutdown and cleanly release
     * the endpoint.
     */
    int optval2 = 1;
    if (setsockopt(sd, SOL_SOCKET, SO_NOSIGPIPE, (char *) &optval2, sizeof(optval2)) < 0) {
        BTL_ERROR(("setsockopt(SO_NOSIGPIPE) failed: %s (%d)", strerror(opal_socket_errno),
                   opal_socket_errno));
    }
#endif
}

/*
 *  Start a connection to the endpoint. This will likely not complete,
 *  as the socket is set to non-blocking, so register for event
 *  notification of connect completion. On connection we send our
 *  globally unique process identifier to the endpoint and wait for
 *  the endpoint response.
 */
static int mca_btl_tcp_endpoint_start_connect(mca_btl_base_endpoint_t *btl_endpoint)
{
    int rc, flags;
    struct sockaddr_storage endpoint_addr;
    /* By default consider a IPv4 connection */
    uint16_t af_family = AF_INET;
    opal_socklen_t addrlen = sizeof(struct sockaddr_in);

#if OPAL_ENABLE_IPV6
    if (AF_INET6 == btl_endpoint->endpoint_addr->addr_family) {
        af_family = AF_INET6;
        addrlen = sizeof(struct sockaddr_in6);
    }
#endif
    assert(btl_endpoint->endpoint_sd < 0);
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
    if ((flags = fcntl(btl_endpoint->endpoint_sd, F_GETFL, 0)) < 0) {
        opal_show_help("help-mpi-btl-tcp.txt", "socket flag fail", true, opal_process_info.nodename,
                       getpid(), "fcntl(sd, F_GETFL, 0)", strerror(opal_socket_errno),
                       opal_socket_errno);
        /* Upper layer will handler the error */
        return OPAL_ERR_UNREACH;
    } else {
        flags |= O_NONBLOCK;
        if (fcntl(btl_endpoint->endpoint_sd, F_SETFL, flags) < 0) {
            opal_show_help("help-mpi-btl-tcp.txt", "socket flag fail", true,
                           opal_process_info.nodename, getpid(),
                           "fcntl(sd, F_SETFL, flags & O_NONBLOCK)", strerror(opal_socket_errno),
                           opal_socket_errno);
            /* Upper layer will handler the error */
            return OPAL_ERR_UNREACH;
        }
    }

    /* start the connect - will likely fail with EINPROGRESS */
    mca_btl_tcp_proc_tosocks(btl_endpoint->endpoint_addr, &endpoint_addr);

    /* Bind the socket to one of the addresses associated with
     * this btl module.  This sets the source IP to one of the
     * addresses shared in modex, so that the destination rank
     * can properly pair btl modules, even in cases where Linux
     * might do something unexpected with routing */
    if (endpoint_addr.ss_family == AF_INET) {
        assert(NULL != &btl_endpoint->endpoint_btl->tcp_ifaddr);
        if (bind(btl_endpoint->endpoint_sd,
                 (struct sockaddr *) &btl_endpoint->endpoint_btl->tcp_ifaddr,
                 sizeof(struct sockaddr_in))
            < 0) {
            BTL_ERROR(
                ("bind on local address (%s:%d) failed: %s (%d)",
                 opal_net_get_hostname((struct sockaddr *) &btl_endpoint->endpoint_btl->tcp_ifaddr),
                 htons(((struct sockaddr_in *) &btl_endpoint->endpoint_btl->tcp_ifaddr)->sin_port),
                 strerror(opal_socket_errno), opal_socket_errno));

            CLOSE_THE_SOCKET(btl_endpoint->endpoint_sd);
            return OPAL_ERROR;
        }
    }
#if OPAL_ENABLE_IPV6
    if (endpoint_addr.ss_family == AF_INET6) {
        assert(NULL != &btl_endpoint->endpoint_btl->tcp_ifaddr);
        if (bind(btl_endpoint->endpoint_sd,
                 (struct sockaddr *) &btl_endpoint->endpoint_btl->tcp_ifaddr,
                 sizeof(struct sockaddr_in6))
            < 0) {
            BTL_ERROR(
                ("bind on local address (%s:%d) failed: %s (%d)",
                 opal_net_get_hostname((struct sockaddr *) &btl_endpoint->endpoint_btl->tcp_ifaddr),
                 htons(
                     ((struct sockaddr_in6 *) &btl_endpoint->endpoint_btl->tcp_ifaddr)->sin6_port),
                 strerror(opal_socket_errno), opal_socket_errno));

            CLOSE_THE_SOCKET(btl_endpoint->endpoint_sd);
            return OPAL_ERROR;
        }
    }
#endif
    opal_output_verbose(10, opal_btl_base_framework.framework_output,
                        "btl: tcp: attempting to connect() to %s address %s on port %d",
                        OPAL_NAME_PRINT(btl_endpoint->endpoint_proc->proc_opal->proc_name),
                        opal_net_get_hostname((struct sockaddr *) &endpoint_addr),
                        ntohs(btl_endpoint->endpoint_addr->addr_port));

    if (0 == connect(btl_endpoint->endpoint_sd, (struct sockaddr *) &endpoint_addr, addrlen)) {
        opal_output_verbose(10, opal_btl_base_framework.framework_output,
                            "btl:tcp: connect() to %s:%d completed",
                            opal_net_get_hostname((struct sockaddr *) &endpoint_addr),
                            ntohs(((struct sockaddr_in *) &endpoint_addr)->sin_port));
        /* send our globally unique process identifier to the endpoint */
        if ((rc = mca_btl_tcp_endpoint_send_connect_ack(btl_endpoint)) == OPAL_SUCCESS) {
            btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECT_ACK;
            MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "event_add(recv) [start_connect]");
            opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
            if (mca_btl_tcp_event_base == opal_sync_event_base) {
                /* If no progress thread then raise the awarness of the default progress engine */
                opal_progress_event_users_increment();
            }
            return OPAL_SUCCESS;
        }
        /* We connected to the peer, but he close the socket before we got a chance to send our guid
         */
        MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, true, "dropped connection [start_connect]");
    } else {
        /* non-blocking so wait for completion */
        if (opal_socket_errno == EINPROGRESS || opal_socket_errno == EWOULDBLOCK) {
            btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECTING;
            MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "event_add(send) [start_connect]");
            MCA_BTL_TCP_ACTIVATE_EVENT(&btl_endpoint->endpoint_send_event, 0);
            opal_output_verbose(30, opal_btl_base_framework.framework_output,
                                "btl:tcp: would block, so allowing background progress");
            return OPAL_SUCCESS;
        }
    }

    {
        char *address;
        address = opal_net_get_hostname((struct sockaddr *) &endpoint_addr);
        BTL_PEER_ERROR(btl_endpoint->endpoint_proc->proc_opal,
                       ("Unable to connect to the peer %s on port %d: %s\n", address,
                        ntohs(btl_endpoint->endpoint_addr->addr_port),
                        strerror(opal_socket_errno)));
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
static int mca_btl_tcp_endpoint_complete_connect(mca_btl_base_endpoint_t *btl_endpoint)
{
    int so_error = 0;
    opal_socklen_t so_length = sizeof(so_error);
    struct sockaddr_storage endpoint_addr;

    /* Delete the send event notification, as the next step is waiting for the ack
     * from the peer. Once this ack is received we will deal with the send notification
     * accordingly.
     */
    opal_event_del(&btl_endpoint->endpoint_send_event);

    mca_btl_tcp_proc_tosocks(btl_endpoint->endpoint_addr, &endpoint_addr);

    /* check connect completion status */
    if (getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_ERROR, (char *) &so_error, &so_length)
        < 0) {
        opal_show_help("help-mpi-btl-tcp.txt", "socket flag fail", true, opal_process_info.nodename,
                       getpid(), "fcntl(sd, F_GETFL, 0)", strerror(opal_socket_errno),
                       opal_socket_errno);
        BTL_ERROR(("getsockopt() to %s:%d failed: %s (%d)",
                   opal_net_get_hostname((struct sockaddr *) &endpoint_addr),
                   ((struct sockaddr_in *) &endpoint_addr)->sin_port, strerror(opal_socket_errno),
                   opal_socket_errno));
        btl_endpoint->endpoint_state = MCA_BTL_TCP_FAILED;
        mca_btl_tcp_endpoint_close(btl_endpoint);
        return OPAL_ERROR;
    }
    if (so_error == EINPROGRESS || so_error == EWOULDBLOCK) {
        return OPAL_SUCCESS;
    }
    if (so_error != 0) {
        if (mca_btl_base_warn_peer_error || mca_btl_base_verbose > 0) {
            char *msg;
            opal_asprintf(&msg, "connect() to %s:%d failed",
                          opal_net_get_hostname((struct sockaddr *) &endpoint_addr),
                          ntohs(((struct sockaddr_in *) &endpoint_addr)->sin_port));
            opal_show_help("help-mpi-btl-tcp.txt", "client connect fail", true,
                           opal_process_info.nodename, getpid(), msg, strerror(so_error), so_error);
            free(msg);
        }
        btl_endpoint->endpoint_state = MCA_BTL_TCP_FAILED;
        mca_btl_tcp_endpoint_close(btl_endpoint);
        return OPAL_ERROR;
    }

    opal_output_verbose(
        10, opal_btl_base_framework.framework_output,
        "btl:tcp: connect() to %s:%d completed (complete_connect), sending connect ACK",
        opal_net_get_hostname((struct sockaddr *) &endpoint_addr),
        ntohs(((struct sockaddr_in *) &endpoint_addr)->sin_port));

    if (mca_btl_tcp_endpoint_send_connect_ack(btl_endpoint) == OPAL_SUCCESS) {
        btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECT_ACK;
        opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
        if (mca_btl_tcp_event_base == opal_sync_event_base) {
            /* If no progress thread then raise the awarness of the default progress engine */
            opal_progress_event_users_increment();
        }
        MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, false, "event_add(recv) [complete_connect]");
        return OPAL_SUCCESS;
    }
    MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, false, " [complete_connect]");
    btl_endpoint->endpoint_state = MCA_BTL_TCP_FAILED;
    mca_btl_tcp_endpoint_close(btl_endpoint);
    return OPAL_ERROR;
}

/*
 * A file descriptor is available/ready for recv. Check the state
 * of the socket and take the appropriate action.
 */

static void mca_btl_tcp_endpoint_recv_handler(int sd, short flags, void *user)
{
    mca_btl_base_endpoint_t *btl_endpoint = (mca_btl_base_endpoint_t *) user;

    /* Make sure we don't have a race between a thread that remove the
     * recv event, and one event already scheduled.
     */
    if (sd != btl_endpoint->endpoint_sd) {
        return;
    }

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
     * will be eventually triggered again shortly.
     */
    if (OPAL_THREAD_TRYLOCK(&btl_endpoint->endpoint_recv_lock)) {
        return;
    }

    switch (btl_endpoint->endpoint_state) {
    case MCA_BTL_TCP_CONNECT_ACK: {
        int rc = mca_btl_tcp_endpoint_recv_connect_ack(btl_endpoint);
        if (OPAL_SUCCESS == rc) {
            /* we are now connected. Start sending the data */
            OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
            mca_btl_tcp_endpoint_connected(btl_endpoint);
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
            MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, true, "connected");
        } else if (OPAL_ERR_BAD_PARAM == rc || OPAL_ERROR == rc) {
            /* If we get a BAD_PARAM, it means that it probably wasn't
               an OMPI process on the other end of the socket (e.g.,
               the magic string ID failed). recv_connect_ack already cleaned
               up the socket. */
            /* If we get OPAL_ERROR, the other end closed the connection
             * because it has initiated a symmetrical connection on its end.
             * recv_connect_ack already cleaned up the socket. */
        } else {
            /* Otherwise, it probably *was* an OMPI peer process on
               the other end, and something bad has probably
               happened.  */
            mca_btl_tcp_module_t *m = btl_endpoint->endpoint_btl;

            /* Fail up to the PML */
            if (NULL != m->tcp_error_cb) {
                m->tcp_error_cb(
                    (mca_btl_base_module_t *) m, MCA_BTL_ERROR_FLAGS_FATAL,
                    btl_endpoint->endpoint_proc->proc_opal,
                    "TCP ACK is neither SUCCESS nor ERR (something bad has probably happened)");
            }
        }
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        return;
    }
    case MCA_BTL_TCP_CONNECTED: {
        mca_btl_tcp_frag_t *frag;

        frag = btl_endpoint->endpoint_recv_frag;
        if (NULL == frag) {
            if (mca_btl_tcp_module.super.btl_max_send_size
                > mca_btl_tcp_module.super.btl_eager_limit) {
                MCA_BTL_TCP_FRAG_ALLOC_MAX(frag);
            } else {
                MCA_BTL_TCP_FRAG_ALLOC_EAGER(frag);
            }

            if (NULL == frag) {
                OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
                return;
            }
            MCA_BTL_TCP_FRAG_INIT_DST(frag, btl_endpoint);
        }

#if MCA_BTL_TCP_ENDPOINT_CACHE
        assert(0 == btl_endpoint->endpoint_cache_length);
    data_still_pending_on_endpoint:
#endif /* MCA_BTL_TCP_ENDPOINT_CACHE */
        /* check for completion of non-blocking recv on the current fragment */
        if (mca_btl_tcp_frag_recv(frag, btl_endpoint->endpoint_sd) == false) {
            btl_endpoint->endpoint_recv_frag = frag;
        } else {
            btl_endpoint->endpoint_recv_frag = NULL;
            if (MCA_BTL_TCP_HDR_TYPE_SEND == frag->hdr.type) {
                mca_btl_active_message_callback_t *reg = mca_btl_base_active_message_trigger
                                                         + frag->hdr.base.tag;
                const mca_btl_base_receive_descriptor_t desc
                    = {.endpoint = btl_endpoint,
                       .des_segments = frag->base.des_segments,
                       .des_segment_count = frag->base.des_segment_count,
                       .tag = frag->hdr.base.tag,
                       .cbdata = reg->cbdata};
                reg->cbfunc(&frag->btl->super, &desc);
            }
#if MCA_BTL_TCP_ENDPOINT_CACHE
            if (0 != btl_endpoint->endpoint_cache_length) {
                /* If the cache still contain some data we can reuse the same fragment
                 * until we flush it completely.
                 */
                MCA_BTL_TCP_FRAG_INIT_DST(frag, btl_endpoint);
                goto data_still_pending_on_endpoint;
            }
#endif /* MCA_BTL_TCP_ENDPOINT_CACHE */
            MCA_BTL_TCP_FRAG_RETURN(frag);
        }
#if MCA_BTL_TCP_ENDPOINT_CACHE
        assert(0 == btl_endpoint->endpoint_cache_length);
#endif /* MCA_BTL_TCP_ENDPOINT_CACHE */
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

static void mca_btl_tcp_endpoint_send_handler(int sd, short flags, void *user)
{
    mca_btl_tcp_endpoint_t *btl_endpoint = (mca_btl_tcp_endpoint_t *) user;

    /* if another thread is already here, give up */
    if (OPAL_THREAD_TRYLOCK(&btl_endpoint->endpoint_send_lock)) {
        return;
    }

    switch (btl_endpoint->endpoint_state) {
    case MCA_BTL_TCP_CONNECTING:
        mca_btl_tcp_endpoint_complete_connect(btl_endpoint);
        break;
    case MCA_BTL_TCP_CONNECTED:
        /* complete the current send */
        while (NULL != btl_endpoint->endpoint_send_frag) {
            mca_btl_tcp_frag_t *frag = btl_endpoint->endpoint_send_frag;
            int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

            assert(btl_endpoint->endpoint_state == MCA_BTL_TCP_CONNECTED);
            if (mca_btl_tcp_frag_send(frag, btl_endpoint->endpoint_sd) == false) {
                break;
            }
            /* progress any pending sends */
            btl_endpoint->endpoint_send_frag = (mca_btl_tcp_frag_t *) opal_list_remove_first(
                &btl_endpoint->endpoint_frags);

            /* if required - update request status and release fragment */
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
            assert(frag->base.des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
            if (NULL != frag->base.des_cbfunc) {
                frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, frag->rc);
            }
            if (btl_ownership) {
                MCA_BTL_TCP_FRAG_RETURN(frag);
            }
            /* if we fail to take the lock simply return. In the worst case the
             * send_handler will be triggered once more, and as there will be
             * nothing to send the handler will be deleted.
             */
            if (OPAL_THREAD_TRYLOCK(&btl_endpoint->endpoint_send_lock)) {
                return;
            }
        }

        /* if nothing else to do unregister for send event notifications */
        if (NULL == btl_endpoint->endpoint_send_frag) {
            MCA_BTL_TCP_ENDPOINT_DUMP(10, btl_endpoint, false,
                                      "event_del(send) [endpoint_send_handler]");
            opal_event_del(&btl_endpoint->endpoint_send_event);
        }
        break;
    case MCA_BTL_TCP_FAILED:
        MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, true,
                                  "event_del(send) [endpoint_send_handler:error]");
        opal_event_del(&btl_endpoint->endpoint_send_event);
        break;
    default:
        BTL_ERROR(("invalid connection state (%d)", btl_endpoint->endpoint_state));
        MCA_BTL_TCP_ENDPOINT_DUMP(1, btl_endpoint, true,
                                  "event_del(send) [endpoint_send_handler:error]");
        opal_event_del(&btl_endpoint->endpoint_send_event);
        break;
    }
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
}
