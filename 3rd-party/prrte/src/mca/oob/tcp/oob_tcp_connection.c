/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies Ltd. All rights reserved.
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <fcntl.h>
#include <sys/socket.h>

#ifdef HAVE_SYS_UIO_H
#    include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#    include <net/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#include "src/include/prte_socket_errno.h"
#ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif
#ifdef HAVE_NETINET_TCP_H
#    include <netinet/tcp.h>
#endif

#include "prte_stdint.h"
#include "src/event/event-internal.h"
#include "src/mca/prtebacktrace/prtebacktrace.h"
#include "src/util/error.h"
#include "src/util/pmix_fd.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_show_help.h"
#include "types.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/mca/prtereachable/base/base.h"
#include "src/mca/state/state.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_wait.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_show_help.h"

#include "oob_tcp.h"
#include "oob_tcp_common.h"
#include "oob_tcp_connection.h"
#include "oob_tcp_peer.h"
#include "src/mca/oob/tcp/oob_tcp_common.h"
#include "src/mca/oob/tcp/oob_tcp_component.h"
#include "src/mca/oob/tcp/oob_tcp_connection.h"
#include "src/mca/oob/tcp/oob_tcp_peer.h"

static void tcp_peer_event_init(prte_oob_tcp_peer_t *peer);
static int tcp_peer_send_connect_ack(prte_oob_tcp_peer_t *peer);
static int tcp_peer_send_connect_nack(int sd, pmix_proc_t *name);
static int tcp_peer_send_blocking(int sd, void *data, size_t size);
static bool tcp_peer_recv_blocking(prte_oob_tcp_peer_t *peer, int sd, void *data, size_t size);
static void tcp_peer_connected(prte_oob_tcp_peer_t *peer);

static int tcp_peer_create_socket(prte_oob_tcp_peer_t *peer, sa_family_t family)
{
    int flags;

    if (peer->sd >= 0) {
        return PRTE_SUCCESS;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_oob_base_framework.framework_output,
                         "%s oob:tcp:peer creating socket to %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name))));
    peer->sd = socket(family, SOCK_STREAM, 0);
    if (peer->sd < 0) {
        pmix_output(0, "%s-%s tcp_peer_create_socket: socket() failed: %s (%d)\n",
                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                    strerror(prte_socket_errno), prte_socket_errno);
        return PRTE_ERR_UNREACH;
    }

    /* Set this fd to be close-on-exec so that any subsequent children don't see it */
    if (pmix_fd_set_cloexec(peer->sd) != PRTE_SUCCESS) {
        pmix_output(0, "%s unable to set socket to CLOEXEC", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        close(peer->sd);
        peer->sd = -1;
        return PRTE_ERROR;
    }

    /* setup socket options */
    prte_oob_tcp_set_socket_options(peer->sd);

    /* setup event callbacks */
    tcp_peer_event_init(peer);

    /* setup the socket as non-blocking */
    if (peer->sd >= 0) {
        if ((flags = fcntl(peer->sd, F_GETFL, 0)) < 0) {
            pmix_output(0, "%s-%s tcp_peer_connect: fcntl(F_GETFL) failed: %s (%d)\n",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                        strerror(prte_socket_errno), prte_socket_errno);
        } else {
            flags |= O_NONBLOCK;
            if (fcntl(peer->sd, F_SETFL, flags) < 0)
                pmix_output(0, "%s-%s tcp_peer_connect: fcntl(F_SETFL) failed: %s (%d)\n",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                            strerror(prte_socket_errno), prte_socket_errno);
        }
    }

    return PRTE_SUCCESS;
}

/*
 * Try connecting to a peer - cycle across all known addresses
 * until one succeeds.
 */
void prte_oob_tcp_peer_try_connect(int fd, short args, void *cbdata)
{
    pmix_list_t *local_list = &prte_mca_oob_tcp_component.local_ifs, *remote_list;
    int rc, i, j, local_if_count, remote_if_count, best, best_i = 0, best_j = 0;
    prte_oob_tcp_conn_op_t *op = (prte_oob_tcp_conn_op_t *) cbdata;
    prte_reachable_t *results = NULL;
    volatile pmix_list_item_t *ptr;
    prte_socklen_t addrlen = 0;
    prte_oob_tcp_peer_t *peer;
    prte_oob_tcp_addr_t *addr;
    bool connected = false;
    pmix_pif_t *intf;
    char *host;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    remote_list = PMIX_NEW(pmix_list_t);
    if (NULL == remote_list) {
        pmix_output(0, "%s CANNOT CREATE SOCKET, OUT OF MEMORY",
                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_COMM_FAILED);
        return;
    }

    PMIX_ACQUIRE_OBJECT(op);
    peer = op->peer;

    /* Construct a list of remote pmix_pif_t from peer */
    PMIX_LIST_FOREACH(addr, &peer->addrs, prte_oob_tcp_addr_t)
    {
        intf = PMIX_NEW(pmix_pif_t);
        if (NULL == intf) {
            pmix_output(0, "%s CANNOT CREATE SOCKET, OUT OF MEMORY",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_COMM_FAILED);
            goto cleanup;
        }
        intf->af_family = addr->addr.ss_family;
        memcpy(&intf->if_addr, &addr->addr, sizeof(struct sockaddr_storage));
        intf->if_mask = addr->if_mask;
        /* We do not pass along bandwidth information, setting as arbitrary non
         * zero value
         */
        intf->if_bandwidth = 1;
        pmix_list_append(remote_list, &(intf->super));
    }
    local_if_count = pmix_list_get_size(local_list);
    remote_if_count = pmix_list_get_size(remote_list);

    results = prte_reachable.reachable(local_list, remote_list);

    /* Find match, bind socket. If connect attempt failed, move to next */
    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s prte_tcp_peer_try_connect: "
                        "attempting to connect to proc %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)));

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s prte_tcp_peer_try_connect: "
                        "attempting to connect to proc %s on socket %d",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                        peer->sd);

    /* Loops over the reachable bitmap. This should only run once, but
     * if a connection does fail even after being declared as reachable,
     * it will try remaining connections.
     */
    while (!connected) {
        /* Select the best connection. This is not going to be a large
         * table and should only run once in the normal case, so no sorting
         * is attempted.
         */
        best = 0;
        for (i = 0; i < local_if_count; i++) {
            for (j = 0; j < remote_if_count; j++) {
                if (best < results->weights[i][j]) {
                    best = results->weights[i][j];
                    best_i = i;
                    best_j = j;
                }
            }
        }
        /* If no connections are found, skip the rest of the connecting logic
         * and exit the loop.
         */
        if (0 == best) {
            break;
        }
        /* Set this entry to be 0 so it won't be selected when looking for
         * the next best connection
         */
        results->weights[best_i][best_j] = 0;
        ptr = peer->addrs.pmix_list_sentinel.pmix_list_next;
        for (j = 0; j < best_j; j++) {
            ptr = ptr->pmix_list_next;
        }
        /* Record the peer address we are using */
        peer->active_addr = (prte_oob_tcp_addr_t *) ptr;
        addr = peer->active_addr;
        /* Grab the local address we are using to bind the socket with */
        ptr = prte_mca_oob_tcp_component.local_ifs.pmix_list_sentinel.pmix_list_next;
        for (i = 0; i < best_i; i++) {
            ptr = ptr->pmix_list_next;
        }
        intf = (pmix_pif_t *) ptr;
        pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                            "%s prte_tcp_peer_try_connect: "
                            "attempting to connect to proc %s on %s:%d - %d retries",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                            pmix_net_get_hostname((struct sockaddr *) &addr->addr),
                            pmix_net_get_port((struct sockaddr *) &addr->addr), addr->retries);
        if (MCA_OOB_TCP_FAILED == addr->state) {
            pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                                "%s prte_tcp_peer_try_connect: %s:%d is down",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                pmix_net_get_hostname((struct sockaddr *) &addr->addr),
                                pmix_net_get_port((struct sockaddr *) &addr->addr));
            continue;
        }
        if (prte_mca_oob_tcp_component.max_retries < addr->retries) {
            pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                                "%s prte_tcp_peer_try_connect: %s:%d retries exceeded",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                pmix_net_get_hostname((struct sockaddr *) &addr->addr),
                                pmix_net_get_port((struct sockaddr *) &addr->addr));
            continue;
        }
        addrlen = addr->addr.ss_family == AF_INET6 ? sizeof(struct sockaddr_in6)
                                                   : sizeof(struct sockaddr_in);

        /* Since we are manually binding sockets now, we must
         * close and create a new socket if we are binding to a
         * new address.
         */
        if (peer->sd >= 0) {
            CLOSE_THE_SOCKET(peer->sd);
            peer->sd = -1;
        }
        rc = tcp_peer_create_socket(peer, addr->addr.ss_family);

        if (PRTE_SUCCESS != rc) {
            /* FIXME: we cannot create a TCP socket - this spans
             * all interfaces, so all we can do is report
             * back to the component that this peer is
             * unreachable so it can remove the peer
             * from its list and report back to the base
             * NOTE: this could be a reconnect attempt,
             * so we also need to mark any queued messages
             * and return them as "unreachable"
             */
            pmix_output(0, "%s CANNOT CREATE SOCKET", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_COMM_FAILED);
            goto cleanup;
        }

        /* Bind the socket manually to selected address */
        if (bind(peer->sd, (struct sockaddr *) &intf->if_addr, addrlen) < 0) {
            /* If we cannot bind to this address, set remaining entries
             * for this address from the reachable table to no connection
             * and try a new connection.
             */
            if ((EADDRINUSE == prte_socket_errno) || (EADDRNOTAVAIL == prte_socket_errno)) {
                for (j = 0; j < remote_if_count; j++) {
                    results->weights[best_i][j] = 0;
                }
                continue;
            }
            /* If we have another bind issue, something has gone horribly
             * wrong.
             */
            pmix_output(0, "%s bind() failed, can't recover : %s (%d)",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), strerror(prte_socket_errno),
                        prte_socket_errno);

            CLOSE_THE_SOCKET(peer->sd);
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_COMM_FAILED);
            goto cleanup;
        }

    retry_connect:
        addr->retries++;

        rc = connect(peer->sd, (struct sockaddr *) &addr->addr, addrlen);
        if (rc < 0) {
            /* non-blocking so wait for completion */
            if (prte_socket_errno == EINPROGRESS || prte_socket_errno == EWOULDBLOCK) {
                pmix_output_verbose(
                    OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                    "%s waiting for connect completion to %s - activating send event",
                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&peer->name));
                /* just ensure the send_event is active */
                if (!peer->send_ev_active) {
                    prte_event_add(&peer->send_event, 0);
                    peer->send_ev_active = true;
                }
                PMIX_RELEASE(op);
                goto out;
            }

            /* Some kernels (Linux 2.6) will automatically software
             * abort a connection that was ECONNREFUSED on the last
             * attempt, without even trying to establish the
             * connection.  Handle that case in a semi-rational
             * way by trying twice before giving up
             */
            if (ECONNABORTED == prte_socket_errno) {
                if (addr->retries < prte_mca_oob_tcp_component.max_retries) {
                    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT,
                                        prte_oob_base_framework.framework_output,
                                        "%s connection aborted by OS to %s - retrying",
                                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                        PRTE_NAME_PRINT(&peer->name));
                    goto retry_connect;
                } else {
                    /* We were unsuccessful in establishing this connection, and are
                     * not likely to suddenly become successful, so rotate to next option
                     */
                    addr->state = MCA_OOB_TCP_FAILED;
                    continue;
                }
            }
        } else {
            /* connection succeeded */
            addr->retries = 0;
            connected = true;
            peer->num_retries = 0;
            break;
        }
    } // End of looping over reachable bitmap entries

    /* End of attempting connection */
    if (!connected) {
        /* it could be that the intended recipient just hasn't
         * started yet. if requested, wait awhile and try again
         * unless/until we hit the maximum number of retries */
        if (0 < prte_mca_oob_tcp_component.retry_delay) {
            if (prte_mca_oob_tcp_component.max_recon_attempts < 0
                || peer->num_retries < prte_mca_oob_tcp_component.max_recon_attempts) {
                struct timeval tv;
                /* close the current socket */
                CLOSE_THE_SOCKET(peer->sd);
                /* reset the addr states */
                PMIX_LIST_FOREACH(addr, &peer->addrs, prte_oob_tcp_addr_t)
                {
                    addr->state = MCA_OOB_TCP_UNCONNECTED;
                    addr->retries = 0;
                }
                /* give it awhile and try again */
                tv.tv_sec = prte_mca_oob_tcp_component.retry_delay;
                tv.tv_usec = 0;
                ++peer->num_retries;
                PRTE_RETRY_TCP_CONN_STATE(peer, prte_oob_tcp_peer_try_connect, &tv);
                goto cleanup;
            }
        }
        /* no address succeeded, so we cannot reach this peer */
        peer->state = MCA_OOB_TCP_FAILED;
        host = prte_get_proc_hostname(&(peer->name));
        if (NULL == host && NULL != peer->active_addr) {
            host = pmix_net_get_hostname((struct sockaddr *) &(peer->active_addr->addr));
        }
        /* use an pmix_output here instead of show_help as we may well
         * not be connected to the HNP at this point */
        pmix_output(prte_clean_output,
                    "------------------------------------------------------------\n"
                    "A process or daemon was unable to complete a TCP connection\n"
                    "to another process:\n"
                    "  Local host:    %s\n"
                    "  Remote host:   %s\n"
                    "This is usually caused by a firewall on the remote host. Please\n"
                    "check that any firewall (e.g., iptables) has been disabled and\n"
                    "try again.\n"
                    "------------------------------------------------------------",
                    prte_process_info.nodename, (NULL == host) ? "<unknown>" : host);
        /* close the socket */
        CLOSE_THE_SOCKET(peer->sd);
        /* let the TCP component know that this module failed to make
         * the connection so it can do some bookkeeping and fail back
         * to the OOB level so another component can try. This will activate
         * an event in the component event base, and so it will fire async
         * from us if we are in our own progress thread
         */
        PRTE_ACTIVATE_TCP_CMP_OP(peer, prte_mca_oob_tcp_component_failed_to_connect);
        /* FIXME: post any messages in the send queue back to the OOB
         * level for reassignment
         */
        if (NULL != peer->send_msg) {
        }
        while (NULL != pmix_list_remove_first(&peer->send_queue)) {
        }
        goto cleanup;
    }

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s prte_tcp_peer_try_connect: "
                        "Connection to proc %s succeeded",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&peer->name));

    /* setup our recv to catch the return ack call */
    if (!peer->recv_ev_active) {
        prte_event_add(&peer->recv_event, 0);
        peer->recv_ev_active = true;
    }

    /* send our globally unique process identifier to the peer */
    if (PRTE_SUCCESS == (rc = tcp_peer_send_connect_ack(peer))) {
        peer->state = MCA_OOB_TCP_CONNECT_ACK;
    } else if (PRTE_ERR_UNREACH == rc) {
        /* this could happen if we are in a race condition where both
         * we and the peer are trying to connect at the same time. If I
         * am the higher vpid, then retry the connection - otherwise,
         * step aside for now */
        int cmpval = prte_util_compare_name_fields(PRTE_NS_CMP_ALL, PRTE_PROC_MY_NAME, &peer->name);
        if (PRTE_VALUE1_GREATER == cmpval) {
            peer->state = MCA_OOB_TCP_CONNECTING;
            PRTE_ACTIVATE_TCP_CONN_STATE(peer, prte_oob_tcp_peer_try_connect);
        } else {
            peer->state = MCA_OOB_TCP_UNCONNECTED;
        }
        /* close the socket */
        CLOSE_THE_SOCKET(peer->sd);
        goto out;
    } else {
        pmix_output(0,
                    "%s prte_tcp_peer_try_connect: "
                    "tcp_peer_send_connect_ack to proc %s on %s:%d failed: %s (%d)",
                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                    pmix_net_get_hostname((struct sockaddr *) &addr->addr),
                    pmix_net_get_port((struct sockaddr *) &addr->addr), prte_strerror(rc), rc);
        /* close the socket */
        CLOSE_THE_SOCKET(peer->sd);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_COMM_FAILED);
    }

cleanup:
    PMIX_RELEASE(op);
out:
    if (NULL != results) {
        free(results);
    }
    if (NULL != remote_list) {
        PMIX_RELEASE(remote_list);
    }
}

/* send a handshake that includes our process identifier, our
 * version string, and a security token to ensure we are talking
 * to another OMPI process
 */
static int tcp_peer_send_connect_ack(prte_oob_tcp_peer_t *peer)
{
    char *msg;
    prte_oob_tcp_hdr_t hdr;
    uint16_t ack_flag = htons(1);
    size_t sdsize, offset = 0;

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s SEND CONNECT ACK", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    /* load the header */
    hdr.origin = *PRTE_PROC_MY_NAME;
    hdr.dst = peer->name;
    hdr.type = MCA_OOB_TCP_IDENT;
    hdr.tag = 0;
    hdr.seq_num = 0;
    memset(hdr.routed, 0, PRTE_MAX_RTD_SIZE + 1);

    /* payload size */
    sdsize = sizeof(ack_flag) + strlen(prte_version_string) + 1;
    hdr.nbytes = sdsize;
    MCA_OOB_TCP_HDR_HTON(&hdr);

    /* create a space for our message */
    sdsize += sizeof(hdr);
    if (NULL == (msg = (char *) malloc(sdsize))) {
        return PRTE_ERR_OUT_OF_RESOURCE;
    }
    memset(msg, 0, sdsize);

    /* load the message */
    memcpy(msg + offset, &hdr, sizeof(hdr));
    offset += sizeof(hdr);
    memcpy(msg + offset, &ack_flag, sizeof(ack_flag));
    offset += sizeof(ack_flag);
    memcpy(msg + offset, prte_version_string, strlen(prte_version_string) + 1);
    offset += strlen(prte_version_string) + 1;

    /* send it */
    if (PRTE_SUCCESS != tcp_peer_send_blocking(peer->sd, msg, sdsize)) {
        free(msg);
        peer->state = MCA_OOB_TCP_FAILED;
        prte_oob_tcp_peer_close(peer);
        return PRTE_ERR_UNREACH;
    }
    free(msg);

    return PRTE_SUCCESS;
}

/* send a handshake that includes our process identifier, our
 * version string, and a security token to ensure we are talking
 * to another OMPI process
 */
static int tcp_peer_send_connect_nack(int sd, pmix_proc_t *name)
{
    char *msg;
    prte_oob_tcp_hdr_t hdr;
    uint16_t ack_flag = htons(0);
    int rc = PRTE_SUCCESS;
    size_t sdsize, offset = 0;

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s SEND CONNECT NACK", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    /* load the header */
    hdr.origin = *PRTE_PROC_MY_NAME;
    hdr.dst = *name;
    hdr.type = MCA_OOB_TCP_IDENT;
    hdr.tag = 0;
    hdr.seq_num = 0;
    memset(hdr.routed, 0, PRTE_MAX_RTD_SIZE + 1);

    /* payload size */
    sdsize = sizeof(ack_flag);
    hdr.nbytes = sdsize;
    MCA_OOB_TCP_HDR_HTON(&hdr);

    /* create a space for our message */
    sdsize += sizeof(hdr);
    if (NULL == (msg = (char *) malloc(sdsize))) {
        return PRTE_ERR_OUT_OF_RESOURCE;
    }
    memset(msg, 0, sdsize);

    /* load the message */
    memcpy(msg + offset, &hdr, sizeof(hdr));
    offset += sizeof(hdr);
    memcpy(msg + offset, &ack_flag, sizeof(ack_flag));
    offset += sizeof(ack_flag);

    /* send it */
    if (PRTE_SUCCESS != tcp_peer_send_blocking(sd, msg, sdsize)) {
        /* it's ok if it fails - remote side may already
         * identifiet the collision and closed the connection
         */
        rc = PRTE_SUCCESS;
    }
    free(msg);
    return rc;
}

/*
 * Initialize events to be used by the peer instance for TCP select/poll callbacks.
 */
static void tcp_peer_event_init(prte_oob_tcp_peer_t *peer)
{
    if (peer->sd >= 0) {
        assert(!peer->send_ev_active && !peer->recv_ev_active);
        prte_event_set(prte_event_base, &peer->recv_event, peer->sd, PRTE_EV_READ | PRTE_EV_PERSIST,
                       prte_oob_tcp_recv_handler, peer);
        if (peer->recv_ev_active) {
            prte_event_del(&peer->recv_event);
            peer->recv_ev_active = false;
        }

        prte_event_set(prte_event_base, &peer->send_event, peer->sd,
                       PRTE_EV_WRITE | PRTE_EV_PERSIST, prte_oob_tcp_send_handler, peer);
        if (peer->send_ev_active) {
            prte_event_del(&peer->send_event);
            peer->send_ev_active = false;
        }
    }
}

/*
 * Check the status of the connection. If the connection failed, will retry
 * later. Otherwise, send this processes identifier to the peer on the
 * newly connected socket.
 */
void prte_oob_tcp_peer_complete_connect(prte_oob_tcp_peer_t *peer)
{
    int so_error = 0;
    prte_socklen_t so_length = sizeof(so_error);

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s:tcp:complete_connect called for peer %s on socket %d",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&peer->name), peer->sd);

    /* check connect completion status */
    if (getsockopt(peer->sd, SOL_SOCKET, SO_ERROR, (char *) &so_error, &so_length) < 0) {
        pmix_output(0, "%s tcp_peer_complete_connect: getsockopt() to %s failed: %s (%d)\n",
                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                    strerror(prte_socket_errno), prte_socket_errno);
        peer->state = MCA_OOB_TCP_FAILED;
        prte_oob_tcp_peer_close(peer);
        return;
    }

    if (so_error == EINPROGRESS) {
        pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                            "%s:tcp:send:handler still in progress",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        return;
    } else if (so_error == ECONNREFUSED || so_error == ETIMEDOUT) {
        pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                            "%s-%s tcp_peer_complete_connect: connection failed: %s (%d)",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                            strerror(so_error), so_error);
        prte_oob_tcp_peer_close(peer);
        return;
    } else if (so_error != 0) {
        /* No need to worry about the return code here - we return regardless
           at this point, and if an error did occur a message has already been
           printed for the user */
        pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                            "%s-%s tcp_peer_complete_connect: "
                            "connection failed with error %d",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                            so_error);
        prte_oob_tcp_peer_close(peer);
        return;
    }

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s tcp_peer_complete_connect: "
                        "sending ack to %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)));

    if (tcp_peer_send_connect_ack(peer) == PRTE_SUCCESS) {
        peer->state = MCA_OOB_TCP_CONNECT_ACK;
        pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                            "%s tcp_peer_complete_connect: "
                            "setting read event on connection to %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)));

        if (!peer->recv_ev_active) {
            peer->recv_ev_active = true;
            PMIX_POST_OBJECT(peer);
            prte_event_add(&peer->recv_event, 0);
        }
    } else {
        pmix_output(0, "%s tcp_peer_complete_connect: unable to send connect ack to %s",
                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)));
        peer->state = MCA_OOB_TCP_FAILED;
        prte_oob_tcp_peer_close(peer);
    }
}

/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the peers endpoint.
 */
static int tcp_peer_send_blocking(int sd, void *data, size_t size)
{
    unsigned char *ptr = (unsigned char *) data;
    size_t cnt = 0;
    int retval;

    PMIX_ACQUIRE_OBJECT(ptr);

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s send blocking of %" PRIsize_t " bytes to socket %d",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), size, sd);

    while (cnt < size) {
        retval = send(sd, (char *) ptr + cnt, size - cnt, 0);
        if (retval < 0) {
            if (prte_socket_errno != EINTR && prte_socket_errno != EAGAIN
                && prte_socket_errno != EWOULDBLOCK) {
                pmix_output(0, "%s tcp_peer_send_blocking: send() to socket %d failed: %s (%d)\n",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), sd, strerror(prte_socket_errno),
                            prte_socket_errno);
                return PRTE_ERR_UNREACH;
            }
            continue;
        }
        cnt += retval;
    }

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s blocking send complete to socket %d",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), sd);

    return PRTE_SUCCESS;
}

/*
 *  Receive the peers globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 */
static bool retry(prte_oob_tcp_peer_t *peer, int sd, bool fatal)
{
    int cmpval;

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s SIMUL CONNECTION WITH %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        PRTE_NAME_PRINT(&peer->name));
    cmpval = prte_util_compare_name_fields(PRTE_NS_CMP_ALL, &peer->name, PRTE_PROC_MY_NAME);
    if (fatal) {
        if (peer->send_ev_active) {
            prte_event_del(&peer->send_event);
            peer->send_ev_active = false;
        }
        if (peer->recv_ev_active) {
            prte_event_del(&peer->recv_event);
            peer->recv_ev_active = false;
        }
        if (0 <= peer->sd) {
            CLOSE_THE_SOCKET(peer->sd);
            peer->sd = -1;
        }
        if (PRTE_VALUE1_GREATER == cmpval) {
            /* force the other end to retry the connection */
            peer->state = MCA_OOB_TCP_UNCONNECTED;
        } else {
            /* retry the connection */
            peer->state = MCA_OOB_TCP_CONNECTING;
            PRTE_ACTIVATE_TCP_CONN_STATE(peer, prte_oob_tcp_peer_try_connect);
        }
        return true;
    } else {
        if (PRTE_VALUE1_GREATER == cmpval) {
            /* The other end will retry the connection */
            if (peer->send_ev_active) {
                prte_event_del(&peer->send_event);
                peer->send_ev_active = false;
            }
            if (peer->recv_ev_active) {
                prte_event_del(&peer->recv_event);
                peer->recv_ev_active = false;
            }
            CLOSE_THE_SOCKET(peer->sd);
            peer->state = MCA_OOB_TCP_UNCONNECTED;
            return false;
        } else {
            /* The connection will be retried */
            tcp_peer_send_connect_nack(sd, &peer->name);
            CLOSE_THE_SOCKET(sd);
            return true;
        }
    }
}

int prte_oob_tcp_peer_recv_connect_ack(prte_oob_tcp_peer_t *pr, int sd, prte_oob_tcp_hdr_t *dhdr)
{
    char *msg;
    char *version;
    size_t offset = 0, cnt;
    prte_oob_tcp_hdr_t hdr;
    prte_oob_tcp_peer_t *peer;
    uint16_t ack_flag;
    bool is_new = (NULL == pr);

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s RECV CONNECT ACK FROM %s ON SOCKET %d",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        (NULL == pr) ? "UNKNOWN" : PRTE_NAME_PRINT(&pr->name), sd);

    peer = pr;
    /* get the header */
    if (tcp_peer_recv_blocking(peer, sd, &hdr, sizeof(prte_oob_tcp_hdr_t))) {
        if (NULL != peer) {
            /* If the peer state is CONNECT_ACK, then we were waiting for
             * the connection to be ack'd
             */
            if (peer->state != MCA_OOB_TCP_CONNECT_ACK) {
                /* handshake broke down - abort this connection */
                pmix_output(0, "%s RECV CONNECT BAD HANDSHAKE (%d) FROM %s ON SOCKET %d",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), peer->state,
                            PRTE_NAME_PRINT(&(peer->name)), sd);
                prte_oob_tcp_peer_close(peer);
                return PRTE_ERR_UNREACH;
            }
        }
    } else {
        /* unable to complete the recv */
        pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                            "%s unable to complete recv of connect-ack from %s ON SOCKET %d",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                            (NULL == peer) ? "UNKNOWN" : PRTE_NAME_PRINT(&peer->name), sd);
        return PRTE_ERR_UNREACH;
    }

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s connect-ack recvd from %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        (NULL == peer) ? "UNKNOWN" : PRTE_NAME_PRINT(&peer->name));

    /* convert the header */
    MCA_OOB_TCP_HDR_NTOH(&hdr);
    /* if the requestor wanted the header returned, then do so now */
    if (NULL != dhdr) {
        *dhdr = hdr;
    }

    if (MCA_OOB_TCP_PROBE == hdr.type) {
        /* send a header back */
        hdr.type = MCA_OOB_TCP_PROBE;
        hdr.dst = hdr.origin;
        hdr.origin = *PRTE_PROC_MY_NAME;
        MCA_OOB_TCP_HDR_HTON(&hdr);
        tcp_peer_send_blocking(sd, &hdr, sizeof(prte_oob_tcp_hdr_t));
        CLOSE_THE_SOCKET(sd);
        return PRTE_SUCCESS;
    }

    if (hdr.type != MCA_OOB_TCP_IDENT) {
        pmix_output(0, "tcp_peer_recv_connect_ack: invalid header type: %d\n", hdr.type);
        if (NULL != peer) {
            peer->state = MCA_OOB_TCP_FAILED;
            prte_oob_tcp_peer_close(peer);
        } else {
            CLOSE_THE_SOCKET(sd);
        }
        return PRTE_ERR_COMM_FAILURE;
    }

    /* if we don't already have it, get the peer */
    if (NULL == peer) {
        peer = prte_oob_tcp_peer_lookup(&hdr.origin);
        if (NULL == peer) {
            pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                                "%s prte_oob_tcp_recv_connect: connection from new peer",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
            peer = PMIX_NEW(prte_oob_tcp_peer_t);
            PMIX_XFER_PROCID(&peer->name, &hdr.origin);
            peer->state = MCA_OOB_TCP_ACCEPTING;
            pmix_list_append(&prte_mca_oob_tcp_component.peers, &peer->super);
        }
    } else {
        /* compare the peers name to the expected value */
        if (!PMIX_CHECK_PROCID(&peer->name, &hdr.origin)) {
            pmix_output(0,
                        "%s tcp_peer_recv_connect_ack: "
                        "received unexpected process identifier %s from %s\n",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(hdr.origin)),
                        PRTE_NAME_PRINT(&(peer->name)));
            peer->state = MCA_OOB_TCP_FAILED;
            prte_oob_tcp_peer_close(peer);
            return PRTE_ERR_CONNECTION_REFUSED;
        }
    }

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s connect-ack header from %s is okay", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        PRTE_NAME_PRINT(&peer->name));

    /* get the authentication and version payload */
    if (NULL == (msg = (char *) malloc(hdr.nbytes))) {
        peer->state = MCA_OOB_TCP_FAILED;
        prte_oob_tcp_peer_close(peer);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }
    if (!tcp_peer_recv_blocking(peer, sd, msg, hdr.nbytes)) {
        /* unable to complete the recv but should never happen */
        pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                            "%s unable to complete recv of connect-ack from %s ON SOCKET %d",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&peer->name),
                            peer->sd);
        free(msg);
        return PRTE_ERR_UNREACH;
    }

    /* Check the type of acknowledgement */
    memcpy(&ack_flag, msg + offset, sizeof(ack_flag));
    offset += sizeof(ack_flag);

    ack_flag = ntohs(ack_flag);
    if (!ack_flag) {
        if (MCA_OOB_TCP_CONNECT_ACK == peer->state) {
            /* We got nack from the remote side which means that
             * it will be the initiator of the connection.
             */

            /* release the socket */
            CLOSE_THE_SOCKET(peer->sd);
            peer->sd = -1;

            /* unregister active events */
            if (peer->recv_ev_active) {
                prte_event_del(&peer->recv_event);
                peer->recv_ev_active = false;
            }
            if (peer->send_ev_active) {
                prte_event_del(&peer->send_event);
                peer->send_ev_active = false;
            }

            /* change the state so we'll accept the remote
             * connection when it'll apeear
             */
            peer->state = MCA_OOB_TCP_UNCONNECTED;
        } else {
            /* FIXME: this shouldn't happen. We need to force next address
             * to be tried.
             */
            prte_oob_tcp_peer_close(peer);
        }
        free(msg);
        return PRTE_ERR_UNREACH;
    }

    /* check for a race condition - if I was in the process of
     * creating a connection to the peer, or have already established
     * such a connection, then we need to reject this connection. We will
     * let the higher ranked process retry - if I'm the lower ranked
     * process, I'll simply defer until I receive the request
     */
    if (is_new
        && (MCA_OOB_TCP_CONNECTED == peer->state || MCA_OOB_TCP_CONNECTING == peer->state
            || MCA_OOB_TCP_CONNECT_ACK == peer->state)) {
        if (retry(peer, sd, false)) {
            free(msg);
            return PRTE_ERR_UNREACH;
        }
    }

    /* check that this is from a matching version */
    version = (char *) ((char *) msg + offset);
    cnt = 0;
    while ('\0' != version[cnt] && cnt < (hdr.nbytes - offset)) {
        ++cnt;
    }
    if (cnt == (hdr.nbytes - offset)) {
        version[cnt-1] = '\0';
        --cnt;
    }
    offset += cnt + 1;
    if (0 != strcmp(version, prte_version_string)) {
        pmix_show_help("help-oob-tcp.txt", "version mismatch", true, prte_process_info.nodename,
                       PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), prte_version_string,
                       pmix_fd_get_peer_name(peer->sd), PRTE_NAME_PRINT(&(peer->name)), version);

        peer->state = MCA_OOB_TCP_FAILED;
        prte_oob_tcp_peer_close(peer);
        free(msg);
        return PRTE_ERR_CONNECTION_REFUSED;
    }
    free(msg);

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s connect-ack version from %s matches ours",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&peer->name));

    /* if the requestor wanted the header returned, then they
     * will complete their processing
     */
    if (NULL != dhdr) {
        return PRTE_SUCCESS;
    }

    /* set the peer into the component and OOB-level peer tables to indicate
     * that we know this peer and we will be handling him
     */
    PRTE_ACTIVATE_TCP_CMP_OP(peer, prte_mca_oob_tcp_component_set_module);

    /* connected */
    tcp_peer_connected(peer);
    if (OOB_TCP_DEBUG_CONNECT
        <= pmix_output_get_verbosity(prte_oob_base_framework.framework_output)) {
        prte_oob_tcp_peer_dump(peer, "connected");
    }
    return PRTE_SUCCESS;
}

/*
 *  Setup peer state to reflect that connection has been established,
 *  and start any pending sends.
 */
static void tcp_peer_connected(prte_oob_tcp_peer_t *peer)
{
    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s-%s tcp_peer_connected on socket %d", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        PRTE_NAME_PRINT(&(peer->name)), peer->sd);

    if (peer->timer_ev_active) {
        prte_event_del(&peer->timer_event);
        peer->timer_ev_active = false;
    }
    peer->state = MCA_OOB_TCP_CONNECTED;
    if (NULL != peer->active_addr) {
        peer->active_addr->retries = 0;
    }

    /* initiate send of first message on queue */
    if (NULL == peer->send_msg) {
        peer->send_msg = (prte_oob_tcp_send_t *) pmix_list_remove_first(&peer->send_queue);
    }
    if (NULL != peer->send_msg && !peer->send_ev_active) {
        peer->send_ev_active = true;
        PMIX_POST_OBJECT(peer);
        prte_event_add(&peer->send_event, 0);
    }
}

/*
 * Remove any event registrations associated with the socket
 * and update the peer state to reflect the connection has
 * been closed.
 */
void prte_oob_tcp_peer_close(prte_oob_tcp_peer_t *peer)
{
    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s tcp_peer_close for %s sd %d state %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                        peer->sd, prte_oob_tcp_state_print(peer->state));

    /* release the socket */
    close(peer->sd);
    peer->sd = -1;

    /* if we were CONNECTING, then we need to mark the address as
     * failed and cycle back to try the next address */
    if (MCA_OOB_TCP_CONNECTING == peer->state) {
        if (NULL != peer->active_addr) {
            peer->active_addr->state = MCA_OOB_TCP_FAILED;
        }
        PRTE_ACTIVATE_TCP_CONN_STATE(peer, prte_oob_tcp_peer_try_connect);
        return;
    }

    peer->state = MCA_OOB_TCP_CLOSED;
    if (NULL != peer->active_addr) {
        peer->active_addr->state = MCA_OOB_TCP_CLOSED;
    }

    /* unregister active events */
    if (peer->recv_ev_active) {
        prte_event_del(&peer->recv_event);
        peer->recv_ev_active = false;
    }
    if (peer->send_ev_active) {
        prte_event_del(&peer->send_event);
        peer->send_ev_active = false;
    }

    /* inform the component-level that we have lost a connection so
     * it can decide what to do about it.
     */
    PRTE_ACTIVATE_TCP_CMP_OP(peer, prte_mca_oob_tcp_component_lost_connection);

    if (prte_prteds_term_ordered || prte_finalizing || prte_abnormal_term_ordered) {
        /* nothing more to do */
        return;
    }

    /* FIXME: push any queued messages back onto the OOB for retry - note that
     * this must be done after the prior call to ensure that the component
     * processes the "lost connection" notice before the OOB begins to
     * handle these recycled messages. This prevents us from unintentionally
     * attempting to send the message again across the now-failed interface
     */
    /*
    if (NULL != peer->send_msg) {
    }
    while (NULL != (snd = (prte_oob_tcp_send_t*)pmix_list_remove_first(&peer->send_queue))) {
    }
    */
}

/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the peers endpoint.
 */
static bool tcp_peer_recv_blocking(prte_oob_tcp_peer_t *peer, int sd, void *data, size_t size)
{
    unsigned char *ptr = (unsigned char *) data;
    size_t cnt = 0;

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s waiting for connect ack from %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        (NULL == peer) ? "UNKNOWN" : PRTE_NAME_PRINT(&(peer->name)));

    while (cnt < size) {
        int retval = recv(sd, (char *) ptr + cnt, size - cnt, 0);

        /* remote closed connection */
        if (retval == 0) {
            pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                                "%s-%s tcp_peer_recv_blocking: "
                                "peer closed connection: peer state %d",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                (NULL == peer) ? "UNKNOWN" : PRTE_NAME_PRINT(&(peer->name)),
                                (NULL == peer) ? 0 : peer->state);
            if (NULL != peer) {
                prte_oob_tcp_peer_close(peer);
            } else {
                CLOSE_THE_SOCKET(sd);
            }
            return false;
        }

        /* socket is non-blocking so handle errors */
        if (retval < 0) {
            if (prte_socket_errno != EINTR && prte_socket_errno != EAGAIN
                && prte_socket_errno != EWOULDBLOCK) {
                if (NULL == peer) {
                    /* protect against things like port scanners */
                    CLOSE_THE_SOCKET(sd);
                    return false;
                } else if (peer->state == MCA_OOB_TCP_CONNECT_ACK) {
                    /* If we overflow the listen backlog, it's
                       possible that even though we finished the three
                       way handshake, the remote host was unable to
                       transition the connection from half connected
                       (received the initial SYN) to fully connected
                       (in the listen backlog).  We likely won't see
                       the failure until we try to receive, due to
                       timing and the like.  The first thing we'll get
                       in that case is a RST packet, which receive
                       will turn into a connection reset by peer
                       errno.  In that case, leave the socket in
                       CONNECT_ACK and propogate the error up to
                       recv_connect_ack, who will try to establish the
                       connection again */
                    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT,
                                        prte_oob_base_framework.framework_output,
                                        "%s connect ack received error %s from %s",
                                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                        strerror(prte_socket_errno),
                                        PRTE_NAME_PRINT(&(peer->name)));
                    return false;
                } else {
                    pmix_output(0,
                                "%s tcp_peer_recv_blocking: "
                                "recv() failed for %s: %s (%d)\n",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                                strerror(prte_socket_errno), prte_socket_errno);
                    peer->state = MCA_OOB_TCP_FAILED;
                    prte_oob_tcp_peer_close(peer);
                    return false;
                }
            }
            continue;
        }
        cnt += retval;
    }

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s connect ack received from %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        (NULL == peer) ? "UNKNOWN" : PRTE_NAME_PRINT(&(peer->name)));
    return true;
}

/*
 * Routine for debugging to print the connection state and socket options
 */
void prte_oob_tcp_peer_dump(prte_oob_tcp_peer_t *peer, const char *msg)
{
    char src[64];
    char dst[64];
    char buff[255];
    int sndbuf, rcvbuf, nodelay, flags;
    struct sockaddr_storage inaddr;
    prte_socklen_t addrlen = sizeof(struct sockaddr_storage);
    prte_socklen_t optlen;

    if (getsockname(peer->sd, (struct sockaddr *) &inaddr, &addrlen) < 0) {
        pmix_output(0, "tcp_peer_dump: getsockname error: %s (%d)\n",
                    strerror(prte_socket_errno), prte_socket_errno);
        snprintf(src, sizeof(src), "%s", "unknown");
    } else {
        snprintf(src, sizeof(src), "%s", pmix_net_get_hostname((struct sockaddr *) &inaddr));
    }
    if (getpeername(peer->sd, (struct sockaddr *) &inaddr, &addrlen) < 0) {
        pmix_output(0, "tcp_peer_dump: getpeername error: %s (%d)\n",
                    strerror(prte_socket_errno), prte_socket_errno);
        snprintf(dst, sizeof(dst), "%s", "unknown");
    } else {
        snprintf(dst, sizeof(dst), "%s", pmix_net_get_hostname((struct sockaddr *) &inaddr));
    }

    if ((flags = fcntl(peer->sd, F_GETFL, 0)) < 0) {
        pmix_output(0, "tcp_peer_dump: fcntl(F_GETFL) failed: %s (%d)\n",
                    strerror(prte_socket_errno), prte_socket_errno);
    }

#if defined(SO_SNDBUF)
    optlen = sizeof(sndbuf);
    if (getsockopt(peer->sd, SOL_SOCKET, SO_SNDBUF, (char *) &sndbuf, &optlen) < 0) {
        pmix_output(0, "tcp_peer_dump: SO_SNDBUF option: %s (%d)\n", strerror(prte_socket_errno),
                    prte_socket_errno);
    }
#else
    sndbuf = -1;
#endif
#if defined(SO_RCVBUF)
    optlen = sizeof(rcvbuf);
    if (getsockopt(peer->sd, SOL_SOCKET, SO_RCVBUF, (char *) &rcvbuf, &optlen) < 0) {
        pmix_output(0, "tcp_peer_dump: SO_RCVBUF option: %s (%d)\n", strerror(prte_socket_errno),
                    prte_socket_errno);
    }
#else
    rcvbuf = -1;
#endif
#if defined(TCP_NODELAY)
    optlen = sizeof(nodelay);
    if (getsockopt(peer->sd, IPPROTO_TCP, TCP_NODELAY, (char *) &nodelay, &optlen) < 0) {
        pmix_output(0, "tcp_peer_dump: TCP_NODELAY option: %s (%d)\n", strerror(prte_socket_errno),
                    prte_socket_errno);
    }
#else
    nodelay = 0;
#endif

    snprintf(buff, sizeof(buff), "%s-%s %s: %s - %s nodelay %d sndbuf %d rcvbuf %d flags %08x\n",
             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)), msg, src, dst,
             nodelay, sndbuf, rcvbuf, flags);
    pmix_output(0, "%s", buff);
}

/*
 * Accept incoming connection - if not already connected
 */

bool prte_oob_tcp_peer_accept(prte_oob_tcp_peer_t *peer)
{
    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s tcp:peer_accept called for peer %s in state %s on socket %d",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&peer->name),
                        prte_oob_tcp_state_print(peer->state), peer->sd);

    if (peer->state != MCA_OOB_TCP_CONNECTED) {

        tcp_peer_event_init(peer);

        if (tcp_peer_send_connect_ack(peer) != PRTE_SUCCESS) {
            pmix_output(0,
                        "%s-%s tcp_peer_accept: "
                        "tcp_peer_send_connect_ack failed\n",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)));
            peer->state = MCA_OOB_TCP_FAILED;
            prte_oob_tcp_peer_close(peer);
            return false;
        }

        /* set the peer into the component and OOB-level peer tables to indicate
         * that we know this peer and we will be handling him
         */
        PRTE_ACTIVATE_TCP_CMP_OP(peer, prte_mca_oob_tcp_component_set_module);

        tcp_peer_connected(peer);
        if (!peer->recv_ev_active) {
            peer->recv_ev_active = true;
            PMIX_POST_OBJECT(peer);
            prte_event_add(&peer->recv_event, 0);
        }
        if (OOB_TCP_DEBUG_CONNECT
            <= pmix_output_get_verbosity(prte_oob_base_framework.framework_output)) {
            prte_oob_tcp_peer_dump(peer, "accepted");
        }
        return true;
    }

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s tcp:peer_accept ignored for peer %s in state %s on socket %d",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&peer->name),
                        prte_oob_tcp_state_print(peer->state), peer->sd);
    return false;
}
