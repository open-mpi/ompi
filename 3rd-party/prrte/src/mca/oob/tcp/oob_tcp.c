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
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"
#include "types.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#include <fcntl.h>
#ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif
#include <ctype.h>

#include "src/include/prte_socket_errno.h"
#include "src/runtime/prte_progress_threads.h"
#include "src/util/pmix_argv.h"
#include "src/util/error.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_parse_options.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/oob/tcp/oob_tcp.h"
#include "src/mca/oob/tcp/oob_tcp_common.h"
#include "src/mca/oob/tcp/oob_tcp_component.h"
#include "src/mca/oob/tcp/oob_tcp_connection.h"
#include "src/mca/oob/tcp/oob_tcp_peer.h"
#include "src/mca/oob/tcp/oob_tcp_sendrecv.h"

static void accept_connection(const int accepted_fd, const struct sockaddr *addr);
static void ping(const pmix_proc_t *proc);
static void send_nb(prte_rml_send_t *msg);

prte_oob_tcp_module_t prte_oob_tcp_module = {.accept_connection = accept_connection,
                                             .ping = ping,
                                             .send_nb = send_nb};

/*
 * Local utility functions
 */
static void recv_handler(int sd, short flags, void *user);

/* Called by prte_oob_tcp_accept() and connection_handler() on
 * a socket that has been accepted.  This call finishes processing the
 * socket, including setting socket options and registering for the
 * OOB-level connection handshake.  Used in both the threaded and
 * event listen modes.
 */
static void accept_connection(const int accepted_fd, const struct sockaddr *addr)
{
    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s accept_connection: %s:%d\n", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        pmix_net_get_hostname(addr), pmix_net_get_port(addr));

    /* setup socket options */
    prte_oob_tcp_set_socket_options(accepted_fd);

    /* use a one-time event to wait for receipt of peer's
     *  process ident message to complete this connection
     */
    PRTE_ACTIVATE_TCP_ACCEPT_STATE(accepted_fd, addr, recv_handler);
}

/* API functions */
static void ping(const pmix_proc_t *proc)
{
    prte_oob_tcp_peer_t *peer;

    pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                        "%s:[%s:%d] processing ping to peer %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        __FILE__, __LINE__, PRTE_NAME_PRINT(proc));

    /* do we know this peer? */
    if (NULL == (peer = prte_oob_tcp_peer_lookup(proc))) {
        /* push this back to the component so it can try
         * another module within this transport. If no
         * module can be found, the component can push back
         * to the framework so another component can try
         */
        pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                            "%s:[%s:%d] hop %s unknown", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                            __FILE__, __LINE__, PRTE_NAME_PRINT(proc));
        PRTE_ACTIVATE_TCP_MSG_ERROR(NULL, NULL, proc, prte_mca_oob_tcp_component_hop_unknown);
        return;
    }

    /* if we are already connected, there is nothing to do */
    if (MCA_OOB_TCP_CONNECTED == peer->state) {
        pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                            "%s:[%s:%d] already connected to peer %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__, __LINE__,
                            PRTE_NAME_PRINT(proc));
        return;
    }

    /* if we are already connecting, there is nothing to do */
    if (MCA_OOB_TCP_CONNECTING == peer->state || MCA_OOB_TCP_CONNECT_ACK == peer->state) {
        pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                            "%s:[%s:%d] already connecting to peer %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__, __LINE__,
                            PRTE_NAME_PRINT(proc));
        return;
    }

    /* attempt the connection */
    peer->state = MCA_OOB_TCP_CONNECTING;
    PRTE_ACTIVATE_TCP_CONN_STATE(peer, prte_oob_tcp_peer_try_connect);
}

static void send_nb(prte_rml_send_t *msg)
{
    prte_oob_tcp_peer_t *peer;
    pmix_proc_t hop;

    /* do we have a route to this peer (could be direct)? */
    PMIX_LOAD_NSPACE(hop.nspace, PRTE_PROC_MY_NAME->nspace);
    hop.rank = prte_rml_get_route(msg->dst.rank);
    /* do we know this hop? */
    if (NULL == (peer = prte_oob_tcp_peer_lookup(&hop))) {
        /* if this message is going to the HNP, send it direct */
        if (PRTE_PROC_MY_HNP->rank == msg->dst.rank) {
            hop.rank = PRTE_PROC_MY_HNP->rank;
            peer = prte_oob_tcp_peer_lookup(&hop);
            if (NULL != peer) {
                goto send;
            }
        }
        /* push this back to the component so it can try
         * another module within this transport. If no
         * module can be found, the component can push back
         * to the framework so another component can try
         */
        pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                            "%s:[%s:%d] processing send to peer %s:%d seq_num = %d hop %s unknown",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__, __LINE__,
                            PRTE_NAME_PRINT(&msg->dst), msg->tag, msg->seq_num,
                            PRTE_NAME_PRINT(&hop));
        PRTE_ACTIVATE_TCP_NO_ROUTE(msg, &hop, prte_mca_oob_tcp_component_no_route);
        return;
    }

send:
    pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                        "%s:[%s:%d] processing send to peer %s:%d seq_num = %d via %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__, __LINE__,
                        PRTE_NAME_PRINT(&msg->dst), msg->tag, msg->seq_num,
                        PRTE_NAME_PRINT(&peer->name));

    /* add the msg to the hop's send queue */
    if (MCA_OOB_TCP_CONNECTED == peer->state) {
        pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                            "%s tcp:send_nb: already connected to %s - queueing for send",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&peer->name));
        MCA_OOB_TCP_QUEUE_SEND(msg, peer);
        return;
    }

    /* add the message to the queue for sending after the
     * connection is formed
     */
    MCA_OOB_TCP_QUEUE_PENDING(msg, peer);

    if (MCA_OOB_TCP_CONNECTING != peer->state && MCA_OOB_TCP_CONNECT_ACK != peer->state) {
        /* we have to initiate the connection - again, we do not
         * want to block while the connection is created.
         * So throw us into an event that will create
         * the connection via a mini-state-machine :-)
         */
        pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                            "%s tcp:send_nb: initiating connection to %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&peer->name));
        peer->state = MCA_OOB_TCP_CONNECTING;
        PRTE_ACTIVATE_TCP_CONN_STATE(peer, prte_oob_tcp_peer_try_connect);
    }
}

/*
 * Event callback when there is data available on the registered
 * socket to recv.  This is called for the listen sockets to accept an
 * incoming connection, on new sockets trying to complete the software
 * connection process, and for probes.  Data on an established
 * connection is handled elsewhere.
 */
static void recv_handler(int sd, short flg, void *cbdata)
{
    prte_oob_tcp_conn_op_t *op = (prte_oob_tcp_conn_op_t *) cbdata;
    int flags;
    prte_oob_tcp_hdr_t hdr;
    prte_oob_tcp_peer_t *peer;
    PRTE_HIDE_UNUSED_PARAMS(flg);

    PMIX_ACQUIRE_OBJECT(op);

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s:tcp:recv:handler called", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    /* get the handshake */
    if (PRTE_SUCCESS != prte_oob_tcp_peer_recv_connect_ack(NULL, sd, &hdr)) {
        goto cleanup;
    }

    /* finish processing ident */
    if (MCA_OOB_TCP_IDENT == hdr.type) {
        if (NULL == (peer = prte_oob_tcp_peer_lookup(&hdr.origin))) {
            /* should never happen */
            prte_oob_tcp_peer_close(peer);
            goto cleanup;
        }
        /* set socket up to be non-blocking */
        if ((flags = fcntl(sd, F_GETFL, 0)) < 0) {
            pmix_output(0, "%s prte_oob_tcp_recv_connect: fcntl(F_GETFL) failed: %s (%d)",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), strerror(prte_socket_errno),
                        prte_socket_errno);
        } else {
            flags |= O_NONBLOCK;
            if (fcntl(sd, F_SETFL, flags) < 0) {
                pmix_output(0, "%s prte_oob_tcp_recv_connect: fcntl(F_SETFL) failed: %s (%d)",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), strerror(prte_socket_errno),
                            prte_socket_errno);
            }
        }
        /* is the peer instance willing to accept this connection */
        peer->sd = sd;
        if (prte_oob_tcp_peer_accept(peer) == false) {
            if (OOB_TCP_DEBUG_CONNECT
                <= pmix_output_get_verbosity(prte_oob_base_framework.framework_output)) {
                pmix_output(0,
                            "%s-%s prte_oob_tcp_recv_connect: "
                            "rejected connection from %s connection state %d",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&(peer->name)),
                            PRTE_NAME_PRINT(&(hdr.origin)), peer->state);
            }
            CLOSE_THE_SOCKET(sd);
        }
    }

cleanup:
    PMIX_RELEASE(op);
}
