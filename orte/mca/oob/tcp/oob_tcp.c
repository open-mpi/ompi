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
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <fcntl.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#include <ctype.h>

#include "opal/util/show_help.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/opal_socket_errno.h"
#include "opal/util/if.h"
#include "opal/util/net.h"
#include "opal/util/argv.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/sec/sec.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/util/parse_options.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/oob/tcp/oob_tcp.h"
#include "orte/mca/oob/tcp/oob_tcp_component.h"
#include "orte/mca/oob/tcp/oob_tcp_peer.h"
#include "orte/mca/oob/tcp/oob_tcp_common.h"
#include "orte/mca/oob/tcp/oob_tcp_connection.h"
#include "orte/mca/oob/tcp/oob_tcp_ping.h"

static void tcp_init(void);
static void tcp_fini(void);
static void accept_connection(const int accepted_fd,
                              const struct sockaddr *addr);
static void set_peer(const orte_process_name_t* name,
                     const uint16_t af_family,
                     const char *net, const char *ports);
static void ping(const orte_process_name_t *proc);
static void send_nb(orte_rml_send_t *msg);
static void resend(struct mca_oob_tcp_msg_error_t *mop);
static void ft_event(int state);

mca_oob_tcp_module_t mca_oob_tcp_module = {
    {
        tcp_init,
        tcp_fini,
        accept_connection,
        set_peer,
        ping,
        send_nb,
        resend,
        ft_event
    }
};

/*
 * Local utility functions
 */
static void recv_handler(int sd, short flags, void* user);
static void* progress_thread_engine(opal_object_t *obj)
{
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s TCP OOB PROGRESS THREAD RUNNING",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    while (mca_oob_tcp_module.ev_active) {
        opal_event_loop(mca_oob_tcp_module.ev_base, OPAL_EVLOOP_ONCE);
    }
    return OPAL_THREAD_CANCELLED;
}


/*
 * Initialize global variables used w/in this module.
 */
static void tcp_init(void)
{
    /* setup the module's state variables */
    OBJ_CONSTRUCT(&mca_oob_tcp_module.peers, opal_hash_table_t);
    opal_hash_table_init(&mca_oob_tcp_module.peers, 32);
    mca_oob_tcp_module.ev_active = false;

    if (orte_oob_base.use_module_threads) {
        /* if we are to use independent progress threads at
         * the module level, start it now
         */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s STARTING TCP PROGRESS THREAD",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        mca_oob_tcp_module.ev_base = opal_event_base_create();
        /* construct the thread object */
        OBJ_CONSTRUCT(&mca_oob_tcp_module.progress_thread, opal_thread_t);
        /* fork off a thread to progress it */
        mca_oob_tcp_module.progress_thread.t_run = progress_thread_engine;
        mca_oob_tcp_module.ev_active = true;
        if (OPAL_SUCCESS != opal_thread_start(&mca_oob_tcp_module.progress_thread)) {
            opal_output(0, "%s progress thread failed to start",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        }
    }
}

/*
 * Module cleanup.
 */
static void tcp_fini(void)
{
    uint64_t ui64;
    char *nptr;
    mca_oob_tcp_peer_t *peer;

    /* cleanup all peers */
    if (OPAL_SUCCESS == opal_hash_table_get_first_key_uint64(&mca_oob_tcp_module.peers, &ui64,
                                                             (void**)&peer, (void**)&nptr)) {
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s RELEASING PEER OBJ %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (NULL == peer) ? "NULL" : ORTE_NAME_PRINT(&peer->name));
        if (NULL != peer) {
            OBJ_RELEASE(peer);
        }
        while (OPAL_SUCCESS == opal_hash_table_get_next_key_uint64(&mca_oob_tcp_module.peers, &ui64,
                                                                   (void**)&peer, nptr, (void**)&nptr)) {
            opal_output_verbose(2, orte_oob_base_framework.framework_output,
                                "%s RELEASING PEER OBJ %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (NULL == peer) ? "NULL" : ORTE_NAME_PRINT(&peer->name));
            if (NULL != peer) {
                OBJ_RELEASE(peer);
            }
        }
    }
    OBJ_DESTRUCT(&mca_oob_tcp_module.peers);

    if (mca_oob_tcp_module.ev_active) {
        /* if we used an independent progress thread at
         * the module level, stop it now
         */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s STOPPING TCP PROGRESS THREAD",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* stop the progress thread */
        mca_oob_tcp_module.ev_active = false;
        /* break the event loop */
        opal_event_base_loopexit(mca_oob_tcp_module.ev_base);
        /* wait for thread to exit */
        opal_thread_join(&mca_oob_tcp_module.progress_thread, NULL);
        OBJ_DESTRUCT(&mca_oob_tcp_module.progress_thread);
        /* release the event base */
        opal_event_base_free(mca_oob_tcp_module.ev_base);
    }
}

/* Called by mca_oob_tcp_accept() and connection_handler() on
 * a socket that has been accepted.  This call finishes processing the
 * socket, including setting socket options and registering for the
 * OOB-level connection handshake.  Used in both the threaded and
 * event listen modes.
 */
static void accept_connection(const int accepted_fd,
                              const struct sockaddr *addr)
{
    opal_output_verbose(OOB_TCP_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s accept_connection: %s:%d\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        opal_net_get_hostname(addr),
                        opal_net_get_port(addr));

   /* setup socket options */
    orte_oob_tcp_set_socket_options(accepted_fd);

    /* use a one-time event to wait for receipt of peer's
     *  process ident message to complete this connection
     */
    ORTE_ACTIVATE_TCP_ACCEPT_STATE(accepted_fd, addr, recv_handler);
}

static int parse_uri(const uint16_t af_family,
                     const char* host,
                     const char *port,
                     struct sockaddr* inaddr)
{
    struct sockaddr_in *in;
#if OPAL_ENABLE_IPV6
    struct addrinfo hints, *res;
    int ret;
#endif

    if (AF_INET == af_family) {
        memset(inaddr, 0, sizeof(struct sockaddr_in));
        in = (struct sockaddr_in*) inaddr;
        in->sin_family = AF_INET;
        in->sin_addr.s_addr = inet_addr(host);
        if (in->sin_addr.s_addr == INADDR_NONE) {
            return ORTE_ERR_BAD_PARAM;
        }
        ((struct sockaddr_in*) inaddr)->sin_port = htons(atoi(port));
    }
#if OPAL_ENABLE_IPV6
    else if (AF_INET6 == af_family) {
        size_t len;
        memset(inaddr, 0, sizeof(struct sockaddr_in6));
        memset(&hints, 0, sizeof(hints));
        hints.ai_family = af_family;
        hints.ai_socktype = SOCK_STREAM;
        ret = getaddrinfo(host, NULL, &hints, &res);
        
        if (ret) {
            opal_output (0, "oob_tcp_parse_uri: Could not resolve %s. [Error: %s]\n",
                         host, gai_strerror (ret));
            return ORTE_ERR_BAD_PARAM;
        }
        len = (res->ai_addrlen < sizeof(struct sockaddr_in6)) ? res->ai_addrlen : sizeof(struct sockaddr_in6);
        memcpy(inaddr, res->ai_addr, len);
        freeaddrinfo(res);
    }
#endif
    else {
        return ORTE_ERR_NOT_SUPPORTED;
    }
        

    return ORTE_SUCCESS;
}

/*
 * Record listening address for this peer - the connection
 * is created on first-send
 */
static void process_set_peer(int fd, short args, void *cbdata)
{
    mca_oob_tcp_peer_op_t *pop = (mca_oob_tcp_peer_op_t*)cbdata;
    struct sockaddr inaddr;
    mca_oob_tcp_peer_t *peer;
    int rc=ORTE_SUCCESS;
    uint64_t *ui64 = (uint64_t*)(&pop->peer);
    mca_oob_tcp_addr_t *maddr;

    opal_output_verbose(OOB_TCP_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s:tcp:processing set_peer cmd",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    if (AF_INET != pop->af_family) {
            opal_output_verbose(20, orte_oob_base_framework.framework_output,
	                        "%s NOT AF_INET", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        goto cleanup;
    }

    if (NULL == (peer = mca_oob_tcp_peer_lookup(&pop->peer))) {
        peer = OBJ_NEW(mca_oob_tcp_peer_t);
        peer->name.jobid = pop->peer.jobid;
        peer->name.vpid = pop->peer.vpid;
        opal_output_verbose(20, orte_oob_base_framework.framework_output,
                            "%s SET_PEER ADDING PEER %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&pop->peer));
        if (OPAL_SUCCESS != opal_hash_table_set_value_uint64(&mca_oob_tcp_module.peers, (*ui64), peer)) {
            OBJ_RELEASE(peer);
            return;
        }
    }

    if ((rc = parse_uri(pop->af_family, pop->net, pop->port, (struct sockaddr*) &inaddr)) != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    opal_output_verbose(20, orte_oob_base_framework.framework_output,
                        "%s set_peer: peer %s is listening on net %s port %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&pop->peer),
                        (NULL == pop->net) ? "NULL" : pop->net,
                        (NULL == pop->port) ? "NULL" : pop->port);
    maddr = OBJ_NEW(mca_oob_tcp_addr_t);
    memcpy(&maddr->addr, &inaddr, sizeof(inaddr));
    opal_list_append(&peer->addrs, &maddr->super);

 cleanup:
    OBJ_RELEASE(pop);
}

static void set_peer(const orte_process_name_t *name,
                     const uint16_t af_family,
                     const char *net, const char *ports)
{
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s:tcp set addr for peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(name));

    /* have to push this into our event base for processing */
    ORTE_ACTIVATE_TCP_PEER_OP(name, af_family, net, ports, process_set_peer);
}


/* API functions */
static void process_ping(int fd, short args, void *cbdata)
{
    mca_oob_tcp_ping_t *op = (mca_oob_tcp_ping_t*)cbdata;
    mca_oob_tcp_peer_t *peer;

    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s:[%s:%d] processing ping to peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        __FILE__, __LINE__,
                        ORTE_NAME_PRINT(&op->peer));

    /* do we know this peer? */
    if (NULL == (peer = mca_oob_tcp_peer_lookup(&op->peer))) {
        /* push this back to the component so it can try
         * another module within this transport. If no
         * module can be found, the component can push back
         * to the framework so another component can try
         */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s:[%s:%d] hop %s unknown",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            __FILE__, __LINE__,
                            ORTE_NAME_PRINT(&op->peer));
        ORTE_ACTIVATE_TCP_MSG_ERROR(NULL, NULL, &op->peer, mca_oob_tcp_component_hop_unknown);
        goto cleanup;
    }

    /* if we are already connected, there is nothing to do */
    if (MCA_OOB_TCP_CONNECTED == peer->state) {
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s:[%s:%d] already connected to peer %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            __FILE__, __LINE__,
                            ORTE_NAME_PRINT(&op->peer));
        goto cleanup;
    }

    /* if we are already connecting, there is nothing to do */
    if (MCA_OOB_TCP_CONNECTING == peer->state &&
        MCA_OOB_TCP_CONNECT_ACK == peer->state) {
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s:[%s:%d] already connecting to peer %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            __FILE__, __LINE__,
                            ORTE_NAME_PRINT(&op->peer));
        goto cleanup;
    }

    /* attempt the connection */
    peer->state = MCA_OOB_TCP_CONNECTING;
    ORTE_ACTIVATE_TCP_CONN_STATE(peer, mca_oob_tcp_peer_try_connect);

 cleanup:
    OBJ_RELEASE(op);
}

static void ping(const orte_process_name_t *proc)
{
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s:[%s:%d] pinging peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        __FILE__, __LINE__,
                        ORTE_NAME_PRINT(proc));

    /* push this into our event base for processing */
    ORTE_ACTIVATE_TCP_PING(proc, process_ping);
}

static void process_send(int fd, short args, void *cbdata)
{
    mca_oob_tcp_msg_op_t *op = (mca_oob_tcp_msg_op_t*)cbdata;
    mca_oob_tcp_peer_t *peer;
    orte_process_name_t hop;

    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s:[%s:%d] processing send to peer %s:%d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        __FILE__, __LINE__,
                        ORTE_NAME_PRINT(&op->msg->dst), op->msg->tag);

    /* do we have a route to this peer (could be direct)? */
    hop = orte_routed.get_route(&op->msg->dst);
    /* do we know this hop? */
    if (NULL == (peer = mca_oob_tcp_peer_lookup(&hop))) {
        /* push this back to the component so it can try
         * another module within this transport. If no
         * module can be found, the component can push back
         * to the framework so another component can try
         */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s:[%s:%d] hop %s unknown",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            __FILE__, __LINE__,
                            ORTE_NAME_PRINT(&hop));
        ORTE_ACTIVATE_TCP_NO_ROUTE(op->msg, &hop, mca_oob_tcp_component_no_route);
        goto cleanup;
    }

    /* add the msg to the hop's send queue */
    if (MCA_OOB_TCP_CONNECTED == peer->state) {
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s tcp:send_nb: already connected to %s - queueing for send",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer->name));
        MCA_OOB_TCP_QUEUE_SEND(op->msg, peer);
        goto cleanup;
    }

    /* add the message to the queue for sending after the
     * connection is formed
     */
    MCA_OOB_TCP_QUEUE_PENDING(op->msg, peer);

    if (MCA_OOB_TCP_CONNECTING != peer->state &&
        MCA_OOB_TCP_CONNECT_ACK != peer->state) {
        /* we have to initiate the connection - again, we do not
         * want to block while the connection is created.
         * So throw us into an event that will create
         * the connection via a mini-state-machine :-)
         */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s tcp:send_nb: initiating connection to %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer->name));
        peer->state = MCA_OOB_TCP_CONNECTING;
        ORTE_ACTIVATE_TCP_CONN_STATE(peer, mca_oob_tcp_peer_try_connect);
    }

 cleanup:
    OBJ_RELEASE(op);
}

static void send_nb(orte_rml_send_t *msg)
{
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s tcp:send_nb to peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&msg->dst));

    /* push this into our event base for processing */
    ORTE_ACTIVATE_TCP_POST_SEND(msg, process_send);
}

static void process_resend(int fd, short args, void *cbdata)
{
    mca_oob_tcp_msg_error_t *op = (mca_oob_tcp_msg_error_t*)cbdata;
    mca_oob_tcp_peer_t *peer;

    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s:tcp processing resend to peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&op->hop));

    /* do we know this peer? */
    if (NULL == (peer = mca_oob_tcp_peer_lookup(&op->hop))) {
        /* push this back to the component so it can try
         * another module within this transport. If no
         * module can be found, the component can push back
         * to the framework so another component can try
         */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s:[%s:%d] peer %s unknown",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            __FILE__, __LINE__,
                            ORTE_NAME_PRINT(&op->hop));
        ORTE_ACTIVATE_TCP_MSG_ERROR(op->snd, NULL, &op->hop, mca_oob_tcp_component_hop_unknown);
        goto cleanup;
    }

    /* add the msg to this peer's send queue */
    if (MCA_OOB_TCP_CONNECTED == peer->state) {
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s tcp:resend: already connected to %s - queueing for send",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer->name));
        MCA_OOB_TCP_QUEUE_MSG(peer, op->snd, true);
        goto cleanup;
    }

    if (MCA_OOB_TCP_CONNECTING != peer->state &&
        MCA_OOB_TCP_CONNECT_ACK != peer->state) {
        /* add the message to the queue for sending after the
         * connection is formed
         */
        MCA_OOB_TCP_QUEUE_MSG(peer, op->snd, false);
        /* we have to initiate the connection - again, we do not
         * want to block while the connection is created.
         * So throw us into an event that will create
         * the connection via a mini-state-machine :-)
         */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s tcp:send_nb: initiating connection to %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer->name));
        peer->state = MCA_OOB_TCP_CONNECTING;
        ORTE_ACTIVATE_TCP_CONN_STATE(peer, mca_oob_tcp_peer_try_connect);
    }

 cleanup:
    OBJ_RELEASE(op);
}

static void resend(struct mca_oob_tcp_msg_error_t *mp)
{
    mca_oob_tcp_msg_error_t *mop = (mca_oob_tcp_msg_error_t*)mp;

    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s tcp:resend to peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&mop->hop));

    /* push this into our event base for processing */
    ORTE_ACTIVATE_TCP_POST_RESEND(mop, process_resend);
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
    mca_oob_tcp_conn_op_t *op = (mca_oob_tcp_conn_op_t*)cbdata;
    int flags;
    uint64_t *ui64;
    mca_oob_tcp_hdr_t hdr;
    mca_oob_tcp_peer_t *peer;

    opal_output_verbose(OOB_TCP_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s:tcp:recv:handler called",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* get the handshake */
    if (ORTE_SUCCESS != mca_oob_tcp_peer_recv_connect_ack(NULL, sd, &hdr)) {
        goto cleanup;
    }

    /* finish processing ident */
    if (MCA_OOB_TCP_IDENT == hdr.type) {
        if (NULL == (peer = mca_oob_tcp_peer_lookup(&hdr.origin))) {
            /* should never happen */
            mca_oob_tcp_peer_close(peer);
            goto cleanup;
        }
        /* set socket up to be non-blocking */
        if ((flags = fcntl(sd, F_GETFL, 0)) < 0) {
            opal_output(0, "%s mca_oob_tcp_recv_connect: fcntl(F_GETFL) failed: %s (%d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), strerror(opal_socket_errno), opal_socket_errno);
        } else {
            flags |= O_NONBLOCK;
            if (fcntl(sd, F_SETFL, flags) < 0) {
                opal_output(0, "%s mca_oob_tcp_recv_connect: fcntl(F_SETFL) failed: %s (%d)",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), strerror(opal_socket_errno), opal_socket_errno);
            }
        }
        
        /* is the peer instance willing to accept this connection */
        peer->sd = sd;
        if (mca_oob_tcp_peer_accept(peer) == false) {
            if (OOB_TCP_DEBUG_CONNECT <= opal_output_get_verbosity(orte_oob_base_framework.framework_output)) {
                opal_output(0, "%s-%s mca_oob_tcp_recv_connect: "
                            "rejected connection from %s connection state %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)),
                            ORTE_NAME_PRINT(&(hdr.origin)),
                            peer->state);
            }
            CLOSE_THE_SOCKET(sd);
            ui64 = (uint64_t*)(&peer->name);
            opal_hash_table_set_value_uint64(&mca_oob_tcp_module.peers, (*ui64), NULL);
            OBJ_RELEASE(peer);
        }
    }

 cleanup:
    OBJ_RELEASE(op);
}

/* Dummy function for when we are not using FT. */
#if OPAL_ENABLE_FT_CR == 0
static void ft_event(int state)
{
    return;
}

#else
static void ft_event(int state) {
    opal_list_item_t *item;

    if(OPAL_CRS_CHECKPOINT == state) {
#if 0
        /*
         * Disable event processing while we are working
         */
        opal_event_disable();
#endif
    }
    else if(OPAL_CRS_CONTINUE == state) {
#if 0
        /*
         * Resume event processing
         */
        opal_event_enable();
    }
    else if(OPAL_CRS_RESTART == state) {
        /*
         * Clean out cached connection information
         * Select pieces of finalize/init
         */
        for (item = opal_list_remove_first(&mca_oob_tcp_module.peer_list);
            item != NULL;
            item = opal_list_remove_first(&mca_oob_tcp_module.peer_list)) {
            mca_oob_tcp_peer_t* peer = (mca_oob_tcp_peer_t*)item;
            /* JJH: Use the below command for debugging restarts with invalid sockets
             * mca_oob_tcp_peer_dump(peer, "RESTART CLEAN")
             */
            MCA_OOB_TCP_PEER_RETURN(peer);
        }

        OBJ_DESTRUCT(&mca_oob_tcp_module.peer_free);
        OBJ_DESTRUCT(&mca_oob_tcp_module.peer_names);
        OBJ_DESTRUCT(&mca_oob_tcp_module.peers);
        OBJ_DESTRUCT(&mca_oob_tcp_module.peer_list);

        OBJ_CONSTRUCT(&mca_oob_tcp_module.peer_list,     opal_list_t);
        OBJ_CONSTRUCT(&mca_oob_tcp_module.peers,         opal_hash_table_t);
        OBJ_CONSTRUCT(&mca_oob_tcp_module.peer_names,    opal_hash_table_t);
        OBJ_CONSTRUCT(&mca_oob_tcp_module.peer_free,     opal_free_list_t);

        /*
         * Resume event processing
         */
        opal_event_enable();
#endif
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return;
}
#endif
