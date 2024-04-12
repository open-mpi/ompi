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
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * In windows, many of the socket functions return an EWOULDBLOCK
 * instead of things like EAGAIN, EINPROGRESS, etc. It has been
 * verified that this will not conflict with other error codes that
 * are returned by these functions under UNIX/Linux environments
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
#ifdef HAVE_NETINET_TCP_H
#    include <netinet/tcp.h>
#endif
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif
#include <ctype.h>

#include "src/include/prte_socket_errno.h"
#include "src/mca/prtebacktrace/prtebacktrace.h"
#include "src/util/error.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_output.h"

#include "oob_tcp_common.h"
#include "oob_tcp_peer.h"
#include "src/mca/oob/tcp/oob_tcp.h"
#include "src/mca/oob/tcp/oob_tcp_component.h"

/**
 * Set socket buffering
 */
static void set_keepalive(int sd)
{
#if defined(SO_KEEPALIVE)
    int option;
    socklen_t optlen;

    /* see if the keepalive option is available */
    optlen = sizeof(option);
    if (getsockopt(sd, SOL_SOCKET, SO_KEEPALIVE, &option, &optlen) < 0) {
        /* not available, so just return */
        return;
    }

    /* Set the option active */
    option = 1;
    if (setsockopt(sd, SOL_SOCKET, SO_KEEPALIVE, &option, optlen) < 0) {
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "[%s:%d] setsockopt(SO_KEEPALIVE) failed: %s (%d)", __FILE__, __LINE__,
                            strerror(prte_socket_errno), prte_socket_errno);
        return;
    }
#    if defined(TCP_KEEPALIVE)
    /* set the idle time */
    if (setsockopt(sd, IPPROTO_TCP, TCP_KEEPALIVE, &prte_mca_oob_tcp_component.keepalive_time,
                   sizeof(prte_mca_oob_tcp_component.keepalive_time))
        < 0) {
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "[%s:%d] setsockopt(TCP_KEEPALIVE) failed: %s (%d)", __FILE__, __LINE__,
                            strerror(prte_socket_errno), prte_socket_errno);
        return;
    }
#    elif defined(TCP_KEEPIDLE)
    /* set the idle time */
    if (setsockopt(sd, IPPROTO_TCP, TCP_KEEPIDLE, &prte_mca_oob_tcp_component.keepalive_time,
                   sizeof(prte_mca_oob_tcp_component.keepalive_time))
        < 0) {
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "[%s:%d] setsockopt(TCP_KEEPIDLE) failed: %s (%d)", __FILE__, __LINE__,
                            strerror(prte_socket_errno), prte_socket_errno);
        return;
    }
#    endif // TCP_KEEPIDLE
#    if defined(TCP_KEEPINTVL)
    /* set the keepalive interval */
    if (setsockopt(sd, IPPROTO_TCP, TCP_KEEPINTVL, &prte_mca_oob_tcp_component.keepalive_intvl,
                   sizeof(prte_mca_oob_tcp_component.keepalive_intvl))
        < 0) {
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "[%s:%d] setsockopt(TCP_KEEPINTVL) failed: %s (%d)", __FILE__, __LINE__,
                            strerror(prte_socket_errno), prte_socket_errno);
        return;
    }
#    endif // TCP_KEEPINTVL
#    if defined(TCP_KEEPCNT)
    /* set the miss rate */
    if (setsockopt(sd, IPPROTO_TCP, TCP_KEEPCNT, &prte_mca_oob_tcp_component.keepalive_probes,
                   sizeof(prte_mca_oob_tcp_component.keepalive_probes))
        < 0) {
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "[%s:%d] setsockopt(TCP_KEEPCNT) failed: %s (%d)", __FILE__, __LINE__,
                            strerror(prte_socket_errno), prte_socket_errno);
    }
#    endif // TCP_KEEPCNT
#endif     // SO_KEEPALIVE
}

void prte_oob_tcp_set_socket_options(int sd)
{
#if defined(TCP_NODELAY)
    int optval;
    optval = 1;
    if (setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (char *) &optval, sizeof(optval)) < 0) {
        prte_backtrace_print(stderr, NULL, 1);
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "[%s:%d] setsockopt(TCP_NODELAY) failed: %s (%d)", __FILE__, __LINE__,
                            strerror(prte_socket_errno), prte_socket_errno);
    }
#endif
#if defined(SO_SNDBUF)
    if (prte_mca_oob_tcp_component.tcp_sndbuf > 0
        && setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (char *) &prte_mca_oob_tcp_component.tcp_sndbuf,
                      sizeof(int))
               < 0) {
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "[%s:%d] setsockopt(SO_SNDBUF) failed: %s (%d)", __FILE__, __LINE__,
                            strerror(prte_socket_errno), prte_socket_errno);
    }
#endif
#if defined(SO_RCVBUF)
    if (prte_mca_oob_tcp_component.tcp_rcvbuf > 0
        && setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (char *) &prte_mca_oob_tcp_component.tcp_rcvbuf,
                      sizeof(int))
               < 0) {
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "[%s:%d] setsockopt(SO_RCVBUF) failed: %s (%d)", __FILE__, __LINE__,
                            strerror(prte_socket_errno), prte_socket_errno);
    }
#endif

    if (0 < prte_mca_oob_tcp_component.keepalive_time) {
        set_keepalive(sd);
    }
}

prte_oob_tcp_peer_t *prte_oob_tcp_peer_lookup(const pmix_proc_t *name)
{
    prte_oob_tcp_peer_t *peer;

    PMIX_LIST_FOREACH(peer, &prte_mca_oob_tcp_component.peers, prte_oob_tcp_peer_t)
    {
        if (PMIX_CHECK_PROCID(name, &peer->name)) {
            return peer;
        }
    }
    return NULL;
}

char *prte_oob_tcp_state_print(prte_oob_tcp_state_t state)
{
    switch (state) {
    case MCA_OOB_TCP_UNCONNECTED:
        return "UNCONNECTED";
    case MCA_OOB_TCP_CLOSED:
        return "CLOSED";
    case MCA_OOB_TCP_RESOLVE:
        return "RESOLVE";
    case MCA_OOB_TCP_CONNECTING:
        return "CONNECTING";
    case MCA_OOB_TCP_CONNECT_ACK:
        return "ACK";
    case MCA_OOB_TCP_CONNECTED:
        return "CONNECTED";
    case MCA_OOB_TCP_FAILED:
        return "FAILED";
    default:
        return "UNKNOWN";
    }
}
