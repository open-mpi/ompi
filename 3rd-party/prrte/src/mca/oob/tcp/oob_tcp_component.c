/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2006-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
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
#ifdef HAVE_NET_IF_H
#    include <net/if.h>
#endif
#ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif
#include <arpa/inet.h>
#include <ctype.h>
#include <sys/socket.h>

#ifndef MIN
#    define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

#include "src/class/pmix_list.h"
#include "src/event/event-internal.h"
#include "src/include/prte_socket_errno.h"
#include "src/runtime/prte_progress_threads.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_if.h"
#include "src/util/error.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/rml/rml.h"
#include "src/mca/state/state.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_wait.h"
#include "src/threads/pmix_threads.h"
#include "src/util/attr.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_parse_options.h"
#include "src/util/pmix_show_help.h"

#include "oob_tcp_peer.h"
#include "src/mca/oob/tcp/oob_tcp.h"
#include "src/mca/oob/tcp/oob_tcp_common.h"
#include "src/mca/oob/tcp/oob_tcp_component.h"
#include "src/mca/oob/tcp/oob_tcp_connection.h"
#include "src/mca/oob/tcp/oob_tcp_listener.h"
#include "src/mca/oob/tcp/oob_tcp_peer.h"

/*
 * Local utility functions
 */

static int tcp_component_register(void);
static int tcp_component_open(void);
static int tcp_component_close(void);

static int component_available(void);
static int component_startup(void);
static void component_shutdown(void);
static int component_send(prte_rml_send_t *msg);
static char *component_get_addr(void);
static int component_set_addr(pmix_proc_t *peer, char **uris);
static bool component_is_reachable(pmix_proc_t *peer);

/*
 * Struct of function pointers and all that to let us be initialized
 */
prte_mca_oob_tcp_component_t prte_mca_oob_tcp_component = {
    .super = {
        .oob_base = {
            PRTE_OOB_BASE_VERSION_2_0_0,
            .pmix_mca_component_name = "tcp",
            PMIX_MCA_BASE_MAKE_VERSION(component,
                                       PRTE_MAJOR_VERSION,
                                       PRTE_MINOR_VERSION,
                                       PMIX_RELEASE_VERSION),
            .pmix_mca_open_component = tcp_component_open,
            .pmix_mca_close_component = tcp_component_close,
            .pmix_mca_register_component_params = tcp_component_register,
        },
        .priority = 30, // default priority of this transport
        .available = component_available,
        .startup = component_startup,
        .shutdown = component_shutdown,
        .send_nb = component_send,
        .get_addr = component_get_addr,
        .set_addr = component_set_addr,
        .is_reachable = component_is_reachable,
    }
};

/*
 * Initialize global variables used w/in this module.
 */
static int tcp_component_open(void)
{
    PMIX_CONSTRUCT(&prte_mca_oob_tcp_component.peers, pmix_list_t);
    PMIX_CONSTRUCT(&prte_mca_oob_tcp_component.listeners, pmix_list_t);
    if (PRTE_PROC_IS_MASTER) {
        PMIX_CONSTRUCT(&prte_mca_oob_tcp_component.listen_thread, pmix_thread_t);
        prte_mca_oob_tcp_component.listen_thread_active = false;
        prte_mca_oob_tcp_component.listen_thread_tv.tv_sec = 3600;
        prte_mca_oob_tcp_component.listen_thread_tv.tv_usec = 0;
    }
    prte_mca_oob_tcp_component.addr_count = 0;
    prte_mca_oob_tcp_component.ipv4conns = NULL;
    prte_mca_oob_tcp_component.ipv4ports = NULL;
    prte_mca_oob_tcp_component.ipv6conns = NULL;
    prte_mca_oob_tcp_component.ipv6ports = NULL;
    prte_mca_oob_tcp_component.if_masks = NULL;

    PMIX_CONSTRUCT(&prte_mca_oob_tcp_component.local_ifs, pmix_list_t);
    return PRTE_SUCCESS;
}

/*
 * Cleanup of global variables used by this module.
 */
static int tcp_component_close(void)
{
    PMIX_LIST_DESTRUCT(&prte_mca_oob_tcp_component.local_ifs);
    PMIX_LIST_DESTRUCT(&prte_mca_oob_tcp_component.peers);

    if (NULL != prte_mca_oob_tcp_component.ipv4conns) {
        PMIX_ARGV_FREE_COMPAT(prte_mca_oob_tcp_component.ipv4conns);
    }
    if (NULL != prte_mca_oob_tcp_component.ipv4ports) {
        PMIX_ARGV_FREE_COMPAT(prte_mca_oob_tcp_component.ipv4ports);
    }

#if PRTE_ENABLE_IPV6
    if (NULL != prte_mca_oob_tcp_component.ipv6conns) {
        PMIX_ARGV_FREE_COMPAT(prte_mca_oob_tcp_component.ipv6conns);
    }
    if (NULL != prte_mca_oob_tcp_component.ipv6ports) {
        PMIX_ARGV_FREE_COMPAT(prte_mca_oob_tcp_component.ipv6ports);
    }
#endif
    if (NULL != prte_mca_oob_tcp_component.if_masks) {
        PMIX_ARGV_FREE_COMPAT(prte_mca_oob_tcp_component.if_masks);
    }
    return PRTE_SUCCESS;
}
static char *static_port_string;
#if PRTE_ENABLE_IPV6
static char *static_port_string6;
#endif // PRTE_ENABLE_IPV6

static char *dyn_port_string;
#if PRTE_ENABLE_IPV6
static char *dyn_port_string6;
#endif

static int tcp_component_register(void)
{
    pmix_mca_base_component_t *component = &prte_mca_oob_tcp_component.super.oob_base;

    /* register oob module parameters */
    prte_mca_oob_tcp_component.peer_limit = -1;
    (void) pmix_mca_base_component_var_register(component, "peer_limit",
                                                "Maximum number of peer connections to simultaneously maintain (-1 = infinite)",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_oob_tcp_component.peer_limit);

    prte_mca_oob_tcp_component.max_retries = 2;
    (void) pmix_mca_base_component_var_register(component, "peer_retries",
                                                "Number of times to try shutting down a connection before giving up",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_oob_tcp_component.max_retries);

    prte_mca_oob_tcp_component.tcp_sndbuf = 0;
    (void) pmix_mca_base_component_var_register(component, "sndbuf",
                                                "TCP socket send buffering size (in bytes, 0 => leave system default)",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_oob_tcp_component.tcp_sndbuf);

    prte_mca_oob_tcp_component.tcp_rcvbuf = 0;
    (void) pmix_mca_base_component_var_register(component, "rcvbuf",
                                                "TCP socket receive buffering size (in bytes, 0 => leave system default)",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_oob_tcp_component.tcp_rcvbuf);


    static_port_string = NULL;
    (void) pmix_mca_base_component_var_register(component, "static_ipv4_ports",
                                                "Static ports for daemons and procs (IPv4)",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &static_port_string);

    /* if ports were provided, parse the provided range */
    if (NULL != static_port_string) {
        pmix_util_parse_range_options(static_port_string, &prte_mca_oob_tcp_component.tcp_static_ports);
        if (0 == strcmp(prte_mca_oob_tcp_component.tcp_static_ports[0], "-1")) {
            PMIX_ARGV_FREE_COMPAT(prte_mca_oob_tcp_component.tcp_static_ports);
            prte_mca_oob_tcp_component.tcp_static_ports = NULL;
        }
    } else {
        prte_mca_oob_tcp_component.tcp_static_ports = NULL;
    }

#if PRTE_ENABLE_IPV6
    static_port_string6 = NULL;
    (void) pmix_mca_base_component_var_register(component, "static_ipv6_ports",
                                                "Static ports for daemons and procs (IPv6)",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &static_port_string6);

    /* if ports were provided, parse the provided range */
    if (NULL != static_port_string6) {
        pmix_util_parse_range_options(static_port_string6,
                                      &prte_mca_oob_tcp_component.tcp6_static_ports);
        if (0 == strcmp(prte_mca_oob_tcp_component.tcp6_static_ports[0], "-1")) {
            PMIX_ARGV_FREE_COMPAT(prte_mca_oob_tcp_component.tcp6_static_ports);
            prte_mca_oob_tcp_component.tcp6_static_ports = NULL;
        }
    } else {
        prte_mca_oob_tcp_component.tcp6_static_ports = NULL;
    }
#endif // PRTE_ENABLE_IPV6

    if (NULL != prte_mca_oob_tcp_component.tcp_static_ports
        || NULL != prte_mca_oob_tcp_component.tcp6_static_ports) {
        prte_static_ports = true;
    }

    dyn_port_string = NULL;
    (void) pmix_mca_base_component_var_register(component, "dynamic_ipv4_ports",
                                                "Range of ports to be dynamically used by daemons and procs (IPv4)",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &dyn_port_string);
    /* if ports were provided, parse the provided range */
    if (NULL != dyn_port_string) {
        /* can't have both static and dynamic ports! */
        if (prte_static_ports) {
            char *err = PMIX_ARGV_JOIN_COMPAT(prte_mca_oob_tcp_component.tcp_static_ports, ',');
            pmix_show_help("help-oob-tcp.txt", "static-and-dynamic", true, err, dyn_port_string);
            free(err);
            return PRTE_ERROR;
        }
        pmix_util_parse_range_options(dyn_port_string, &prte_mca_oob_tcp_component.tcp_dyn_ports);
        if (0 == strcmp(prte_mca_oob_tcp_component.tcp_dyn_ports[0], "-1")) {
            PMIX_ARGV_FREE_COMPAT(prte_mca_oob_tcp_component.tcp_dyn_ports);
            prte_mca_oob_tcp_component.tcp_dyn_ports = NULL;
        }
    } else {
        prte_mca_oob_tcp_component.tcp_dyn_ports = NULL;
    }

#if PRTE_ENABLE_IPV6
    dyn_port_string6 = NULL;
    (void) pmix_mca_base_component_var_register(component, "dynamic_ipv6_ports",
                                                "Range of ports to be dynamically used by daemons and procs (IPv6)",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &dyn_port_string6);
    /* if ports were provided, parse the provided range */
    if (NULL != dyn_port_string6) {
        /* can't have both static and dynamic ports! */
        if (prte_static_ports) {
            char *err4 = NULL, *err6 = NULL;
            if (NULL != prte_mca_oob_tcp_component.tcp_static_ports) {
                err4 = PMIX_ARGV_JOIN_COMPAT(prte_mca_oob_tcp_component.tcp_static_ports, ',');
            }
            if (NULL != prte_mca_oob_tcp_component.tcp6_static_ports) {
                err6 = PMIX_ARGV_JOIN_COMPAT(prte_mca_oob_tcp_component.tcp6_static_ports, ',');
            }
            pmix_show_help("help-oob-tcp.txt", "static-and-dynamic-ipv6", true,
                           (NULL == err4) ? "N/A" : err4, (NULL == err6) ? "N/A" : err6,
                           dyn_port_string6);
            if (NULL != err4) {
                free(err4);
            }
            if (NULL != err6) {
                free(err6);
            }
            return PRTE_ERROR;
        }
        pmix_util_parse_range_options(dyn_port_string6, &prte_mca_oob_tcp_component.tcp6_dyn_ports);
        if (0 == strcmp(prte_mca_oob_tcp_component.tcp6_dyn_ports[0], "-1")) {
            PMIX_ARGV_FREE_COMPAT(prte_mca_oob_tcp_component.tcp6_dyn_ports);
            prte_mca_oob_tcp_component.tcp6_dyn_ports = NULL;
        }
    } else {
        prte_mca_oob_tcp_component.tcp6_dyn_ports = NULL;
    }
#endif // PRTE_ENABLE_IPV6

    prte_mca_oob_tcp_component.disable_ipv4_family = false;
    (void) pmix_mca_base_component_var_register(component, "disable_ipv4_family",
                                                "Disable the IPv4 interfaces",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_oob_tcp_component.disable_ipv4_family);

#if PRTE_ENABLE_IPV6
    prte_mca_oob_tcp_component.disable_ipv6_family = false;
    (void) pmix_mca_base_component_var_register(component, "disable_ipv6_family",
                                                "Disable the IPv6 interfaces",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_oob_tcp_component.disable_ipv6_family);
#endif // PRTE_ENABLE_IPV6

    // Wait for this amount of time before sending the first keepalive probe
    prte_mca_oob_tcp_component.keepalive_time = 300;
    (void) pmix_mca_base_component_var_register(component, "keepalive_time",
                                                "Idle time in seconds before starting to send keepalives (keepalive_time <= 0 disables "
                                                "keepalive functionality)",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_oob_tcp_component.keepalive_time);

    // Resend keepalive probe every INT seconds
    prte_mca_oob_tcp_component.keepalive_intvl = 20;
    (void) pmix_mca_base_component_var_register(component, "keepalive_intvl",
                                                "Time between successive keepalive pings when peer has not responded, in seconds (ignored "
                                                "if keepalive_time <= 0)",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_oob_tcp_component.keepalive_intvl);

    // After sending PR probes every INT seconds consider the connection dead
    prte_mca_oob_tcp_component.keepalive_probes = 9;
    (void) pmix_mca_base_component_var_register(component, "keepalive_probes",
                                                "Number of keepalives that can be missed before "
                                                "declaring error (ignored if keepalive_time <= 0)",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_oob_tcp_component.keepalive_probes);

    prte_mca_oob_tcp_component.retry_delay = 0;
    (void) pmix_mca_base_component_var_register(component, "retry_delay",
                                                "Time (in sec) to wait before trying to connect to peer again",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_oob_tcp_component.retry_delay);

    prte_mca_oob_tcp_component.max_recon_attempts = 10;
    (void) pmix_mca_base_component_var_register(component, "max_recon_attempts",
                                                "Max number of times to attempt connection before giving up (-1 -> never give up)",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_oob_tcp_component.max_recon_attempts);

    return PRTE_SUCCESS;
}

static int component_available(void)
{
    pmix_pif_t *copied_interface, *selected_interface;
    struct sockaddr_storage my_ss;
    char name[PMIX_IF_NAMESIZE];
    /* Larger than necessary, used for copying mask */
    char string[50];
    int kindex;
    int i;
    bool keeploopback = false;

    pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                        "oob:tcp: component_available called");

    /* if we are the master, then check the interfaces for loopbacks
     * and keep loopbacks only if no non-loopback interface exists */
    if (PRTE_PROC_IS_MASTER) {
        keeploopback = true;
        PMIX_LIST_FOREACH(selected_interface, &pmix_if_list, pmix_pif_t)
        {
            if (!(selected_interface->if_flags & IFF_LOOPBACK)) {
                keeploopback = false;
                break;
            }
        }
    }

    /* look at all available interfaces */
    PMIX_LIST_FOREACH(selected_interface, &pmix_if_list, pmix_pif_t)
    {
        if ((selected_interface->if_flags & IFF_LOOPBACK) &&
            !keeploopback) {
            continue;
        }

        i = selected_interface->if_index;
        kindex = selected_interface->if_kernel_index;
        memcpy((struct sockaddr *) &my_ss, &selected_interface->if_addr,
               MIN(sizeof(struct sockaddr_storage), sizeof(selected_interface->if_addr)));

        /* Refs ticket #3019
         * it would probably be worthwhile to print out a warning if PRRTE detects multiple
         * IP interfaces that are "up" on the same subnet (because that's a Bad Idea). Note
         * that we should only check for this after applying the relevant include/exclude
         * list MCA params. If we detect redundant ports, we can also automatically ignore
         * them so that applications won't hang.
         */

        /* add this address to our connections */
        if (AF_INET == my_ss.ss_family) {
            pmix_output_verbose(10, prte_oob_base_framework.framework_output,
                                "%s oob:tcp:init adding %s to our list of %s connections",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                pmix_net_get_hostname((struct sockaddr *) &my_ss),
                                (AF_INET == my_ss.ss_family) ? "V4" : "V6");
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prte_mca_oob_tcp_component.ipv4conns,
                                    pmix_net_get_hostname((struct sockaddr *) &my_ss));
        } else if (AF_INET6 == my_ss.ss_family) {
#if PRTE_ENABLE_IPV6
            pmix_output_verbose(10, prte_oob_base_framework.framework_output,
                                "%s oob:tcp:init adding %s to our list of %s connections",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                pmix_net_get_hostname((struct sockaddr *) &my_ss),
                                (AF_INET == my_ss.ss_family) ? "V4" : "V6");
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prte_mca_oob_tcp_component.ipv6conns,
                                    pmix_net_get_hostname((struct sockaddr *) &my_ss));
#endif // PRTE_ENABLE_IPV6
        } else {
            pmix_output_verbose(10, prte_oob_base_framework.framework_output,
                                "%s oob:tcp:init ignoring %s from out list of connections",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                pmix_net_get_hostname((struct sockaddr *) &my_ss));
            continue;
        }
        copied_interface = PMIX_NEW(pmix_pif_t);
        if (NULL == copied_interface) {
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        pmix_string_copy(copied_interface->if_name, selected_interface->if_name, sizeof(name));
        copied_interface->if_index = i;
        copied_interface->if_kernel_index = kindex;
        copied_interface->af_family = my_ss.ss_family;
        copied_interface->if_flags = selected_interface->if_flags;
        copied_interface->if_speed = selected_interface->if_speed;
        memcpy(&copied_interface->if_addr, &selected_interface->if_addr,
               sizeof(struct sockaddr_storage));
        copied_interface->if_mask = selected_interface->if_mask;
        /* If bandwidth is not found, set to arbitrary non zero value */
        copied_interface->if_bandwidth = selected_interface->if_bandwidth > 0
                                             ? selected_interface->if_bandwidth
                                             : 1;
        memcpy(&copied_interface->if_mac, &selected_interface->if_mac,
               sizeof(copied_interface->if_mac));
        copied_interface->ifmtu = selected_interface->ifmtu;
        /* Add the if_mask to the list */
        sprintf(string, "%d", selected_interface->if_mask);
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prte_mca_oob_tcp_component.if_masks, string);
        pmix_list_append(&prte_mca_oob_tcp_component.local_ifs, &(copied_interface->super));
    }

    if (0 == PMIX_ARGV_COUNT_COMPAT(prte_mca_oob_tcp_component.ipv4conns)
#if PRTE_ENABLE_IPV6
        && 0 == PMIX_ARGV_COUNT_COMPAT(prte_mca_oob_tcp_component.ipv6conns)
#endif
    ) {
        return PRTE_ERR_NOT_AVAILABLE;
    }

    return PRTE_SUCCESS;
}

/* Start all modules */
static int component_startup(void)
{
    int rc = PRTE_SUCCESS;

    pmix_output_verbose(2, prte_oob_base_framework.framework_output, "%s TCP STARTUP",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    /* if we are a daemon/HNP,
     * then it is possible that someone else may initiate a
     * connection to us. In these cases, we need to start the
     * listening thread/event. Otherwise, we will be the one
     * initiating communication, and there is no need for
     * a listener */
    if (PRTE_PROC_IS_MASTER || PRTE_PROC_IS_DAEMON) {
        if (PRTE_SUCCESS != (rc = prte_oob_tcp_start_listening())) {
            PRTE_ERROR_LOG(rc);
        }
    }

    return rc;
}

static void component_shutdown(void)
{
    int i = 0, rc;

    pmix_output_verbose(2, prte_oob_base_framework.framework_output, "%s TCP SHUTDOWN",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    if (PRTE_PROC_IS_MASTER && prte_mca_oob_tcp_component.listen_thread_active) {
        prte_mca_oob_tcp_component.listen_thread_active = false;
        /* tell the thread to exit */
        rc = write(prte_mca_oob_tcp_component.stop_thread[1], &i, sizeof(int));
        if (0 < rc) {
            pmix_thread_join(&prte_mca_oob_tcp_component.listen_thread, NULL);
        }

        close(prte_mca_oob_tcp_component.stop_thread[0]);
        close(prte_mca_oob_tcp_component.stop_thread[1]);

    } else {
        pmix_output_verbose(2, prte_oob_base_framework.framework_output, "no hnp or not active");
    }

    /* cleanup listen event list */
    PMIX_LIST_DESTRUCT(&prte_mca_oob_tcp_component.listeners);

    pmix_output_verbose(2, prte_oob_base_framework.framework_output, "%s TCP SHUTDOWN done",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
}

static int component_send(prte_rml_send_t *msg)
{
    pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                        "%s oob:tcp:send_nb to peer %s:%d seq = %d",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&msg->dst), msg->tag,
                        msg->seq_num);

    /* The module will first see if it knows
     * of a way to send the data to the target, and then
     * attempt to send the data. It  will call the cbfunc
     * with the status upon completion - if it can't do it for
     * some reason, it will pass the error to our fn below so
     * it can do something about it
     */
    prte_oob_tcp_module.send_nb(msg);
    return PRTE_SUCCESS;
}

static char *component_get_addr(void)
{
    char *cptr = NULL, *tmp, *tp, *tm;

    if (!prte_mca_oob_tcp_component.disable_ipv4_family &&
        NULL != prte_mca_oob_tcp_component.ipv4conns) {
        tmp = PMIX_ARGV_JOIN_COMPAT(prte_mca_oob_tcp_component.ipv4conns, ',');
        tp = PMIX_ARGV_JOIN_COMPAT(prte_mca_oob_tcp_component.ipv4ports, ',');
        tm = PMIX_ARGV_JOIN_COMPAT(prte_mca_oob_tcp_component.if_masks, ',');
        pmix_asprintf(&cptr, "tcp://%s:%s:%s", tmp, tp, tm);
        free(tmp);
        free(tp);
        free(tm);
    }
#if PRTE_ENABLE_IPV6
    if (!prte_mca_oob_tcp_component.disable_ipv6_family && NULL != prte_mca_oob_tcp_component.ipv6conns) {
        char *tmp2;

        /* Fixes #2498
         * RFC 3986, section 3.2.2
         * The notation in that case is to encode the IPv6 IP number in square brackets:
         * "http://[2001:db8:1f70::999:de8:7648:6e8]:100/"
         * A host identified by an Internet Protocol literal address, version 6 [RFC3513]
         * or later, is distinguished by enclosing the IP literal within square brackets.
         * This is the only place where square bracket characters are allowed in the URI
         * syntax. In anticipation of future, as-yet-undefined IP literal address formats,
         * an implementation may use an optional version flag to indicate such a format
         * explicitly rather than rely on heuristic determination.
         */
        tmp = PMIX_ARGV_JOIN_COMPAT(prte_mca_oob_tcp_component.ipv6conns, ',');
        tp = PMIX_ARGV_JOIN_COMPAT(prte_mca_oob_tcp_component.ipv6ports, ',');
        tm = PMIX_ARGV_JOIN_COMPAT(prte_mca_oob_tcp_component.if_masks, ',');
        if (NULL == cptr) {
            /* no ipv4 stuff */
            pmix_asprintf(&cptr, "tcp6://[%s]:%s:%s", tmp, tp, tm);
        } else {
            pmix_asprintf(&tmp2, "%s;tcp6://[%s]:%s:%s", cptr, tmp, tp, tm);
            free(cptr);
            cptr = tmp2;
        }
        free(tmp);
        free(tp);
        free(tm);
    }
#endif // PRTE_ENABLE_IPV6

    /* return our uri */
    return cptr;
}

/* the host in this case is always in "dot" notation, and
 * thus we do not need to do a DNS lookup to convert it */
static int parse_uri(const uint16_t af_family, const char *host, const char *port,
                     struct sockaddr_storage *inaddr)
{
    struct sockaddr_in *in;

    if (AF_INET == af_family) {
        memset(inaddr, 0, sizeof(struct sockaddr_in));
        in = (struct sockaddr_in *) inaddr;
        in->sin_family = AF_INET;
        in->sin_addr.s_addr = inet_addr(host);
        if (in->sin_addr.s_addr == INADDR_NONE) {
            return PRTE_ERR_BAD_PARAM;
        }
        ((struct sockaddr_in *) inaddr)->sin_port = htons(atoi(port));
    }
#if PRTE_ENABLE_IPV6
    else if (AF_INET6 == af_family) {
        struct sockaddr_in6 *in6;
        memset(inaddr, 0, sizeof(struct sockaddr_in6));
        in6 = (struct sockaddr_in6 *) inaddr;

        if (0 == inet_pton(AF_INET6, host, (void *) &in6->sin6_addr)) {
            pmix_output(0, "oob_tcp_parse_uri: Could not convert %s\n", host);
            return PRTE_ERR_BAD_PARAM;
        }
        in6->sin6_family = AF_INET6;
        in6->sin6_port = htons(atoi(port));
    }
#endif
    else {
        return PRTE_ERR_NOT_SUPPORTED;
    }
    return PRTE_SUCCESS;
}

static int component_set_addr(pmix_proc_t *peer, char **uris)
{
    char **addrs, **masks, *hptr;
    char *tcpuri = NULL, *host, *ports, *masks_string;
    int i, j, rc;
    uint16_t af_family = AF_UNSPEC;
    uint64_t ui64;
    bool found;
    prte_oob_tcp_peer_t *pr;
    prte_oob_tcp_addr_t *maddr;

    memcpy(&ui64, (char *) peer, sizeof(uint64_t));
    /* cycle across component parts and see if one belongs to us */
    found = false;

    for (i = 0; NULL != uris[i]; i++) {
        tcpuri = strdup(uris[i]);
        if (NULL == tcpuri) {
            pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                                "%s oob:tcp: out of memory", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
            continue;
        }
        if (0 == strncmp(uris[i], "tcp:", 4)) {
            af_family = AF_INET;
            host = tcpuri + strlen("tcp://");
        } else if (0 == strncmp(uris[i], "tcp6:", 5)) {
#if PRTE_ENABLE_IPV6
            af_family = AF_INET6;
            host = tcpuri + strlen("tcp6://");
#else  // PRTE_ENABLE_IPV6
            /* we don't support this connection type */
            pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                                "%s oob:tcp: address %s not supported",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), uris[i]);
            free(tcpuri);
            continue;
#endif // PRTE_ENABLE_IPV6
        } else {
            /* not one of ours */
            pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                                "%s oob:tcp: ignoring address %s",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), uris[i]);
            free(tcpuri);
            continue;
        }

        /* this one is ours - record the peer */
        pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                            "%s oob:tcp: working peer %s address %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(peer), uris[i]);

        /* separate the mask from the network addrs */
        masks_string = strrchr(tcpuri, ':');
        if (NULL == masks_string) {
            PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
            free(tcpuri);
            continue;
        }
        *masks_string = '\0';
        masks_string++;
        masks = PMIX_ARGV_SPLIT_COMPAT(masks_string, ',');

        /* separate the ports from the network addrs */
        ports = strrchr(tcpuri, ':');
        if (NULL == ports) {
            PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
            free(tcpuri);
            continue;
        }
        *ports = '\0';
        ports++;

        /* split the addrs */
        /* if this is a tcp6 connection, the first one will have a '['
         * at the beginning of it, and the last will have a ']' at the
         * end - we need to remove those extra characters
         */
        hptr = host;
#if PRTE_ENABLE_IPV6
        if (AF_INET6 == af_family) {
            if ('[' == host[0]) {
                hptr = &host[1];
            }
            if (']' == host[strlen(host) - 1]) {
                host[strlen(host) - 1] = '\0';
            }
        }
#endif // PRTE_ENABLE_IPV6
        addrs = PMIX_ARGV_SPLIT_COMPAT(hptr, ',');

        /* cycle across the provided addrs */
        for (j = 0; NULL != addrs[j]; j++) {
            if (NULL == masks[j]) {
                /* Missing mask information */
                pmix_output_verbose(2, prte_oob_base_framework.framework_output,
                                    "%s oob:tcp: uri missing mask information.",
                                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
                return PRTE_ERR_TAKE_NEXT_OPTION;
            }
            /* if they gave us "localhost", then just take the first conn on our list */
            if (0 == strcasecmp(addrs[j], "localhost")) {
#if PRTE_ENABLE_IPV6
                if (AF_INET6 == af_family) {
                    if (NULL == prte_mca_oob_tcp_component.ipv6conns
                        || NULL == prte_mca_oob_tcp_component.ipv6conns[0]) {
                        continue;
                    }
                    host = prte_mca_oob_tcp_component.ipv6conns[0];
                } else {
#endif // PRTE_ENABLE_IPV6
                    if (NULL == prte_mca_oob_tcp_component.ipv4conns
                        || NULL == prte_mca_oob_tcp_component.ipv4conns[0]) {
                        continue;
                    }
                    host = prte_mca_oob_tcp_component.ipv4conns[0];
#if PRTE_ENABLE_IPV6
                }
#endif
            } else {
                host = addrs[j];
            }

            if (NULL == (pr = prte_oob_tcp_peer_lookup(peer))) {
                pr = PMIX_NEW(prte_oob_tcp_peer_t);
                PMIX_XFER_PROCID(&pr->name, peer);
                pmix_output_verbose(20, prte_oob_base_framework.framework_output,
                                    "%s SET_PEER ADDING PEER %s",
                                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(peer));
                pmix_list_append(&prte_mca_oob_tcp_component.peers, &pr->super);
            }

            maddr = PMIX_NEW(prte_oob_tcp_addr_t);
            ((struct sockaddr_storage *) &(maddr->addr))->ss_family = af_family;
            if (PRTE_SUCCESS
                != (rc = parse_uri(af_family, host, ports,
                                   (struct sockaddr_storage *) &(maddr->addr)))) {
                PRTE_ERROR_LOG(rc);
                PMIX_RELEASE(maddr);
                pmix_list_remove_item(&prte_mca_oob_tcp_component.peers, &pr->super);
                PMIX_RELEASE(pr);
                return PRTE_ERR_TAKE_NEXT_OPTION;
            }
            maddr->if_mask = atoi(masks[j]);

            pmix_output_verbose(20, prte_oob_base_framework.framework_output,
                                "%s set_peer: peer %s is listening on net %s port %s",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(peer),
                                (NULL == host) ? "NULL" : host, (NULL == ports) ? "NULL" : ports);
            pmix_list_append(&pr->addrs, &maddr->super);

            found = true;
        }
        PMIX_ARGV_FREE_COMPAT(addrs);
        free(tcpuri);
    }
    if (found) {
        /* indicate that this peer is addressable by this component */
        return PRTE_SUCCESS;
    }

    /* otherwise indicate that it is not addressable by us */
    return PRTE_ERR_TAKE_NEXT_OPTION;
}

static bool component_is_reachable(pmix_proc_t *peer)
{
    PRTE_HIDE_UNUSED_PARAMS(peer);

    /* assume we can reach the hop - the module will tell us if it can't
     * when we try to send the first time, and then we'll correct it */
    return true;
}

void prte_mca_oob_tcp_component_set_module(int fd, short args, void *cbdata)
{
    prte_oob_tcp_peer_op_t *pop = (prte_oob_tcp_peer_op_t *) cbdata;
    prte_oob_base_peer_t *bpr;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(pop);

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s tcp:set_module called for peer %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        PRTE_NAME_PRINT(&pop->peer));

    /* make sure the OOB knows that we can reach this peer - we
     * are in the same event base as the OOB base, so we can
     * directly access its storage
     */
    bpr = prte_oob_base_get_peer(&pop->peer);
    if (NULL == bpr) {
        bpr = PMIX_NEW(prte_oob_base_peer_t);
        PMIX_XFER_PROCID(&bpr->name, &pop->peer);
        pmix_list_append(&prte_oob_base.peers, &bpr->super);
    }
    pmix_bitmap_set_bit(&bpr->addressable, prte_mca_oob_tcp_component.super.idx);
    bpr->component = &prte_mca_oob_tcp_component.super;

    PMIX_RELEASE(pop);
}

void prte_mca_oob_tcp_component_lost_connection(int fd, short args, void *cbdata)
{
    prte_oob_tcp_peer_op_t *pop = (prte_oob_tcp_peer_op_t *) cbdata;
    prte_oob_base_peer_t *bpr;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(pop);

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s tcp:lost connection called for peer %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&pop->peer));

    /* Mark that we no longer support this peer */
    bpr = prte_oob_base_get_peer(&pop->peer);
    if (NULL != bpr) {
        pmix_bitmap_clear_bit(&bpr->addressable, prte_mca_oob_tcp_component.super.idx);
        pmix_list_remove_item(&prte_oob_base.peers, &bpr->super);
        PMIX_RELEASE(bpr);
    }

    if (!prte_finalizing) {
        /* activate the proc state */
        if (PRTE_SUCCESS != prte_rml_route_lost(pop->peer.rank)) {
            PRTE_ACTIVATE_PROC_STATE(&pop->peer, PRTE_PROC_STATE_LIFELINE_LOST);
        } else {
            PRTE_ACTIVATE_PROC_STATE(&pop->peer, PRTE_PROC_STATE_COMM_FAILED);
        }
    }
    PMIX_RELEASE(pop);
}

void prte_mca_oob_tcp_component_no_route(int fd, short args, void *cbdata)
{
    prte_oob_tcp_msg_error_t *mop = (prte_oob_tcp_msg_error_t *) cbdata;
    prte_oob_base_peer_t *bpr;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(mop);

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s tcp:no route called for peer %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        PRTE_NAME_PRINT(&mop->hop));

    /* mark that we cannot reach this hop */
    bpr = prte_oob_base_get_peer(&mop->hop);
    if (NULL == bpr) {
        bpr = PMIX_NEW(prte_oob_base_peer_t);
        PMIX_XFER_PROCID(&bpr->name, &mop->hop);
    }
    pmix_bitmap_clear_bit(&bpr->addressable, prte_mca_oob_tcp_component.super.idx);

    /* report the error back to the OOB and let it try other components
     * or declare a problem
     */
    mop->rmsg->retries++;
    /* activate the OOB send state */
    PRTE_OOB_SEND(mop->rmsg);

    PMIX_RELEASE(mop);
}

void prte_mca_oob_tcp_component_hop_unknown(int fd, short args, void *cbdata)
{
    prte_oob_tcp_msg_error_t *mop = (prte_oob_tcp_msg_error_t *) cbdata;
    prte_rml_send_t *snd;
    prte_oob_base_peer_t *bpr;
    pmix_status_t rc;
    pmix_byte_object_t bo;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(mop);

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s tcp:unknown hop called for peer %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        PRTE_NAME_PRINT(&mop->hop));

    if (prte_finalizing || prte_abnormal_term_ordered) {
        /* just ignore the problem */
        PMIX_RELEASE(mop);
        return;
    }

    /* mark that this component cannot reach this hop */
    bpr = prte_oob_base_get_peer(&mop->hop);
    if (NULL == bpr) {
        /* the overall OOB has no knowledge of this hop. Only
         * way this could happen is if the peer contacted us
         * via this component, and it wasn't entered into the
         * OOB framework hash table. We have no way of knowing
         * what to do next, so just output an error message and
         * abort */
        pmix_output(0,
                    "%s ERROR: message to %s requires routing and the OOB has no knowledge of the "
                    "reqd hop %s",
                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&mop->snd->hdr.dst),
                    PRTE_NAME_PRINT(&mop->hop));
        PRTE_ACTIVATE_PROC_STATE(&mop->hop, PRTE_PROC_STATE_UNABLE_TO_SEND_MSG);
        PMIX_RELEASE(mop);
        return;
    }
    pmix_bitmap_clear_bit(&bpr->addressable, prte_mca_oob_tcp_component.super.idx);

    /* mark that this component cannot reach this destination either */
    bpr = prte_oob_base_get_peer(&mop->snd->hdr.dst);
    if (NULL == bpr) {
        pmix_output(
            0,
            "%s ERROR: message to %s requires routing and the OOB has no knowledge of this process",
            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&mop->snd->hdr.dst));
        PRTE_ACTIVATE_PROC_STATE(&mop->hop, PRTE_PROC_STATE_UNABLE_TO_SEND_MSG);
        PMIX_RELEASE(mop);
        return;
    }
    pmix_bitmap_clear_bit(&bpr->addressable, prte_mca_oob_tcp_component.super.idx);

    /* post the message to the OOB so it can see
     * if another component can transfer it
     */
    MCA_OOB_TCP_HDR_NTOH(&mop->snd->hdr);
    snd = PMIX_NEW(prte_rml_send_t);
    snd->retries = mop->rmsg->retries + 1;
    PMIX_XFER_PROCID(&snd->dst, &mop->snd->hdr.dst);
    PMIX_XFER_PROCID(&snd->origin, &mop->snd->hdr.origin);
    snd->tag = mop->snd->hdr.tag;
    snd->seq_num = mop->snd->hdr.seq_num;
    bo.bytes = mop->snd->data;
    bo.size = mop->snd->hdr.nbytes;
    PMIX_DATA_BUFFER_CREATE(snd->dbuf);
    rc = PMIx_Data_load(snd->dbuf, &bo);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
    snd->cbfunc = NULL;
    snd->cbdata = NULL;
    /* activate the OOB send state */
    PRTE_OOB_SEND(snd);
    /* protect the data */
    mop->snd->data = NULL;

    PMIX_RELEASE(mop);
}

void prte_mca_oob_tcp_component_failed_to_connect(int fd, short args, void *cbdata)
{
    prte_oob_tcp_peer_op_t *pop = (prte_oob_tcp_peer_op_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(pop);

    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s tcp:failed_to_connect called for peer %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&pop->peer));

    /* if we are terminating, then don't attempt to reconnect */
    if (prte_prteds_term_ordered || prte_finalizing || prte_abnormal_term_ordered) {
        PMIX_RELEASE(pop);
        return;
    }

    /* activate the proc state */
    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s tcp:failed_to_connect unable to reach peer %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&pop->peer));

    PRTE_ACTIVATE_PROC_STATE(&pop->peer, PRTE_PROC_STATE_FAILED_TO_CONNECT);
    PMIX_RELEASE(pop);
}


/* OOB TCP Class instances */

static void peer_cons(prte_oob_tcp_peer_t *peer)
{
    peer->auth_method = NULL;
    peer->sd = -1;
    PMIX_CONSTRUCT(&peer->addrs, pmix_list_t);
    peer->active_addr = NULL;
    peer->state = MCA_OOB_TCP_UNCONNECTED;
    peer->num_retries = 0;
    PMIX_CONSTRUCT(&peer->send_queue, pmix_list_t);
    peer->send_msg = NULL;
    peer->recv_msg = NULL;
    peer->send_ev_active = false;
    peer->recv_ev_active = false;
    peer->timer_ev_active = false;
}
static void peer_des(prte_oob_tcp_peer_t *peer)
{
    if (NULL != peer->auth_method) {
        free(peer->auth_method);
    }
    if (peer->send_ev_active) {
        prte_event_del(&peer->send_event);
    }
    if (peer->recv_ev_active) {
        prte_event_del(&peer->recv_event);
    }
    if (peer->timer_ev_active) {
        prte_event_del(&peer->timer_event);
    }
    if (0 <= peer->sd) {
        pmix_output_verbose(2, prte_oob_base_framework.framework_output, "%s CLOSING SOCKET %d",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), peer->sd);
        CLOSE_THE_SOCKET(peer->sd);
    }
    PMIX_LIST_DESTRUCT(&peer->addrs);
    PMIX_LIST_DESTRUCT(&peer->send_queue);
}
PMIX_CLASS_INSTANCE(prte_oob_tcp_peer_t, pmix_list_item_t, peer_cons, peer_des);

static void padd_cons(prte_oob_tcp_addr_t *ptr)
{
    memset(&ptr->addr, 0, sizeof(ptr->addr));
    ptr->retries = 0;
    ptr->state = MCA_OOB_TCP_UNCONNECTED;
}
PMIX_CLASS_INSTANCE(prte_oob_tcp_addr_t, pmix_list_item_t, padd_cons, NULL);

static void pop_cons(prte_oob_tcp_peer_op_t *pop)
{
    pop->net = NULL;
    pop->port = NULL;
}
static void pop_des(prte_oob_tcp_peer_op_t *pop)
{
    if (NULL != pop->net) {
        free(pop->net);
    }
    if (NULL != pop->port) {
        free(pop->port);
    }
}
PMIX_CLASS_INSTANCE(prte_oob_tcp_peer_op_t, pmix_object_t, pop_cons, pop_des);

PMIX_CLASS_INSTANCE(prte_oob_tcp_msg_op_t, pmix_object_t, NULL, NULL);

PMIX_CLASS_INSTANCE(prte_oob_tcp_conn_op_t, pmix_object_t, NULL, NULL);

static void nicaddr_cons(prte_oob_tcp_nicaddr_t *ptr)
{
    ptr->af_family = PF_UNSPEC;
    memset(&ptr->addr, 0, sizeof(ptr->addr));
}
PMIX_CLASS_INSTANCE(prte_oob_tcp_nicaddr_t, pmix_list_item_t, nicaddr_cons, NULL);
