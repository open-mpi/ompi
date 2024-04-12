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
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
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
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif
#include <ctype.h>

#include "src/class/pmix_list.h"
#include "src/include/prte_socket_errno.h"
#include "src/util/pmix_argv.h"
#include "src/util/error.h"
#include "src/util/pmix_fd.h"
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
#include "src/mca/oob/tcp/oob_tcp_listener.h"
#include "src/mca/oob/tcp/oob_tcp_peer.h"

static void connection_event_handler(int incoming_sd, short flags, void *cbdata);
static void *listen_thread(pmix_object_t *obj);
static int create_listen(void);
#if PRTE_ENABLE_IPV6
static int create_listen6(void);
#endif
static void connection_handler(int sd, short flags, void *cbdata);
static void connection_event_handler(int sd, short flags, void *cbdata);

/*
 * Component initialization - create a module for each available
 * TCP interface and initialize the static resources associated
 * with that module.
 *
 * Also initializes the list of devices that will be used/supported by
 * the module, using the if_include and if_exclude variables.  This is
 * the only place that this sorting should occur -- all other places
 * should use the tcp_avaiable_devices list.  This is a change from
 * previous versions of this component.
 */
int prte_oob_tcp_start_listening(void)
{
    int rc = PRTE_SUCCESS, rc2 = PRTE_SUCCESS;
    prte_oob_tcp_listener_t *listener;

    /* if we don't have any TCP interfaces, we shouldn't be here */
    if (NULL == prte_mca_oob_tcp_component.ipv4conns
#if PRTE_ENABLE_IPV6
        && NULL == prte_mca_oob_tcp_component.ipv6conns
#endif
    ) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        return PRTE_ERR_NOT_FOUND;
    }

    /* create listen socket(s) for incoming connection attempts */
    rc = create_listen();

#if PRTE_ENABLE_IPV6
    /* create listen socket(s) for incoming connection attempts */
    rc2 = create_listen6();
#endif

    if (PRTE_SUCCESS != rc && PRTE_SUCCESS != rc2) {
        /* we were unable to open any listening sockets */
        pmix_show_help("help-oob-tcp.txt", "no-listeners", true);
        return PRTE_ERR_FATAL;
    }

    /* if I am the HNP, start a listening thread so we can
     * harvest connection requests as rapidly as possible
     */
    if (PRTE_PROC_IS_MASTER) {
        if (0 > pipe(prte_mca_oob_tcp_component.stop_thread)) {
            PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
            return PRTE_ERR_OUT_OF_RESOURCE;
        }

        /* Make sure the pipe FDs are set to close-on-exec so that
           they don't leak into children */
        if (pmix_fd_set_cloexec(prte_mca_oob_tcp_component.stop_thread[0]) != PRTE_SUCCESS
            || pmix_fd_set_cloexec(prte_mca_oob_tcp_component.stop_thread[1]) != PRTE_SUCCESS) {
            close(prte_mca_oob_tcp_component.stop_thread[0]);
            close(prte_mca_oob_tcp_component.stop_thread[1]);
            PRTE_ERROR_LOG(PRTE_ERR_IN_ERRNO);
            return PRTE_ERR_IN_ERRNO;
        }

        prte_mca_oob_tcp_component.listen_thread_active = true;
        prte_mca_oob_tcp_component.listen_thread.t_run = listen_thread;
        prte_mca_oob_tcp_component.listen_thread.t_arg = NULL;
        if (PRTE_SUCCESS != (rc = pmix_thread_start(&prte_mca_oob_tcp_component.listen_thread))) {
            PRTE_ERROR_LOG(rc);
            pmix_output(0, "%s Unable to start listen thread", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        }
        return rc;
    }

    /* otherwise, setup to listen via the event lib */

    PMIX_LIST_FOREACH(listener, &prte_mca_oob_tcp_component.listeners, prte_oob_tcp_listener_t)
    {
        listener->ev_active = true;
        prte_event_set(prte_event_base, &listener->event, listener->sd,
                       PRTE_EV_READ | PRTE_EV_PERSIST, connection_event_handler, 0);
        PMIX_POST_OBJECT(listener);
        prte_event_add(&listener->event, 0);
    }

    return PRTE_SUCCESS;
}

/*
 * Create an IPv4 listen socket and bind to all interfaces.
 *
 * At one time, this also registered a callback with the event library
 * for when connections were received on the listen socket.  This is
 * no longer the case -- the caller must register any events required.
 *
 * Called by both the threaded and event based listen modes.
 */
static int create_listen(void)
{
    int flags, i;
    uint16_t port = 0;
    struct sockaddr_storage inaddr;
    prte_socklen_t addrlen;
    char **ports = NULL;
    int sd = -1;
    char *tconn;
    prte_oob_tcp_listener_t *conn;

    /* If an explicit range of ports was given, find the first open
     * port in the range.  Otherwise, tcp_port_min will be 0, which
     * means "pick any port"
     */
    if (NULL != prte_mca_oob_tcp_component.tcp_static_ports) {
        /* if static ports were provided, take the
         * first entry in the list
         */
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&ports, prte_mca_oob_tcp_component.tcp_static_ports[0]);
        /* flag that we are using static ports */
        prte_static_ports = true;
    } else if (NULL != prte_mca_oob_tcp_component.tcp_dyn_ports) {
        /* take the entire range */
        ports = PMIX_ARGV_COPY_COMPAT(prte_mca_oob_tcp_component.tcp_dyn_ports);
        prte_static_ports = false;
    } else {
        /* flag the system to dynamically take any available port */
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&ports, "0");
        prte_static_ports = false;
    }

    /* bozo check - this should be impossible, but... */
    if (NULL == ports) {
        return PRTE_ERROR;
    }

    /* get the address info for this interface */
    memset(&inaddr, 0, sizeof(inaddr));
    ((struct sockaddr_in *) &inaddr)->sin_family = AF_INET;
    ((struct sockaddr_in *) &inaddr)->sin_addr.s_addr = INADDR_ANY;
    addrlen = sizeof(struct sockaddr_in);

    /* loop across all the specified ports, establishing a socket
     * for each one - note that application procs will ONLY have
     * one socket, but that prun and daemons will have multiple
     * sockets to support more flexible wireup protocols
     */
    for (i = 0; i < PMIX_ARGV_COUNT_COMPAT(ports); i++) {
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "%s attempting to bind to IPv4 port %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ports[i]);
        /* get the port number */
        port = strtol(ports[i], NULL, 10);
        /* convert it to network-byte-order */
        port = htons(port);

        ((struct sockaddr_in *) &inaddr)->sin_port = port;

        /* create a listen socket for incoming connections on this port */
        sd = socket(AF_INET, SOCK_STREAM, 0);
        if (sd < 0) {
            if (EAFNOSUPPORT != prte_socket_errno) {
                pmix_output(0, "prte_mca_oob_tcp_component_init: socket() failed: %s (%d)",
                            strerror(prte_socket_errno), prte_socket_errno);
            }
            PMIX_ARGV_FREE_COMPAT(ports);
            return PRTE_ERR_IN_ERRNO;
        }

        /* Enable/disable reusing ports */
        if (prte_static_ports) {
            flags = 1;
        } else {
            flags = 0;
        }
        if (setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, (const char *) &flags, sizeof(flags)) < 0) {
            pmix_output(0,
                        "prte_oob_tcp_create_listen: unable to set the "
                        "SO_REUSEADDR option (%s:%d)\n",
                        strerror(prte_socket_errno), prte_socket_errno);
            CLOSE_THE_SOCKET(sd);
            PMIX_ARGV_FREE_COMPAT(ports);
            return PRTE_ERROR;
        }

        /* Set the socket to close-on-exec so that no children inherit
           this FD */
        if (pmix_fd_set_cloexec(sd) != PRTE_SUCCESS) {
            pmix_output(0,
                        "prte_oob_tcp_create_listen: unable to set the "
                        "listening socket to CLOEXEC (%s:%d)\n",
                        strerror(prte_socket_errno), prte_socket_errno);
            CLOSE_THE_SOCKET(sd);
            PMIX_ARGV_FREE_COMPAT(ports);
            return PRTE_ERROR;
        }

        if (bind(sd, (struct sockaddr *) &inaddr, addrlen) < 0) {
            if ((EADDRINUSE == prte_socket_errno) || (EADDRNOTAVAIL == prte_socket_errno)) {
                continue;
            }
            pmix_output(0, "%s bind() failed for port %d: %s (%d)",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (int) ntohs(port),
                        strerror(prte_socket_errno), prte_socket_errno);
            CLOSE_THE_SOCKET(sd);
            PMIX_ARGV_FREE_COMPAT(ports);
            return PRTE_ERROR;
        }
        /* resolve assigned port */
        if (getsockname(sd, (struct sockaddr *) &inaddr, &addrlen) < 0) {
            pmix_output(0, "prte_oob_tcp_create_listen: getsockname(): %s (%d)",
                        strerror(prte_socket_errno), prte_socket_errno);
            CLOSE_THE_SOCKET(sd);
            PMIX_ARGV_FREE_COMPAT(ports);
            return PRTE_ERROR;
        }

        /* setup listen backlog to maximum allowed by kernel */
        if (listen(sd, SOMAXCONN) < 0) {
            pmix_output(0, "prte_mca_oob_tcp_component_init: listen(): %s (%d)",
                        strerror(prte_socket_errno), prte_socket_errno);
            CLOSE_THE_SOCKET(sd);
            PMIX_ARGV_FREE_COMPAT(ports);
            return PRTE_ERROR;
        }

        /* set socket up to be non-blocking, otherwise accept could block */
        if ((flags = fcntl(sd, F_GETFL, 0)) < 0) {
            pmix_output(0, "prte_mca_oob_tcp_component_init: fcntl(F_GETFL) failed: %s (%d)",
                        strerror(prte_socket_errno), prte_socket_errno);
            CLOSE_THE_SOCKET(sd);
            PMIX_ARGV_FREE_COMPAT(ports);
            return PRTE_ERROR;
        }
        flags |= O_NONBLOCK;
        if (fcntl(sd, F_SETFL, flags) < 0) {
            pmix_output(0, "prte_mca_oob_tcp_component_init: fcntl(F_SETFL) failed: %s (%d)",
                        strerror(prte_socket_errno), prte_socket_errno);
            CLOSE_THE_SOCKET(sd);
            PMIX_ARGV_FREE_COMPAT(ports);
            return PRTE_ERROR;
        }

        /* add this port to our connections */
        conn = PMIX_NEW(prte_oob_tcp_listener_t);
        conn->sd = sd;
        conn->port = ntohs(((struct sockaddr_in *) &inaddr)->sin_port);
        if (0 == prte_process_info.my_port) {
            /* save the first one */
            prte_process_info.my_port = conn->port;
        }
        pmix_list_append(&prte_mca_oob_tcp_component.listeners, &conn->item);
        /* and to our ports */
        pmix_asprintf(&tconn, "%d", ntohs(((struct sockaddr_in *) &inaddr)->sin_port));
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prte_mca_oob_tcp_component.ipv4ports, tconn);
        free(tconn);
        if (OOB_TCP_DEBUG_CONNECT
            <= pmix_output_get_verbosity(prte_oob_base_framework.framework_output)) {
            port = ntohs(((struct sockaddr_in *) &inaddr)->sin_port);
            pmix_output(0, "%s assigned IPv4 port %d", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), port);
        }

        if (!PRTE_PROC_IS_MASTER) {
            /* only the HNP binds to multiple ports */
            break;
        }
    }
    /* done with this, so release it */
    PMIX_ARGV_FREE_COMPAT(ports);

    if (0 == pmix_list_get_size(&prte_mca_oob_tcp_component.listeners)) {
        /* cleanup */
        if (0 <= sd) {
            CLOSE_THE_SOCKET(sd);
        }
        return PRTE_ERR_SOCKET_NOT_AVAILABLE;
    }

    return PRTE_SUCCESS;
}

#if PRTE_ENABLE_IPV6
/*
 * Create an IPv6 listen socket and bind to all interfaces.
 *
 * At one time, this also registered a callback with the event library
 * for when connections were received on the listen socket.  This is
 * no longer the case -- the caller must register any events required.
 *
 * Called by both the threaded and event based listen modes.
 */
static int create_listen6(void)
{
    int flags, i;
    uint16_t port = 0;
    struct sockaddr_storage inaddr;
    prte_socklen_t addrlen;
    char **ports = NULL;
    int sd;
    char *tconn;
    prte_oob_tcp_listener_t *conn;

    /* If an explicit range of ports was given, find the first open
     * port in the range.  Otherwise, tcp_port_min will be 0, which
     * means "pick any port"
     */
    if (PRTE_PROC_IS_DAEMON) {
        if (NULL != prte_mca_oob_tcp_component.tcp6_static_ports) {
            /* if static ports were provided, take the
             * first entry in the list
             */
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&ports, prte_mca_oob_tcp_component.tcp6_static_ports[0]);
            /* flag that we are using static ports */
            prte_static_ports = true;
        } else if (NULL != prte_mca_oob_tcp_component.tcp6_dyn_ports) {
            /* take the entire range */
            ports = PMIX_ARGV_COPY_COMPAT(prte_mca_oob_tcp_component.tcp6_dyn_ports);
            prte_static_ports = false;
        } else {
            /* flag the system to dynamically take any available port */
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&ports, "0");
            prte_static_ports = false;
        }
    } else {
        if (NULL != prte_mca_oob_tcp_component.tcp6_static_ports) {
            /* if static ports were provided, take the
             * first entry in the list
             */
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&ports, prte_mca_oob_tcp_component.tcp6_static_ports[0]);
            /* flag that we are using static ports */
            prte_static_ports = true;
        } else if (NULL != prte_mca_oob_tcp_component.tcp6_dyn_ports) {
            /* take the entire range */
            ports = PMIX_ARGV_COPY_COMPAT(prte_mca_oob_tcp_component.tcp6_dyn_ports);
            prte_static_ports = false;
        } else {
            /* flag the system to dynamically take any available port */
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&ports, "0");
            prte_static_ports = false;
        }
    }

    /* bozo check - this should be impossible, but... */
    if (NULL == ports) {
        return PRTE_ERROR;
    }

    /* get the address info for this interface */
    memset(&inaddr, 0, sizeof(inaddr));
    ((struct sockaddr_in6 *) &inaddr)->sin6_family = AF_INET6;
    ((struct sockaddr_in6 *) &inaddr)->sin6_addr = in6addr_any;
    addrlen = sizeof(struct sockaddr_in6);

    /* loop across all the specified ports, establishing a socket
     * for each one - note that application procs will ONLY have
     * one socket, but that prun and daemons will have multiple
     * sockets to support more flexible wireup protocols
     */
    for (i = 0; i < PMIX_ARGV_COUNT_COMPAT(ports); i++) {
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "%s attempting to bind to IPv6 port %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ports[i]);
        /* get the port number */
        port = strtol(ports[i], NULL, 10);
        /* convert it to network-byte-order */
        port = htons(port);

        ((struct sockaddr_in6 *) &inaddr)->sin6_port = port;

        /* create a listen socket for incoming connections on this port */
        sd = socket(AF_INET6, SOCK_STREAM, 0);
        if (sd < 0) {
            if (EAFNOSUPPORT != prte_socket_errno) {
                pmix_output(0, "prte_mca_oob_tcp_component_init: socket() failed: %s (%d)",
                            strerror(prte_socket_errno), prte_socket_errno);
            }
            return PRTE_ERR_IN_ERRNO;
        }
        /* Set the socket to close-on-exec so that no children inherit
           this FD */
        if (pmix_fd_set_cloexec(sd) != PRTE_SUCCESS) {
            pmix_output(0,
                        "prte_oob_tcp_create_listen6: unable to set the "
                        "listening socket to CLOEXEC (%s:%d)\n",
                        strerror(prte_socket_errno), prte_socket_errno);
            CLOSE_THE_SOCKET(sd);
            PMIX_ARGV_FREE_COMPAT(ports);
            return PRTE_ERROR;
        }

        /* Enable/disable reusing ports */
        if (prte_static_ports) {
            flags = 1;
        } else {
            flags = 0;
        }
        if (setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, (const char *) &flags, sizeof(flags)) < 0) {
            pmix_output(0,
                        "prte_oob_tcp_create_listen: unable to set the "
                        "SO_REUSEADDR option (%s:%d)\n",
                        strerror(prte_socket_errno), prte_socket_errno);
            CLOSE_THE_SOCKET(sd);
            PMIX_ARGV_FREE_COMPAT(ports);
            return PRTE_ERROR;
        }

        if (bind(sd, (struct sockaddr *) &inaddr, addrlen) < 0) {
            if ((EADDRINUSE == prte_socket_errno) || (EADDRNOTAVAIL == prte_socket_errno)) {
                continue;
            }
            pmix_output(0, "%s bind() failed for port %d: %s (%d)",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (int) ntohs(port),
                        strerror(prte_socket_errno), prte_socket_errno);
            CLOSE_THE_SOCKET(sd);
            PMIX_ARGV_FREE_COMPAT(ports);
            return PRTE_ERROR;
        }
        /* resolve assigned port */
        if (getsockname(sd, (struct sockaddr *) &inaddr, &addrlen) < 0) {
            pmix_output(0, "prte_oob_tcp_create_listen: getsockname(): %s (%d)",
                        strerror(prte_socket_errno), prte_socket_errno);
            CLOSE_THE_SOCKET(sd);
            return PRTE_ERROR;
        }

        /* setup listen backlog to maximum allowed by kernel */
        if (listen(sd, SOMAXCONN) < 0) {
            pmix_output(0, "prte_mca_oob_tcp_component_init: listen(): %s (%d)",
                        strerror(prte_socket_errno), prte_socket_errno);
            return PRTE_ERROR;
        }

        /* set socket up to be non-blocking, otherwise accept could block */
        if ((flags = fcntl(sd, F_GETFL, 0)) < 0) {
            pmix_output(0, "prte_mca_oob_tcp_component_init: fcntl(F_GETFL) failed: %s (%d)",
                        strerror(prte_socket_errno), prte_socket_errno);
            return PRTE_ERROR;
        }
        flags |= O_NONBLOCK;
        if (fcntl(sd, F_SETFL, flags) < 0) {
            pmix_output(0, "prte_mca_oob_tcp_component_init: fcntl(F_SETFL) failed: %s (%d)",
                        strerror(prte_socket_errno), prte_socket_errno);
            return PRTE_ERROR;
        }

        /* add this port to our connections */
        conn = PMIX_NEW(prte_oob_tcp_listener_t);
        conn->tcp6 = true;
        conn->sd = sd;
        conn->port = ntohs(((struct sockaddr_in6 *) &inaddr)->sin6_port);
        pmix_list_append(&prte_mca_oob_tcp_component.listeners, &conn->item);
        /* and to our ports */
        pmix_asprintf(&tconn, "%d", ntohs(((struct sockaddr_in6 *) &inaddr)->sin6_port));
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prte_mca_oob_tcp_component.ipv6ports, tconn);
        free(tconn);
        if (OOB_TCP_DEBUG_CONNECT
            <= pmix_output_get_verbosity(prte_oob_base_framework.framework_output)) {
            pmix_output(0, "%s assigned IPv6 port %d", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        (int) ntohs(((struct sockaddr_in6 *) &inaddr)->sin6_port));
        }

        if (!PRTE_PROC_IS_MASTER) {
            /* only the HNP binds to multiple ports */
            break;
        }
    }
    if (0 == pmix_list_get_size(&prte_mca_oob_tcp_component.listeners)) {
        /* cleanup */
        CLOSE_THE_SOCKET(sd);
        PMIX_ARGV_FREE_COMPAT(ports);
        return PRTE_ERR_SOCKET_NOT_AVAILABLE;
    }

    /* done with this, so release it */
    PMIX_ARGV_FREE_COMPAT(ports);

    return PRTE_SUCCESS;
}
#endif

/*
 * The listen thread created when listen_mode is threaded.  Accepts
 * incoming connections and places them in a queue for further
 * processing
 *
 * Runs until prte_oob_tcp_compnent.shutdown is set to true.
 */
static void *listen_thread(pmix_object_t *obj)
{
    int rc, max, accepted_connections, sd;
    prte_socklen_t addrlen = sizeof(struct sockaddr_storage);
    prte_oob_tcp_pending_connection_t *pending_connection;
    struct timeval timeout;
    fd_set readfds;
    prte_oob_tcp_listener_t *listener;
    PRTE_HIDE_UNUSED_PARAMS(obj);

    /* only execute during the initial VM startup stage - once
     * all the initial daemons have reported in, we will revert
     * to the event method for handling any further connections
     * so as to minimize overhead
     */
    while (prte_mca_oob_tcp_component.listen_thread_active) {
        FD_ZERO(&readfds);
        max = -1;
        PMIX_LIST_FOREACH(listener, &prte_mca_oob_tcp_component.listeners, prte_oob_tcp_listener_t)
        {
            FD_SET(listener->sd, &readfds);
            max = (listener->sd > max) ? listener->sd : max;
        }
        /* add the stop_thread fd */
        FD_SET(prte_mca_oob_tcp_component.stop_thread[0], &readfds);
        max = (prte_mca_oob_tcp_component.stop_thread[0] > max) ? prte_mca_oob_tcp_component.stop_thread[0]
                                                            : max;

        /* set timeout interval */
        timeout.tv_sec = prte_mca_oob_tcp_component.listen_thread_tv.tv_sec;
        timeout.tv_usec = prte_mca_oob_tcp_component.listen_thread_tv.tv_usec;

        /* Block in a select to avoid hammering the cpu.  If a connection
         * comes in, we'll get woken up right away.
         */
        rc = select(max + 1, &readfds, NULL, NULL, &timeout);
        if (!prte_mca_oob_tcp_component.listen_thread_active) {
            /* we've been asked to terminate */
            return NULL;
        }
        if (rc < 0) {
            if (EAGAIN != prte_socket_errno && EINTR != prte_socket_errno) {
                perror("select");
            }
            continue;
        }

        /* Spin accepting connections until all active listen sockets
         * do not have any incoming connections, pushing each connection
         * onto the event queue for processing
         */
        do {
            accepted_connections = 0;
            PMIX_LIST_FOREACH(listener, &prte_mca_oob_tcp_component.listeners, prte_oob_tcp_listener_t)
            {
                sd = listener->sd;

                /* according to the man pages, select replaces the given descriptor
                 * set with a subset consisting of those descriptors that are ready
                 * for the specified operation - in this case, a read. So we need to
                 * first check to see if this file descriptor is included in the
                 * returned subset
                 */
                if (0 == FD_ISSET(sd, &readfds)) {
                    /* this descriptor is not included */
                    continue;
                }

                /* this descriptor is ready to be read, which means a connection
                 * request has been received - so harvest it. All we want to do
                 * here is accept the connection and push the info onto the event
                 * library for subsequent processing - we don't want to actually
                 * process the connection here as it takes too long, and so the
                 * OS might start rejecting connections due to timeout.
                 */
                pending_connection = PMIX_NEW(prte_oob_tcp_pending_connection_t);
                prte_event_set(prte_event_base, &pending_connection->ev, -1, PRTE_EV_WRITE,
                               connection_handler, pending_connection);
                pending_connection->fd = accept(sd, (struct sockaddr *) &(pending_connection->addr),
                                                &addrlen);

                /* check for < 0 as indicating an error upon accept */
                if (pending_connection->fd < 0) {
                    PMIX_RELEASE(pending_connection);

                    /* Non-fatal errors */
                    if (EAGAIN == prte_socket_errno || EWOULDBLOCK == prte_socket_errno) {
                        continue;
                    }

                    /* If we run out of file descriptors, log an extra
                       warning (so that the user can know to fix this
                       problem) and abandon all hope. */
                    else if (EMFILE == prte_socket_errno) {
                        CLOSE_THE_SOCKET(sd);
                        PRTE_ERROR_LOG(PRTE_ERR_SYS_LIMITS_SOCKETS);
                        pmix_show_help("help-oob-tcp.txt", "accept failed", true,
                                       prte_process_info.nodename, prte_socket_errno,
                                       strerror(prte_socket_errno), "Out of file descriptors");
                        goto done;
                    }

                    /* For all other cases, print a
                       warning but try to continue */
                    else {
                        pmix_show_help("help-oob-tcp.txt", "accept failed", true,
                                       prte_process_info.nodename, prte_socket_errno,
                                       strerror(prte_socket_errno),
                                       "Unknown cause; job will try to continue");
                        continue;
                    }
                }

                pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                                    "%s prte_oob_tcp_listen_thread: incoming connection: "
                                    "(%d, %d) %s:%d\n",
                                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), pending_connection->fd,
                                    prte_socket_errno,
                                    pmix_net_get_hostname(
                                        (struct sockaddr *) &pending_connection->addr),
                                    pmix_net_get_port(
                                        (struct sockaddr *) &pending_connection->addr));

                /* if we are on a privileged port, we only accept connections
                 * from other privileged sockets. A privileged port is one
                 * whose port is less than 1024 on Linux, so we'll check for that. */
                if (1024 >= listener->port) {
                    uint16_t inport;
                    inport = pmix_net_get_port((struct sockaddr *) &pending_connection->addr);
                    if (1024 < inport) {
                        /* someone tried to cross-connect privileges,
                         * say something */
                        pmix_show_help("help-oob-tcp.txt", "privilege failure", true,
                                       prte_process_info.nodename, listener->port,
                                       pmix_net_get_hostname(
                                           (struct sockaddr *) &pending_connection->addr),
                                       inport);
                        CLOSE_THE_SOCKET(pending_connection->fd);
                        PMIX_RELEASE(pending_connection);
                        continue;
                    }
                }

                /* activate the event */
                PMIX_POST_OBJECT(pending_connection);
                prte_event_active(&pending_connection->ev, PRTE_EV_WRITE, 1);
                accepted_connections++;
            }
        } while (accepted_connections > 0);
    }

done:
#if 0
    /* once we complete the initial launch, the "flood" of connections
     * will end - only connection requests from local procs, connect/accept
     * operations across mpirun instances, or the occasional tool will need
     * to be serviced. As these are relatively small events, we can easily
     * handle them in the context of the event library and no longer require
     * a separate connection harvesting thread. So switch over to the event
     * lib handler now
     */
    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s prte_oob_tcp_listen_thread: switching to event lib",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
    /* setup to listen via event library */
    PMIX_LIST_FOREACH(listener, &prte_mca_oob_tcp_component.listeners, prte_oob_tcp_listener_t) {
        prte_event_set(prte_event_base, listener->event,
                   listener->sd,
                   PRTE_EV_READ|PRTE_EV_PERSIST,
                   connection_event_handler,
                   0);
        prte_event_add(listener->event, 0);
    }
#endif
    return NULL;
}

/*
 * Handler for accepting connections from the listen thread
 */
static void connection_handler(int sd, short flags, void *cbdata)
{
    prte_oob_tcp_pending_connection_t *new_connection;
    PRTE_HIDE_UNUSED_PARAMS(sd, flags);

    new_connection = (prte_oob_tcp_pending_connection_t *) cbdata;

    PMIX_ACQUIRE_OBJECT(new_connection);

    pmix_output_verbose(4, prte_oob_base_framework.framework_output,
                        "%s connection_handler: working connection "
                        "(%d, %d) %s:%d\n",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), new_connection->fd, prte_socket_errno,
                        pmix_net_get_hostname((struct sockaddr *) &new_connection->addr),
                        pmix_net_get_port((struct sockaddr *) &new_connection->addr));

    /* process the connection */
    prte_oob_tcp_module.accept_connection(new_connection->fd,
                                          (struct sockaddr *) &(new_connection->addr));
    /* cleanup */
    PMIX_RELEASE(new_connection);
}

/*
 * Handler for accepting connections from the event library
 */
static void connection_event_handler(int incoming_sd, short flags, void *cbdata)
{
    struct sockaddr addr;
    prte_socklen_t addrlen = sizeof(struct sockaddr);
    int sd;
    PRTE_HIDE_UNUSED_PARAMS(flags, cbdata);

    sd = accept(incoming_sd, (struct sockaddr *) &addr, &addrlen);
    pmix_output_verbose(OOB_TCP_DEBUG_CONNECT, prte_oob_base_framework.framework_output,
                        "%s connection_event_handler: working connection "
                        "(%d, %d) %s:%d\n",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), sd, prte_socket_errno,
                        pmix_net_get_hostname((struct sockaddr *) &addr),
                        pmix_net_get_port((struct sockaddr *) &addr));
    if (sd < 0) {
        /* Non-fatal errors */
        if (EINTR == prte_socket_errno || EAGAIN == prte_socket_errno
            || EWOULDBLOCK == prte_socket_errno) {
            return;
        }

        /* If we run out of file descriptors, log an extra warning (so
           that the user can know to fix this problem) and abandon all
           hope. */
        else if (EMFILE == prte_socket_errno) {
            CLOSE_THE_SOCKET(incoming_sd);
            PRTE_ERROR_LOG(PRTE_ERR_SYS_LIMITS_SOCKETS);
            pmix_show_help("help-oob-tcp.txt", "accept failed", true, prte_process_info.nodename,
                           prte_socket_errno, strerror(prte_socket_errno),
                           "Out of file descriptors");
            return;
        }

        /* For all other cases, close the socket, print a warning but
           try to continue */
        else {
            CLOSE_THE_SOCKET(incoming_sd);
            pmix_show_help("help-oob-tcp.txt", "accept failed", true, prte_process_info.nodename,
                           prte_socket_errno, strerror(prte_socket_errno),
                           "Unknown cause; job will try to continue");
            return;
        }
    }

    /* process the connection */
    prte_oob_tcp_module.accept_connection(sd, &addr);
}

static void tcp_ev_cons(prte_oob_tcp_listener_t *event)
{
    event->ev_active = false;
    event->tcp6 = false;
    event->sd = -1;
    event->port = 0;
}
static void tcp_ev_des(prte_oob_tcp_listener_t *event)
{
    if (event->ev_active) {
        prte_event_del(&event->event);
    }
    event->ev_active = false;
    if (0 <= event->sd) {
        CLOSE_THE_SOCKET(event->sd);
        event->sd = -1;
    }
}

PMIX_CLASS_INSTANCE(prte_oob_tcp_listener_t, pmix_list_item_t, tcp_ev_cons, tcp_ev_des);

PMIX_CLASS_INSTANCE(prte_oob_tcp_pending_connection_t, pmix_object_t, NULL, NULL);
