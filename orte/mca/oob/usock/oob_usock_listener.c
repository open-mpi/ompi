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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "opal/util/fd.h"
#include "opal/util/if.h"
#include "opal/util/net.h"
#include "opal/util/argv.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/util/name_fns.h"
#include "orte/util/parse_options.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/oob/usock/oob_usock.h"
#include "orte/mca/oob/usock/oob_usock_component.h"
#include "orte/mca/oob/usock/oob_usock_peer.h"
#include "orte/mca/oob/usock/oob_usock_connection.h"
#include "orte/mca/oob/usock/oob_usock_listener.h"

static void connection_event_handler(int incoming_sd, short flags, void* cbdata);

/*
 * start listening on our rendezvous file
 */
int orte_oob_usock_start_listening(void)
{
    int flags;
    opal_socklen_t addrlen;
    int sd = -1;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s START USOCK LISTENING ON %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        mca_oob_usock_component.address.sun_path);

    /* create a listen socket for incoming connection attempts */
    sd = socket(PF_UNIX, SOCK_STREAM, 0);
    if (sd < 0) {
        if (EAFNOSUPPORT != opal_socket_errno) {
            opal_output(0,"mca_oob_usock_start_listening: socket() failed: %s (%d)", 
                        strerror(opal_socket_errno), opal_socket_errno);
        }
        return ORTE_ERR_IN_ERRNO;
    }
    /* Set this fd to be close-on-exec so that children don't see it */
    if (opal_fd_set_cloexec(sd) != OPAL_SUCCESS) {
        opal_output(0, "%s unable to set socket to CLOEXEC",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        CLOSE_THE_SOCKET(sd);
        return ORTE_ERROR;
    }

    addrlen = sizeof(struct sockaddr_un);
    if (bind(sd, (struct sockaddr*)&mca_oob_usock_component.address, addrlen) < 0) {
        opal_output(0, "%s bind() failed on error %s (%d)",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    strerror(opal_socket_errno),
                    opal_socket_errno );
        CLOSE_THE_SOCKET(sd);
        return ORTE_ERROR;
    }
        
    /* setup listen backlog to maximum allowed by kernel */
    if (listen(sd, SOMAXCONN) < 0) {
        opal_output(0, "mca_oob_usock_component_init: listen(): %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        CLOSE_THE_SOCKET(sd);
        return ORTE_ERROR;
    }
        
    /* set socket up to be non-blocking, otherwise accept could block */
    if ((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        opal_output(0, "mca_oob_usock_component_init: fcntl(F_GETFL) failed: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        CLOSE_THE_SOCKET(sd);
        return ORTE_ERROR;
    }
    flags |= O_NONBLOCK;
    if (fcntl(sd, F_SETFL, flags) < 0) {
        opal_output(0, "mca_oob_usock_component_init: fcntl(F_SETFL) failed: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        CLOSE_THE_SOCKET(sd);
        return ORTE_ERROR;
    }

    /* record this socket */
    mca_oob_usock_component.listener_socket = sd;

    /* setup to listen via the event lib */
    mca_oob_usock_component.listener_ev_active = true;
    opal_event_set(orte_event_base, &mca_oob_usock_component.listener_event,
                   mca_oob_usock_component.listener_socket,
                   OPAL_EV_READ|OPAL_EV_PERSIST,
                   connection_event_handler,
                   0);
    opal_event_set_priority(&mca_oob_usock_component.listener_event, ORTE_MSG_PRI);
    opal_event_add(&mca_oob_usock_component.listener_event, 0);

    return ORTE_SUCCESS;
}

/*
 * Handler for accepting connections from the event library
 */
static void connection_event_handler(int incoming_sd, short flags, void* cbdata)
{
    struct sockaddr addr;
    opal_socklen_t addrlen = sizeof(struct sockaddr);
    int sd;

    sd = accept(incoming_sd, (struct sockaddr*)&addr, &addrlen);
    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s connection_event_handler: working connection "
                        "(%d, %d)\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        sd, opal_socket_errno);
    if (sd < 0) {
        if (EINTR == opal_socket_errno) {
            return;
        }
        if (opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
            if (EMFILE == opal_socket_errno) {
                /*
                 * Close incoming_sd so that orte_show_help will have a file
                 * descriptor with which to open the help file.  We will be
                 * exiting anyway, so we don't need to keep it open.
                 */
                CLOSE_THE_SOCKET(incoming_sd);
                ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_SOCKETS);
                orte_show_help("help-orterun.txt", "orterun:sys-limit-sockets", true);
            } else {
                opal_output(0, "mca_oob_usock_accept: accept() failed: %s (%d).", 
                            strerror(opal_socket_errno), opal_socket_errno);
            }
            orte_errmgr.abort(ORTE_ERROR_DEFAULT_EXIT_CODE, NULL);
        }
        return;
    }

    /* process the connection */
    mca_oob_usock_module.api.accept_connection(sd, &addr);
}
