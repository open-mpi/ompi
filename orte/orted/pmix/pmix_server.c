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

#include "opal_stdint.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/util/opal_environ.h"
#include "opal/util/show_help.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/opal_socket_errno.h"
#include "opal/util/if.h"
#include "opal/util/net.h"
#include "opal/util/argv.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/dstore/dstore.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "pmix_server.h"
#include "pmix_server_internal.h"

/*
 * Local utility functions
 */
static void connection_handler(int incoming_sd, short flags, void* cbdata);
static int pmix_server_start_listening(struct sockaddr_un *address);

char *pmix_server_mode = NULL;
bool pmix_server_distribute_data = false;
opal_hash_table_t *pmix_server_peers = NULL;
int pmix_server_verbosity = -1;
int pmix_server_output = -1;
int pmix_server_handle = -1;
static bool initialized = false;
static struct sockaddr_un address;
static int pmix_server_listener_socket = -1;
static bool pmix_server_listener_ev_active = false;
static opal_event_t pmix_server_listener_event;

void pmix_server_register(void)
{
    /* register a variable to control how we distribute and/or
     * retrieve data pushed to PMIX */
    pmix_server_mode = "direct";
    (void) mca_base_var_register ("orte", "pmix", NULL, "server_mode",
                                  "Operating mode for PMIx server: direct=obtain info upon demand[default], group=distribute via collective at startup",
                                  MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL,
                                  &pmix_server_mode);

    /* register a verbosity */
    pmix_server_verbosity = -1;
    (void) mca_base_var_register ("orte", "pmix", NULL, "server_verbose",
                                  "Debug verbosity for PMIx server",
                                  MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL,
                                  &pmix_server_verbosity);
    if (0 <= pmix_server_verbosity) {
        pmix_server_output = opal_output_open(NULL);
        opal_output_set_verbosity(pmix_server_output, pmix_server_verbosity);
    }
}

/*
 * Initialize global variables used w/in the server.
 */
int pmix_server_init(void)
{
    int rc;
    char *uri;

    if (initialized) {
        return ORTE_SUCCESS;
    }
    initialized = true;

    /* setup the servers's state variables */
    pmix_server_peers = OBJ_NEW(opal_hash_table_t);
    opal_hash_table_init(pmix_server_peers, 32);

    if (NULL != pmix_server_mode && 0 == (strcmp(pmix_server_mode, "group"))) {
        pmix_server_distribute_data = true;
    }

    /* if the session directory has not already been setup, do so */
    if (NULL == orte_process_info.top_session_dir) {
        if (ORTE_SUCCESS != (rc = orte_session_dir(true,
                                                   orte_process_info.tmpdir_base,
                                                   orte_process_info.nodename, NULL,
                                                   ORTE_PROC_MY_NAME))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    /* setup the path to the rendezvous point */
    memset(&address, 0, sizeof(struct sockaddr_un));
    address.sun_family = AF_UNIX;
    snprintf(address.sun_path, sizeof(address.sun_path)-1,
             "%s/%s/%s/0/%s", orte_process_info.tmpdir_base,
             orte_process_info.top_session_dir,
             ORTE_JOB_FAMILY_PRINT(ORTE_PROC_MY_NAME->jobid), "pmix");

    /* add it to our launch environment so our children get it */
    (void)asprintf(&uri, "%"PRIu64":%s", *(opal_identifier_t*)&orte_process_info.my_name, address.sun_path);
    opal_output_verbose(2, pmix_server_output,
                        "%s PMIX server uri: %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), uri);
    opal_setenv("PMIX_SERVER_URI", uri, true, &orte_launch_environ);
    free(uri);

    /* setup the datastore handle */
    if (0 > (pmix_server_handle = opal_dstore.open("pmix"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* start listening for connection requests */
    if (ORTE_SUCCESS != (rc = pmix_server_start_listening(&address))) {
        OBJ_DESTRUCT(&pmix_server_peers);
    }

    return rc;
}

void pmix_server_finalize(void)
{
    uint64_t ui64;
    pmix_server_peer_t *pr;
    char *nptr;

    if (!initialized) {
        return;
    }

    opal_output_verbose(2, pmix_server_output,
                        "%s Finalizing PMIX server",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* stop listening */
    if (pmix_server_listener_ev_active) {
        opal_event_del(&pmix_server_listener_event);
    }

    /* delete the rendezvous file */
    unlink(address.sun_path);

    /* cleanup all peers */
    if (OPAL_SUCCESS == opal_hash_table_get_first_key_uint64(pmix_server_peers, &ui64,
                                                             (void**)&pr, (void**)&nptr)) {
        opal_output_verbose(2, pmix_server_output,
                            "%s RELEASING PEER OBJ %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (NULL == pr) ? "NULL" : ORTE_NAME_PRINT(&pr->name));
        if (NULL != pr) {
            OBJ_RELEASE(pr);
        }
        while (OPAL_SUCCESS == opal_hash_table_get_next_key_uint64(pmix_server_peers, &ui64,
                                                                   (void**)&pr, nptr, (void**)&nptr)) {
            opal_output_verbose(2, pmix_server_output,
                                "%s RELEASING PEER OBJ %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (NULL == pr) ? "NULL" : ORTE_NAME_PRINT(&pr->name));
            if (NULL != pr) {
                OBJ_RELEASE(pr);
            }
        }
    }
    OBJ_RELEASE(pmix_server_peers);
}

/*
 * start listening on our rendezvous file
 */
static int pmix_server_start_listening(struct sockaddr_un *address)
{
    int flags;
    opal_socklen_t addrlen;
    int sd = -1;

    /* create a listen socket for incoming connection attempts */
    sd = socket(PF_UNIX, SOCK_STREAM, 0);
    if (sd < 0) {
        if (EAFNOSUPPORT != opal_socket_errno) {
            opal_output(0,"pmix_server_start_listening: socket() failed: %s (%d)", 
                        strerror(opal_socket_errno), opal_socket_errno);
        }
        return ORTE_ERR_IN_ERRNO;
    }

    addrlen = sizeof(struct sockaddr_un);
    if (bind(sd, (struct sockaddr*)address, addrlen) < 0) {
        opal_output(0, "%s bind() failed on error %s (%d)",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    strerror(opal_socket_errno),
                    opal_socket_errno );
        CLOSE_THE_SOCKET(sd);
        return ORTE_ERROR;
    }
        
    /* setup listen backlog to maximum allowed by kernel */
    if (listen(sd, SOMAXCONN) < 0) {
        opal_output(0, "pmix_server_component_init: listen(): %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    }
        
    /* set socket up to be non-blocking, otherwise accept could block */
    if ((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        opal_output(0, "pmix_server_component_init: fcntl(F_GETFL) failed: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    }
    flags |= O_NONBLOCK;
    if (fcntl(sd, F_SETFL, flags) < 0) {
        opal_output(0, "pmix_server_component_init: fcntl(F_SETFL) failed: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    }

    /* record this socket */
    pmix_server_listener_socket = sd;

    /* setup to listen via the event lib */
    pmix_server_listener_ev_active = true;
    opal_event_set(orte_event_base, &pmix_server_listener_event,
                   pmix_server_listener_socket,
                   OPAL_EV_READ|OPAL_EV_PERSIST,
                   connection_handler,
                   0);
    opal_event_set_priority(&pmix_server_listener_event, ORTE_MSG_PRI);
    opal_event_add(&pmix_server_listener_event, 0);

    opal_output_verbose(2, pmix_server_output,
                        "%s pmix server listening on socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), sd);

    return ORTE_SUCCESS;
}

/*
 * Handler for accepting connections from the event library
 */
static void connection_handler(int incoming_sd, short flags, void* cbdata)
{
    struct sockaddr addr;
    opal_socklen_t addrlen = sizeof(struct sockaddr);
    int sd, rc;
    pmix_server_hdr_t hdr;
    pmix_server_peer_t *peer;
    uint64_t *ui64;
    orte_process_name_t sender;

    sd = accept(incoming_sd, (struct sockaddr*)&addr, &addrlen);
    opal_output_verbose(2, pmix_server_output,
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
                opal_output(0, "pmix_server_accept: accept() failed: %s (%d).", 
                            strerror(opal_socket_errno), opal_socket_errno);
            }
        }
        return;
    }

    /* get the handshake */
    if (ORTE_SUCCESS != (rc = pmix_server_recv_connect_ack(NULL, sd, &hdr))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* finish processing ident */
    if (PMIX_USOCK_IDENT == hdr.type) {
        /* get the name */
        memcpy(&sender, &hdr.id, sizeof(opal_identifier_t));
        if (NULL == (peer = pmix_server_peer_lookup(&sender))) {
            /* should never happen */
            peer->state = PMIX_SERVER_CLOSED;
            CLOSE_THE_SOCKET(sd);
            return;
        }
        /* set socket up to be non-blocking */
        if ((flags = fcntl(sd, F_GETFL, 0)) < 0) {
            opal_output(0, "%s pmix_server_recv_connect: fcntl(F_GETFL) failed: %s (%d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), strerror(opal_socket_errno), opal_socket_errno);
        } else {
            flags |= O_NONBLOCK;
            if (fcntl(sd, F_SETFL, flags) < 0) {
                opal_output(0, "%s pmix_server_recv_connect: fcntl(F_SETFL) failed: %s (%d)",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), strerror(opal_socket_errno), opal_socket_errno);
            }
        }

        /* is the peer instance willing to accept this connection */
        peer->sd = sd;
        if (peer->state != PMIX_SERVER_CONNECTED) {
            pmix_server_peer_event_init(peer);

            if (pmix_server_send_connect_ack(peer) != ORTE_SUCCESS) {
                opal_output(0, "%s usock_peer_accept: "
                            "usock_peer_send_connect_ack to %s failed\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)));
                peer->state = PMIX_SERVER_FAILED;
                CLOSE_THE_SOCKET(sd);
                return;
            }

            pmix_server_peer_connected(peer);
            if (2 <= opal_output_get_verbosity(pmix_server_output)) {
                pmix_server_peer_dump(peer, "accepted");
            }
        } else {
            opal_output_verbose(2, pmix_server_output,
                        "%s usock:peer_accept ignored for peer %s in state %s on socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name),
                        pmix_server_state_print(peer->state), peer->sd);
            CLOSE_THE_SOCKET(sd);
            ui64 = (uint64_t*)(&peer->name);
            opal_hash_table_set_value_uint64(pmix_server_peers, (*ui64), NULL);
            OBJ_RELEASE(peer);
        }
    }
}

char* pmix_server_state_print(pmix_server_state_t state)
{
    switch (state) {
    case PMIX_SERVER_UNCONNECTED:
        return "UNCONNECTED";
    case PMIX_SERVER_CLOSED:
        return "CLOSED";
    case PMIX_SERVER_RESOLVE:
        return "RESOLVE";
    case PMIX_SERVER_CONNECTING:
        return "CONNECTING";
    case PMIX_SERVER_CONNECT_ACK:
        return "ACK";
    case PMIX_SERVER_CONNECTED:
        return "CONNECTED";
    case PMIX_SERVER_FAILED:
        return "FAILED";
    default:
        return "UNKNOWN";
    }
}


pmix_server_peer_t* pmix_server_peer_lookup(const orte_process_name_t *name)
{
    pmix_server_peer_t *peer;
    uint64_t ui64;

    memcpy(&ui64, (char*)name, sizeof(uint64_t));
    if (OPAL_SUCCESS != opal_hash_table_get_value_uint64(pmix_server_peers, ui64, (void**)&peer)) {
        return NULL;
    }
    return peer;
}

/*
 * Routine for debugging to print the connection state and socket options
 */
void pmix_server_peer_dump(pmix_server_peer_t* peer, const char* msg)
{
    char buff[255];
    int nodelay,flags;

    if ((flags = fcntl(peer->sd, F_GETFL, 0)) < 0) {
        opal_output(0, "usock_peer_dump: fcntl(F_GETFL) failed: %s (%d)\n",
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
                                                                                                            
#if defined(USOCK_NODELAY)
    optlen = sizeof(nodelay);
    if (getsockopt(peer->sd, IPPROTO_USOCK, USOCK_NODELAY, (char *)&nodelay, &optlen) < 0) {
        opal_output(0, "usock_peer_dump: USOCK_NODELAY option: %s (%d)\n", 
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
#else
    nodelay = 0;
#endif

    snprintf(buff, sizeof(buff), "%s-%s %s: nodelay %d flags %08x\n",
        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
        ORTE_NAME_PRINT(&(peer->name)),
        msg, nodelay, flags);
    opal_output(0, "%s", buff);
}

static void scon(pmix_server_send_t *p)
{
    p->data = NULL;
    p->hdr_sent = false;
    p->sdptr = NULL;
    p->sdbytes = 0;
}
static void dcon(pmix_server_send_t *p)
{
    if (NULL != p->data) {
        free(p->data);
    }
}
OBJ_CLASS_INSTANCE(pmix_server_send_t,
                   opal_list_item_t,
                   scon, dcon);

static void rcon(pmix_server_recv_t *p)
{
    p->data = NULL;
    p->hdr_recvd = false;
    p->rdptr = NULL;
    p->rdbytes = 0;
}
static void rdes(pmix_server_recv_t *p)
{
    if (NULL != p->data) {
        free(p->data);
    }
}
OBJ_CLASS_INSTANCE(pmix_server_recv_t,
                   opal_list_item_t,
                   rcon, rdes);

static void pcon(pmix_server_peer_t *p)
{
    p->sd = -1;
    p->retries = 0;
    p->state = PMIX_SERVER_UNCONNECTED;
    p->send_ev_active = false;
    p->recv_ev_active = false;
    p->timer_ev_active = false;
    OBJ_CONSTRUCT(&p->send_queue, opal_list_t);
    p->send_msg = NULL;
    p->recv_msg = NULL;
}
static void pdes(pmix_server_peer_t *p)
{
    OPAL_LIST_DESTRUCT(&p->send_queue);
}
OBJ_CLASS_INSTANCE(pmix_server_peer_t,
                   opal_object_t,
                   pcon, pdes);
