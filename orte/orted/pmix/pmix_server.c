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
#include "opal/class/opal_list.h"
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
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "pmix_server.h"
#include "pmix_server_internal.h"

/* local classes for tracking collective ops */
typedef struct {
    opal_list_item_t super;
    int sd;
    orte_process_name_t name;
    uint32_t tag;
} pmix_server_local_t;
OBJ_CLASS_INSTANCE(pmix_server_local_t,
                   opal_list_item_t,
                   NULL, NULL);

typedef struct {
    opal_list_item_t super;
    orte_grpcomm_signature_t *sig;
    orte_vpid_t nlocal;           // number of local procs in this collective
    opal_list_t locals;           // list of pmix_server_local_t
    opal_buffer_t bucket;
} pmix_server_trk_t;
static void trkcon(pmix_server_trk_t *p)
{
    p->sig = NULL;
    p->nlocal = 0;
    OBJ_CONSTRUCT(&p->locals, opal_list_t);
    OBJ_CONSTRUCT(&p->bucket, opal_buffer_t);
}
static void trkdes(pmix_server_trk_t *p)
{
    if (NULL != p->sig) {
        OBJ_RELEASE(p->sig);
    }
    OPAL_LIST_DESTRUCT(&p->locals);
    OBJ_DESTRUCT(&p->bucket);
}
static OBJ_CLASS_INSTANCE(pmix_server_trk_t,
                          opal_list_item_t,
                          trkcon, trkdes);

/*
 * Local utility functions
 */
static void connection_handler(int incoming_sd, short flags, void* cbdata);
static int pmix_server_start_listening(struct sockaddr_un *address);
static void pmix_server_recv(int status, orte_process_name_t* sender,
                             opal_buffer_t *buffer,
                             orte_rml_tag_t tg, void *cbdata);
static void pmix_server_release(int status,
                                opal_buffer_t *buffer,
                                void *cbdata);
static void pmix_server_dmdx_recv(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata);
static void pmix_server_dmdx_resp(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata);

char *pmix_server_uri = NULL;
opal_hash_table_t *pmix_server_peers = NULL;
int pmix_server_verbosity = -1;
int pmix_server_output = -1;
int pmix_server_local_handle = -1;
int pmix_server_remote_handle = -1;
int pmix_server_global_handle = -1;
opal_list_t pmix_server_pending_dmx_reqs;
static bool initialized = false;
static struct sockaddr_un address;
static int pmix_server_listener_socket = -1;
static bool pmix_server_listener_ev_active = false;
static opal_event_t pmix_server_listener_event;
static opal_list_t collectives;

void pmix_server_register(void)
{
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

    if (initialized) {
        return ORTE_SUCCESS;
    }
    initialized = true;

    /* setup the servers's state variables */
    pmix_server_peers = OBJ_NEW(opal_hash_table_t);
    opal_hash_table_init(pmix_server_peers, 32);
    OBJ_CONSTRUCT(&collectives, opal_list_t);
    OBJ_CONSTRUCT(&pmix_server_pending_dmx_reqs, opal_list_t);

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
    (void)asprintf(&pmix_server_uri, "%"PRIu64":%s", *(opal_identifier_t*)&orte_process_info.my_name, address.sun_path);
    opal_output_verbose(2, pmix_server_output,
                        "%s PMIX server uri: %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), pmix_server_uri);
    opal_setenv("PMIX_SERVER_URI", pmix_server_uri, true, &orte_launch_environ);

    /* setup the datastore handles */
    if (0 > (pmix_server_local_handle = opal_dstore.open("pmix-local"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (0 > (pmix_server_remote_handle = opal_dstore.open("pmix-remote"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (0 > (pmix_server_global_handle = opal_dstore.open("pmix-global"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* setup recv for collecting local barriers */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON_COLL,
                            ORTE_RML_PERSISTENT, pmix_server_recv, NULL);

    /* setup recv for direct modex requests */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DIRECT_MODEX,
                            ORTE_RML_PERSISTENT, pmix_server_dmdx_recv, NULL);

    /* setup recv for replies to direct modex requests */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DIRECT_MODEX_RESP,
                            ORTE_RML_PERSISTENT, pmix_server_dmdx_resp, NULL);

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
    /* stop receives */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON_COLL);
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_COLL_RELEASE);
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DIRECT_MODEX);
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DIRECT_MODEX_RESP);

    /* cleanup the dstore handles */
    (void)opal_dstore.close(pmix_server_local_handle);
    (void)opal_dstore.close(pmix_server_remote_handle);
    (void)opal_dstore.close(pmix_server_global_handle);

    /* delete the rendezvous file */
    unlink(address.sun_path);
    if (NULL != pmix_server_uri) {
        free(pmix_server_uri);
    }

    /* cleanup collectives */
    OPAL_LIST_DESTRUCT(&collectives);
    OPAL_LIST_DESTRUCT(&pmix_server_pending_dmx_reqs);

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
        /* get the peer */
        if (NULL == (peer = pmix_server_peer_lookup(sd))) {
            /* create the peer */
            peer = OBJ_NEW(pmix_server_peer_t);
            if (OPAL_SUCCESS != (rc = opal_hash_table_set_value_uint64(pmix_server_peers, sd, peer))) {
                OPAL_ERROR_LOG(rc);
                return;
            }
            memcpy(&peer->name, &hdr.id, sizeof(opal_identifier_t));
            peer->state = PMIX_SERVER_ACCEPTING;
            peer->sd = sd;
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
            opal_hash_table_set_value_uint64(pmix_server_peers, sd, NULL);
            CLOSE_THE_SOCKET(sd);
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


pmix_server_peer_t* pmix_server_peer_lookup(int sd)
{
    pmix_server_peer_t *peer;
    uint64_t ui64;

    ui64 = sd;
    if (OPAL_SUCCESS != opal_hash_table_get_value_uint64(pmix_server_peers, ui64, (void**)&peer)) {
        return NULL;
    }
    return peer;
}


static pmix_server_trk_t* get_trk(opal_identifier_t *id,
                                  orte_grpcomm_signature_t *sig)
{
    pmix_server_trk_t *trk;
    orte_job_t *jdata;
    orte_proc_t *proc;
    size_t i;
    orte_process_name_t name;

    /* deal with a null signature here */
    if (NULL == sig->signature) {
        memcpy(&name, id, sizeof(orte_process_name_t));
        /* create a signature indicating that all procs in this one's
         * jobid are participating */
        sig->signature = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
        name.vpid = ORTE_VPID_WILDCARD;
        memcpy(sig->signature, &name, sizeof(orte_process_name_t));
        sig->sz = 1;
    }

    OPAL_LIST_FOREACH(trk, &collectives, pmix_server_trk_t) {
        if (OPAL_EQUAL == opal_dss.compare(sig, trk->sig, ORTE_SIGNATURE)) {
            /* got it */
            opal_output_verbose(2, pmix_server_output,
                                "%s pmix:server found tracker for signature",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            return trk;
        }
    }

    opal_output_verbose(2, pmix_server_output,
                        "%s pmix:server adding new tracker of sig size %u",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (unsigned int)sig->sz);

    /* get here if tracker not found */
    trk = OBJ_NEW(pmix_server_trk_t);
    OBJ_RETAIN(sig);
    trk->sig = sig;
    opal_list_append(&collectives, &trk->super);

    /* if this is a job-wide event, then deal with it here */
    if (1 == sig->sz) {
        memcpy(&name, &sig->signature[0], sizeof(orte_process_name_t));
        /* get the job object */
        if (NULL == (jdata = orte_get_job_data_object(name.jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return NULL;
        }
        if (ORTE_VPID_WILDCARD == name.vpid) {
            trk->nlocal = jdata->num_local_procs;
            goto done;
        }
    }

    /* count how many of these procs are local to us */
    for (i=0; i < sig->sz; i++) {
        memcpy(&name, &sig->signature[i], sizeof(orte_process_name_t));
        /* get the job object */
        if (NULL == (jdata = orte_get_job_data_object(name.jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            continue;
        }
        /* get the proc object */
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, name.vpid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            continue;
        }
        if (ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_LOCAL)) {
            trk->nlocal++;
        }
    }

 done:
    opal_output_verbose(2, pmix_server_output,
                        "%s pmix:server tracker is looking for %s local procs",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_VPID_PRINT(trk->nlocal));
    return trk;
}
        
static void pmix_server_recv(int status, orte_process_name_t* sender,
                             opal_buffer_t *buffer,
                             orte_rml_tag_t tg, void *cbdata)
{
    orte_process_name_t name;
    int rc;
    int32_t cnt;
    opal_identifier_t id;
    pmix_server_trk_t *trk;
    pmix_server_local_t *lcl;
    uint32_t tag;
    int32_t sd;
    orte_grpcomm_signature_t *sig;

    opal_output_verbose(2, pmix_server_output,
                        "%s pmix:server:recv msg recvd from %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(sender));

    /* unpack the id of the proc involved - must be one
     * of my local children */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &id, &cnt, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    memcpy(&name, &id, sizeof(orte_process_name_t));

    /* unpack the socket and tag the proc is listening on */
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &sd, &cnt, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &tag, &cnt, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the signature */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &sig, &cnt, ORTE_SIGNATURE))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* check for the tracker and create it if not found */
    if (NULL == (trk = get_trk(&id, sig))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OBJ_RELEASE(sig);
        return;
    }

    /* add any data that was included from the local proc and
     * needs to be in the allgather */
    opal_dss.copy_payload(&trk->bucket, buffer);

    /* I would only have received this if one of my local children
     * is participating, so add them to the tracker so we know how
     * to send a response back to them when the collective is complete */
    lcl = OBJ_NEW(pmix_server_local_t);
    lcl->sd = sd;
    lcl->name = name;
    lcl->tag = tag;
    opal_list_append(&trk->locals, &lcl->super);

    opal_output_verbose(2, pmix_server_output,
                        "%s pmix:server %d reported for job %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (int)opal_list_get_size(&trk->locals),
                        ORTE_JOBID_PRINT(name.jobid));

    /* if locally complete, pass it to the allgather */
    if (trk->nlocal == opal_list_get_size(&trk->locals)) {
        /* pass along any data that was collected locally */
        if (ORTE_SUCCESS != (rc = orte_grpcomm.allgather(sig, &trk->bucket,
                                                         pmix_server_release, trk))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    /* the tracker will have retained any data it needed, so get rid
     * of our copy */
    OBJ_RELEASE(sig);
}

static void pmix_server_release(int status,
                                opal_buffer_t *buffer,
                                void *cbdata)
{
    pmix_server_trk_t *trk = (pmix_server_trk_t*)cbdata;
    pmix_server_local_t *lcl;
    pmix_server_peer_t *peer;
    opal_buffer_t *reply;

    opal_output_verbose(2, pmix_server_output,
                        "%s pmix:server:release coll release recvd",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* for each local process, send the data */
    reply = OBJ_NEW(opal_buffer_t);
    opal_dss.copy_payload(reply, buffer);
    OPAL_LIST_FOREACH(lcl, &trk->locals, pmix_server_local_t) {
        OBJ_RETAIN(reply);
        opal_output_verbose(2, pmix_server_output,
                            "%s pmix:server:recv sending allgather release of size %lu to %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (unsigned long)buffer->bytes_used,
                            ORTE_NAME_PRINT(&lcl->name));
        peer = pmix_server_peer_lookup(lcl->sd);
        PMIX_SERVER_QUEUE_SEND(peer, lcl->tag, reply);
    }
    OBJ_RELEASE(reply);

    /* release the tracker */
    opal_list_remove_item(&collectives, &trk->super);
    OBJ_RELEASE(trk);
}


static void pmix_server_dmdx_recv(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata)
{
    int rc, ret;
    int32_t cnt;
    opal_buffer_t *reply, *bptr, buf;
    opal_value_t *kvp, *kvp2, kv, *kp;
    opal_identifier_t idreq;
    orte_process_name_t name;
    orte_job_t *jdata;
    orte_proc_t *proc;
    opal_list_t values;
    bool found;

    opal_output_verbose(2, pmix_server_output,
                        "%s dmdx:recv request from proc %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(sender));

    /* unpack the id of the proc whose data is being requested */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &idreq, &cnt, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    /* is this proc one of mine? */
    memcpy((char*)&name, (char*)&idreq, sizeof(orte_process_name_t));
    if (NULL == (jdata = orte_get_job_data_object(name.jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return;
    }
    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, name.vpid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return;
    }
    if (ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_LOCAL)) {
        kvp = NULL;
        kvp2 = NULL;
        found = false;
        /* retrieve the REMOTE blob for that proc */
        OBJ_CONSTRUCT(&values, opal_list_t);
        if (OPAL_SUCCESS == opal_dstore.fetch(pmix_server_remote_handle, &idreq, "modex", &values)) {
            kvp = (opal_value_t*)opal_list_remove_first(&values);
            found = true;
        }
        OPAL_LIST_DESTRUCT(&values);
        /* retrieve the global blob for that proc */
        OBJ_CONSTRUCT(&values, opal_list_t);
        if (OPAL_SUCCESS == opal_dstore.fetch(pmix_server_global_handle, &idreq, "modex", &values)) {
            kvp2 = (opal_value_t*)opal_list_remove_first(&values);
            found = true;
        }
        OPAL_LIST_DESTRUCT(&values);
        /* return it */
        reply = OBJ_NEW(opal_buffer_t);
        /* pack the id of the requested proc */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &idreq, 1, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            return;
        }
        /* pack the status */
        if (found) {
            ret = OPAL_SUCCESS;
        } else {
            ret = OPAL_ERR_NOT_FOUND;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &ret, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            return;
        }
        /* always pass the hostname */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(PMIX_HOSTNAME);
        kv.type = OPAL_STRING;
        kv.data.string = strdup(orte_process_info.nodename);
        kp = &kv;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &kp, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            OBJ_DESTRUCT(&buf);
            OBJ_DESTRUCT(&kv);
            return;
        }
        OBJ_DESTRUCT(&kv);
        /* pack the blob */
        bptr = &buf;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &bptr, 1, OPAL_BUFFER))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            OBJ_DESTRUCT(&buf);
            return;
        }
        OBJ_DESTRUCT(&buf);
        /* remote blob */
        if (NULL != kvp) {
            opal_output_verbose(2, pmix_server_output,
                                "%s passing remote blob of size %d from proc %s to proc %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (int)kvp->data.bo.size,
                                ORTE_NAME_PRINT(&name),
                                ORTE_NAME_PRINT(sender));
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            opal_dss.load(&buf, kvp->data.bo.bytes, kvp->data.bo.size);
            /* protect the data */
            kvp->data.bo.bytes = NULL;
            kvp->data.bo.size = 0;
            bptr = &buf;
            if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &bptr, 1, OPAL_BUFFER))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(reply);
                OBJ_DESTRUCT(&buf);
                return;
            }
            OBJ_DESTRUCT(&buf);
            OBJ_RELEASE(kvp);
        }
        /* global blob */
        if (NULL != kvp2) {
            opal_output_verbose(2, pmix_server_output,
                                "%s passing global blob of size %d from proc %s to proc %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (int)kvp2->data.bo.size,
                                ORTE_NAME_PRINT(&name),
                                ORTE_NAME_PRINT(sender));
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            opal_dss.load(&buf, kvp2->data.bo.bytes, kvp2->data.bo.size);
            /* protect the data */
            kvp2->data.bo.bytes = NULL;
            kvp2->data.bo.size = 0;
            bptr = &buf;
            if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &bptr, 1, OPAL_BUFFER))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(reply);
                OBJ_DESTRUCT(&buf);
                return;
            }
            OBJ_DESTRUCT(&buf);
            OBJ_RELEASE(kvp2);
        }
        /* send the response */
        orte_rml.send_buffer_nb(sender, reply,
                                ORTE_RML_TAG_DIRECT_MODEX_RESP,
                                orte_rml_send_callback, NULL);
        return;
    }
}

static void pmix_server_dmdx_resp(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata)
{
    pmix_server_dmx_req_t *req, *nxt;
    int rc, ret;
    int32_t cnt;
    opal_buffer_t *reply, xfer, *bptr;
    opal_identifier_t target;
    opal_value_t kv;

    opal_output_verbose(2, pmix_server_output,
                        "%s dmdx:recv response from proc %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(sender));

    /* unpack the id of the target whose info we just received */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &target, &cnt, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the status */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &ret, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the hostname blob */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &bptr, &cnt, OPAL_BUFFER))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* prep the reply */
    reply = OBJ_NEW(opal_buffer_t);
    /* pack the returned status */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &ret, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(reply);
        OBJ_RELEASE(bptr);
        return;
    }
    /* pack the hostname blob */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &bptr, 1, OPAL_BUFFER))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(reply);
        OBJ_RELEASE(bptr);
        return;
    }
    OBJ_RELEASE(bptr);

    /* pass across any returned blobs */
    opal_dss.copy_payload(reply, buffer);

    /* if we got something, store the blobs locally so we can
     * meet any further requests without doing a remote fetch.
     * This must be done as a single blob for later retrieval */
    if (ORTE_SUCCESS == ret) {
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup("modex");
        kv.type = OPAL_BYTE_OBJECT;
        OBJ_CONSTRUCT(&xfer, opal_buffer_t);
        opal_dss.copy_payload(&xfer, buffer);
        opal_dss.unload(&xfer, (void**)&kv.data.bo.bytes, &kv.data.bo.size);
        opal_dstore.store(pmix_server_remote_handle, &target, &kv);
        OBJ_DESTRUCT(&kv);
        OBJ_DESTRUCT(&xfer);
    }

    /* check ALL reqs to see who requested this target - due to
     * async behavior, we may have requests from more than one
     * process */
    OPAL_LIST_FOREACH_SAFE(req, nxt, &pmix_server_pending_dmx_reqs, pmix_server_dmx_req_t) {
        if (target == req->target) {
            OBJ_RETAIN(reply);
            PMIX_SERVER_QUEUE_SEND(req->peer, req->tag, reply);
            opal_list_remove_item(&pmix_server_pending_dmx_reqs, &req->super);
            OBJ_RELEASE(req);
        }
    }
}


/*
 * Routine for debugging to print the connection state and socket options
 */
void pmix_server_peer_dump(pmix_server_peer_t* peer, const char* msg)
{
    char buff[255];
    int nodelay,flags;

    if ((flags = fcntl(peer->sd, F_GETFL, 0)) < 0) {
        opal_output(0, "%s usock_peer_dump: fcntl(F_GETFL) failed: %s (%d)\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
                                                                                                            
#if defined(USOCK_NODELAY)
    optlen = sizeof(nodelay);
    if (getsockopt(peer->sd, IPPROTO_USOCK, USOCK_NODELAY, (char *)&nodelay, &optlen) < 0) {
        opal_output(0, "%s usock_peer_dump: USOCK_NODELAY option: %s (%d)\n", 
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
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
        OBJ_RELEASE(p->data);
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

static void rqcon(pmix_server_dmx_req_t *p)
{
    p->peer = NULL;
}
static void rqdes(pmix_server_dmx_req_t *p)
{
    if (NULL != p->peer) {
        OBJ_RELEASE(p->peer);
    }
}
OBJ_CLASS_INSTANCE(pmix_server_dmx_req_t,
                   opal_list_item_t,
                   rqcon, rqdes);
