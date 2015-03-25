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
 * Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
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
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/opal_socket_errno.h"
#include "opal/util/if.h"
#include "opal/util/net.h"
#include "opal/util/argv.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/state/state.h"
#include "orte/util/name_fns.h"
#include "orte/util/parse_options.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/oob/usock/oob_usock.h"
#include "orte/mca/oob/usock/oob_usock_component.h"
#include "orte/mca/oob/usock/oob_usock_peer.h"
#include "orte/mca/oob/usock/oob_usock_connection.h"
#include "orte/mca/oob/usock/oob_usock_listener.h"
#include "orte/mca/oob/usock/oob_usock_ping.h"
/*
 * Local utility functions
 */

static int usock_component_register(void);
static int usock_component_open(void);
static int usock_component_close(void);

static bool component_available(void);
static int component_startup(void);
static void component_shutdown(void);
static int component_send(orte_rml_send_t *msg);
static char* component_get_addr(void);
static int component_set_addr(orte_process_name_t *peer,
                              char **uris);
static bool component_is_reachable(orte_process_name_t *peer);

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_oob_usock_component_t mca_oob_usock_component = {
    {
        {
            MCA_OOB_BASE_VERSION_2_0_0,
            "usock", /* MCA module name */
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            usock_component_open,  /* component open */
            usock_component_close, /* component close */
            NULL, /* component query */
            usock_component_register, /* component register */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        0,   // reserve space for an assigned index
        100, // default priority of this transport
        component_available,
        component_startup,
        component_shutdown,
        component_send,
        component_get_addr,
        component_set_addr,
        component_is_reachable
    },
};

/*
 * Initialize global variables used w/in this module.
 */
static int usock_component_open(void)
{
    return ORTE_SUCCESS;
}

/*
 * Cleanup of global variables used by this module.
 */
static int usock_component_close(void)
{
    return ORTE_SUCCESS;
}


static int usock_component_register(void)
{
    mca_base_component_t *component = &mca_oob_usock_component.super.oob_base;

    /* register oob module parameters */
    mca_oob_usock_component.max_retries = 2;
    (void)mca_base_component_var_register(component, "peer_retries",
                                          "Number of times to try shutting down a connection before giving up",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_oob_usock_component.max_retries);

    return ORTE_SUCCESS;
}


static bool component_available(void)
{
    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                        "oob:usock: component_available called");

    /* if session directories were forbidden, then we cannot be used */
    if (!orte_create_session_dirs ||
        NULL == orte_process_info.tmpdir_base ||
        NULL == orte_process_info.top_session_dir) {
        return false;
    }

    /* this component is not available to tools */
    if (ORTE_PROC_IS_TOOL) {
        return false;
    }

    /* direct-launched apps cannot use it either */
    if (ORTE_PROC_IS_APP &&
        (NULL == orte_process_info.my_daemon_uri)) {
        return false;
    }

    /* otherwise, we are available */
    return true;
}

/* Start the module */
static int component_startup(void)
{
    int rc=ORTE_SUCCESS;

    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s USOCK STARTUP",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* setup the path to the daemon rendezvous point */
    memset(&mca_oob_usock_component.address, 0, sizeof(struct sockaddr_un));
    mca_oob_usock_component.address.sun_family = AF_UNIX;
    snprintf(mca_oob_usock_component.address.sun_path,
             sizeof(mca_oob_usock_component.address.sun_path)-1,
             "%s/%s/%s/0/%s", orte_process_info.tmpdir_base,
             orte_process_info.top_session_dir,
             ORTE_JOB_FAMILY_PRINT(ORTE_PROC_MY_NAME->jobid), "usock");
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "SUNPATH: %s", mca_oob_usock_component.address.sun_path);

    /* if we are a daemon/HNP, start the listening event - this will create
     * the rendezvous link
     */
    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        if (ORTE_SUCCESS != (rc = orte_oob_usock_start_listening())) {
            ORTE_ERROR_LOG(rc);
        }
    } else {
        /* if the rendezvous point isn't there, then that's an error */
        /* if the rendezvous file doesn't exist, that's an error */
        if (0 != access(mca_oob_usock_component.address.sun_path, R_OK)) {
            opal_output_verbose(2, orte_oob_base_framework.framework_output,
                                "SUNPATH: %s NOT READABLE", mca_oob_usock_component.address.sun_path);
            return OPAL_ERR_NOT_FOUND;
        }
    }

    /* start the module */
    mca_oob_usock_module.api.init();

    return rc;
}

static void component_shutdown(void)
{
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s USOCK SHUTDOWN",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        if (mca_oob_usock_component.listener_ev_active) {
            opal_event_del(&mca_oob_usock_component.listener_event);
            mca_oob_usock_component.listener_ev_active = false;
        }
        /* delete the rendezvous file */
        unlink(mca_oob_usock_component.address.sun_path);
    }

    /* shutdown the module */
    if (NULL != mca_oob_usock_module.api.finalize) {
        mca_oob_usock_module.api.finalize();
    }
}

static int component_send(orte_rml_send_t *msg)
{
    orte_proc_t *proc;

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                        "%s oob:usock:send_nb to peer %s:%d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&msg->dst), msg->tag);

    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        /* daemons can only reach local procs */
        if (NULL == (proc = orte_get_proc_object(&msg->dst))) {
            return ORTE_ERR_TAKE_NEXT_OPTION;
        }
        if (!ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_LOCAL)) {
            return ORTE_ERR_TAKE_NEXT_OPTION;
        }
    }

    /* apps can reach anyone via this module as the daemon
     * will route the message to the final destination
     */

    mca_oob_usock_module.api.send_nb(msg);
    return ORTE_SUCCESS;
}

/* although we do not use the uri to determine a peer's
 * address (since we know the path via the session directory),
 * we have to provide something to the uri. This is needed
 * as other places in ORTE use a NULL uri to indicate lack
 * of a daemon. We may eventually remove that dependency,
 * but for now, just ensure that the uri is never NULL,
 * even if we are the only active OOB transport.
 */
static char* component_get_addr(void)
{
    char *tmp;
    tmp = strdup("usock");
    return tmp;
}

static int component_set_addr(orte_process_name_t *peer,
                              char **uris)
{
    orte_proc_t *proc;
    mca_oob_usock_peer_t *pr;
    uint64_t *ui64;

    /* if I am an application, then everything is addressable
     * by me via my daemon
     */
    if (ORTE_PROC_IS_APP) {
        /* if this is my daemon, then take it - otherwise, ignore */
        if (ORTE_PROC_MY_DAEMON->jobid == peer->jobid &&
            ORTE_PROC_MY_DAEMON->vpid == peer->vpid) {
            ui64 = (uint64_t*)peer;
            if (OPAL_SUCCESS != opal_hash_table_get_value_uint64(&mca_oob_usock_module.peers,
                                                                 (*ui64), (void**)&pr) || NULL == pr) {
                pr = OBJ_NEW(mca_oob_usock_peer_t);
                pr->name = *peer;
                opal_hash_table_set_value_uint64(&mca_oob_usock_module.peers, (*ui64), pr);
            }
            /* we have to initiate the connection because otherwise the
             * daemon has no way to communicate to us via this component
             * as the app doesn't have a listening port */
            pr->state = MCA_OOB_USOCK_CONNECTING;
            ORTE_ACTIVATE_USOCK_CONN_STATE(pr, mca_oob_usock_peer_try_connect);
            return ORTE_SUCCESS;
        }
        /* otherwise, indicate that we cannot reach this peer */
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }

    /* if I am a daemon or HNP, I can only reach my
     * own local procs via this component
     */
    if (ORTE_PROC_MY_NAME->jobid == peer->jobid) {
        /* another daemon */
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }
    if (NULL == (proc = orte_get_proc_object(peer)) ||
        !ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_LOCAL)) {
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }
    /* indicate that this peer is addressable by this component */
    ui64 = (uint64_t*)peer;
    if (OPAL_SUCCESS != opal_hash_table_get_value_uint64(&mca_oob_usock_module.peers,
                                                         (*ui64), (void**)&pr) || NULL == pr) {
        pr = OBJ_NEW(mca_oob_usock_peer_t);
        pr->name = *peer;
        opal_hash_table_set_value_uint64(&mca_oob_usock_module.peers, (*ui64), pr);
    }
    return ORTE_SUCCESS;
}

void mca_oob_usock_component_set_module(int fd, short args, void *cbdata)
{
    mca_oob_usock_peer_op_t *pop = (mca_oob_usock_peer_op_t*)cbdata;
    uint64_t ui64;
    int rc;
    orte_oob_base_peer_t *bpr;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s usock:set_module called for peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&pop->peer->name));

    /* retrieve the peer's name */
    memcpy(&ui64, (char*)&(pop->peer->name), sizeof(uint64_t));

    /* make sure the OOB knows that we are handling this peer - we
     * are in the same event base as the OOB base, so we can
     * directly access its storage
     */
    if (OPAL_SUCCESS != opal_hash_table_get_value_uint64(&orte_oob_base.peers,
                                                         ui64, (void**)&bpr) || NULL == bpr) {
        bpr = OBJ_NEW(orte_oob_base_peer_t);
    }
    opal_bitmap_set_bit(&bpr->addressable, mca_oob_usock_component.super.idx);
    bpr->component = &mca_oob_usock_component.super;
    if (OPAL_SUCCESS != (rc = opal_hash_table_set_value_uint64(&orte_oob_base.peers,
                                                               ui64, bpr))) {
        ORTE_ERROR_LOG(rc);
    }

    OBJ_RELEASE(pop);
}

void mca_oob_usock_component_lost_connection(int fd, short args, void *cbdata)
{
    mca_oob_usock_peer_op_t *pop = (mca_oob_usock_peer_op_t*)cbdata;
    uint64_t ui64;
    int rc;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s usock:lost connection called for peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&pop->peer->name));

    /* retrieve the peer's name */
    memcpy(&ui64, (char*)&(pop->peer->name), sizeof(uint64_t));

    /* mark the OOB's table that we can't reach it any more - for now, we don't
     * worry about shifting to another component. Eventually, we will want to push
     * this decision to the OOB so it can try other components and eventually error out
     */
    if (OPAL_SUCCESS != (rc = opal_hash_table_set_value_uint64(&orte_oob_base.peers,
                                                               ui64, NULL))) {
        ORTE_ERROR_LOG(rc);
    }

    /* activate the proc state - since an app only connects to its parent daemon,
     * and the daemon is *always* its lifeline, activate the lifeline lost state */
    if (ORTE_PROC_IS_APP) {
        ORTE_ACTIVATE_PROC_STATE(&pop->peer->name, ORTE_PROC_STATE_LIFELINE_LOST);
    } else {
        /* we are the daemon end, so notify that the child's comm failed */
        ORTE_ACTIVATE_PROC_STATE(&pop->peer->name, ORTE_PROC_STATE_COMM_FAILED);
    }

    OBJ_RELEASE(pop);
}

void mca_oob_usock_component_cannot_send(int fd, short args, void *cbdata)
{
    mca_oob_usock_msg_error_t *pop = (mca_oob_usock_msg_error_t*)cbdata;
    uint64_t ui64;
    int rc;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s usock:unable to send to peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&pop->hop));

    /* retrieve the peer's name */
    memcpy(&ui64, (char*)&(pop->hop), sizeof(uint64_t));

    /* mark the OOB's table that we can't reach it any more - for now, we don't
     * worry about shifting to another component. Eventually, we will want to push
     * this decision to the OOB so it can try other components and eventually error out
     */
    if (OPAL_SUCCESS != (rc = opal_hash_table_set_value_uint64(&orte_oob_base.peers,
                                                               ui64, NULL))) {
        ORTE_ERROR_LOG(rc);
    }

    /* have the OOB base try to send it again */
    ORTE_OOB_SEND(pop->rmsg);

    OBJ_RELEASE(pop);
}

void mca_oob_usock_component_failed_to_connect(int fd, short args, void *cbdata)
{
    mca_oob_usock_peer_op_t *pop = (mca_oob_usock_peer_op_t*)cbdata;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s usock:failed_to_connect called for peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&pop->peer->name));

    /* if we are terminating, then don't do anything further */
    if (orte_orteds_term_ordered || orte_finalizing || orte_abnormal_term_ordered) {
        OBJ_RELEASE(pop);
        return;
    }

    /* activate the proc state */
    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s usock:failed_to_connect unable to reach peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&pop->peer->name));

    /* since an app only connects to its parent daemon,
     * and the daemon is *always* its lifeline, activate the lifeline lost state */
    if (ORTE_PROC_IS_APP) {
        ORTE_ACTIVATE_PROC_STATE(&pop->peer->name, ORTE_PROC_STATE_LIFELINE_LOST);
    } else {
        /* we are the daemon end, so notify that the child's comm failed */
        ORTE_ACTIVATE_PROC_STATE(&pop->peer->name, ORTE_PROC_STATE_COMM_FAILED);
    }
    OBJ_RELEASE(pop);
}

static bool component_is_reachable(orte_process_name_t *peer)
{
    orte_proc_t *proc;

    /* if I am an application, then everything is reachable
     * by me via my daemon
     */
    if (ORTE_PROC_IS_APP) {
        return true;
    }

    /* if I am a daemon or HNP, I can only reach my
     * own local procs via this component
     */
    if (ORTE_PROC_MY_NAME->jobid == peer->jobid) {
        /* another daemon */
        return false;
    }
    if (NULL == (proc = orte_get_proc_object(peer)) ||
        !ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_LOCAL)) {
        return false;
    }
    /* indicate that this peer is reachable by this component */
    return true;
}

char* mca_oob_usock_state_print(mca_oob_usock_state_t state)
{
    switch (state) {
    case MCA_OOB_USOCK_UNCONNECTED:
        return "UNCONNECTED";
    case MCA_OOB_USOCK_CLOSED:
        return "CLOSED";
    case MCA_OOB_USOCK_RESOLVE:
        return "RESOLVE";
    case MCA_OOB_USOCK_CONNECTING:
        return "CONNECTING";
    case MCA_OOB_USOCK_CONNECT_ACK:
        return "ACK";
    case MCA_OOB_USOCK_CONNECTED:
        return "CONNECTED";
    case MCA_OOB_USOCK_FAILED:
        return "FAILED";
    default:
        return "UNKNOWN";
    }
}


mca_oob_usock_peer_t* mca_oob_usock_peer_lookup(const orte_process_name_t *name)
{
    mca_oob_usock_peer_t *peer;
    uint64_t ui64;

    memcpy(&ui64, (char*)name, sizeof(uint64_t));
    if (OPAL_SUCCESS != opal_hash_table_get_value_uint64(&mca_oob_usock_module.peers, ui64, (void**)&peer)) {
        return NULL;
    }
    return peer;
}

/* OOB USOCK Class instances */

static void peer_cons(mca_oob_usock_peer_t *peer)
{
    peer->auth_method = NULL;
    peer->sd = -1;
    peer->state = MCA_OOB_USOCK_UNCONNECTED;
    peer->retries = 0;
    OBJ_CONSTRUCT(&peer->send_queue, opal_list_t);
    peer->send_msg = NULL;
    peer->recv_msg = NULL;
    peer->send_ev_active = false;
    peer->recv_ev_active = false;
    peer->timer_ev_active = false;
}
static void peer_des(mca_oob_usock_peer_t *peer)
{
    if (NULL != peer->auth_method) {
        free(peer->auth_method);
    }
    if (0 <= peer->sd) {
        CLOSE_THE_SOCKET(peer->sd);
    }
    OPAL_LIST_DESTRUCT(&peer->send_queue);
}
OBJ_CLASS_INSTANCE(mca_oob_usock_peer_t,
                   opal_list_item_t,
                   peer_cons, peer_des);

OBJ_CLASS_INSTANCE(mca_oob_usock_peer_op_t,
                   opal_object_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(mca_oob_usock_msg_op_t,
                   opal_object_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(mca_oob_usock_conn_op_t,
                   opal_object_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(mca_oob_usock_ping_t,
                   opal_object_t,
                   NULL, NULL);

