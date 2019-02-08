/* -*- Mode: C; c-basic-offset:4 ; -*- */
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
 * Copyright (c) 2007-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/mca/backtrace/backtrace.h"
#include "opal/mca/event/event.h"

#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"
#endif
#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/routed/routed.h"
#include "rml_oob.h"

static int rml_oob_open(void);
static int rml_oob_close(void);
static int component_query(mca_base_module_t **module, int *priority);

/**
 * component definition
 */
orte_rml_component_t mca_rml_oob_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

    .base = {
        ORTE_RML_BASE_VERSION_3_0_0,

        .mca_component_name = "oob",
        MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                              ORTE_RELEASE_VERSION),
        .mca_open_component = rml_oob_open,
        .mca_close_component = rml_oob_close,
        .mca_query_component = component_query,

    },
    .data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .priority = 5
};

/* Local variables */
static void recv_nb(orte_process_name_t* peer,
                    orte_rml_tag_t tag,
                    bool persistent,
                    orte_rml_callback_fn_t cbfunc,
                    void* cbdata)
{
    orte_rml_recv_request_t *req;

    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s rml_recv_nb for peer %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag);

    /* push the request into the event base so we can add
     * the receive to our list of posted recvs */
    req = OBJ_NEW(orte_rml_recv_request_t);
    req->post->buffer_data = false;
    req->post->peer.jobid = peer->jobid;
    req->post->peer.vpid = peer->vpid;
    req->post->tag = tag;
    req->post->persistent = persistent;
    req->post->cbfunc.iov = cbfunc;
    req->post->cbdata = cbdata;
    ORTE_THREADSHIFT(req, orte_event_base, orte_rml_base_post_recv, ORTE_MSG_PRI);
}
static void recv_buffer_nb(orte_process_name_t* peer,
                           orte_rml_tag_t tag,
                           bool persistent,
                           orte_rml_buffer_callback_fn_t cbfunc,
                           void* cbdata)
{
    orte_rml_recv_request_t *req;

    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s rml_recv_buffer_nb for peer %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag);

    /* push the request into the event base so we can add
     * the receive to our list of posted recvs */
    req = OBJ_NEW(orte_rml_recv_request_t);
    req->post->buffer_data = true;
    req->post->peer.jobid = peer->jobid;
    req->post->peer.vpid = peer->vpid;
    req->post->tag = tag;
    req->post->persistent = persistent;
    req->post->cbfunc.buffer = cbfunc;
    req->post->cbdata = cbdata;
    ORTE_THREADSHIFT(req, orte_event_base, orte_rml_base_post_recv, ORTE_MSG_PRI);
}
static void recv_cancel(orte_process_name_t* peer, orte_rml_tag_t tag)
{
    orte_rml_recv_request_t *req;

    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s rml_recv_cancel for peer %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag);

    ORTE_ACQUIRE_OBJECT(orte_event_base_active);
    if (!orte_event_base_active) {
        /* no event will be processed any more, so simply return. */
        return;
    }

    /* push the request into the event base so we can remove
     * the receive from our list of posted recvs */
    req = OBJ_NEW(orte_rml_recv_request_t);
    req->cancel = true;
    req->post->peer.jobid = peer->jobid;
    req->post->peer.vpid = peer->vpid;
    req->post->tag = tag;
    ORTE_THREADSHIFT(req, orte_event_base, orte_rml_base_post_recv, ORTE_MSG_PRI);
}
static int oob_ping(const char* uri, const struct timeval* tv)
{
    return ORTE_ERR_UNREACH;
}

static orte_rml_base_module_t base_module = {
    .component = (struct orte_rml_component_t*)&mca_rml_oob_component,
    .ping = oob_ping,
    .send_nb = orte_rml_oob_send_nb,
    .send_buffer_nb = orte_rml_oob_send_buffer_nb,
    .recv_nb = recv_nb,
    .recv_buffer_nb = recv_buffer_nb,
    .recv_cancel = recv_cancel,
    .purge = NULL
};

static int rml_oob_open(void)
{
    return ORTE_SUCCESS;
}


static int rml_oob_close(void)
{
    return ORTE_SUCCESS;
}

static int component_query(mca_base_module_t **module, int *priority)
{
    *priority = 50;
    *module = (mca_base_module_t *) &base_module;
    return ORTE_SUCCESS;
}
