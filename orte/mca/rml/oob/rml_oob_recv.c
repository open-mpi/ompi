/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "orte/mca/rml/base/base.h"

#include "rml_oob.h"

void orte_rml_oob_recv_nb(orte_process_name_t* peer,
                          orte_rml_tag_t tag,
                          bool persistent,
                          orte_rml_callback_fn_t cbfunc,
                          void* cbdata)
{
    orte_rml_recv_request_t *req;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_recv_nb for peer %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));

    req = OBJ_NEW(orte_rml_recv_request_t);
    req->post->buffer_data = false; 
    req->post->peer.jobid = peer->jobid;
    req->post->peer.vpid = peer->vpid;
    req->post->tag = tag;
    req->post->persistent = persistent;
    req->post->cbfunc.iov = cbfunc;
    req->post->cbdata = cbdata;
    opal_event_set(orte_event_base, &req->ev, -1,
                   OPAL_EV_WRITE,
                   orte_rml_base_post_recv, req);
    opal_event_set_priority(&req->ev, ORTE_MSG_PRI);
    opal_event_active(&req->ev, OPAL_EV_WRITE, 1);
}


void orte_rml_oob_recv_buffer_nb(orte_process_name_t* peer,
                                 orte_rml_tag_t tag,
                                 bool persistent,
                                 orte_rml_buffer_callback_fn_t cbfunc,
                                 void* cbdata)
{
    orte_rml_recv_request_t *req;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_recv_buffer_nb for peer %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));

    req = OBJ_NEW(orte_rml_recv_request_t);
    req->post->buffer_data = true; 
    req->post->peer.jobid = peer->jobid;
    req->post->peer.vpid = peer->vpid;
    req->post->tag = tag;
    req->post->persistent = persistent;
    req->post->cbfunc.buffer = cbfunc;
    req->post->cbdata = cbdata;
    opal_event_set(orte_event_base, &req->ev, -1,
                   OPAL_EV_WRITE,
                   orte_rml_base_post_recv, req);
    opal_event_set_priority(&req->ev, ORTE_MSG_PRI);
    opal_event_active(&req->ev, OPAL_EV_WRITE, 1);
}


void orte_rml_oob_recv_cancel(orte_process_name_t* peer, 
                              orte_rml_tag_t tag)
{
    orte_rml_recv_request_t *req;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_recv_cancel for peer %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));

    req = OBJ_NEW(orte_rml_recv_request_t);
    req->cancel = true; 
    req->post->peer.jobid = peer->jobid;
    req->post->peer.vpid = peer->vpid;
    req->post->tag = tag;
    opal_event_set(orte_event_base, &req->ev, -1,
                   OPAL_EV_WRITE,
                   orte_rml_base_post_recv, req);
    opal_event_set_priority(&req->ev, ORTE_MSG_PRI);
    opal_event_active(&req->ev, OPAL_EV_WRITE, 1);
}
