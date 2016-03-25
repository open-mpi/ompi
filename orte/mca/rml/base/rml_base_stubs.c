/*
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Intel Corporation.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include <string.h>

#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"
#include "orte/mca/mca.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"

#include "orte/mca/rml/base/base.h"

/*
 * The stub API interface functions
 */

/** Enable communication once a process name has been assigned */
int orte_rml_API_enable_comm(void)
{
    orte_rml_base_active_t *active, *next;
    int rc;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:enable_comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and let each one enable their comm */
    OPAL_LIST_FOREACH_SAFE(active, next, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->enable_comm) {
            if (ORTE_SUCCESS != (rc = active->module->enable_comm())) {
                opal_output_verbose(2, orte_rml_base_framework.framework_output,
                                    "%s rml:base:enable_comm Component %s was unable to enable comm",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    active->component->mca_component_name);
                /* remove this component from our actives */
                opal_list_remove_item(&orte_rml_base.actives, &active->super);
                /* give the module a chance to finalize */
                if (NULL != active->module->finalize) {
                    active->module->finalize();
                }
                OBJ_RELEASE(active);
            }
        }
    }
    /* ensure we still have someone */
    if (0 < opal_list_get_size(&orte_rml_base.actives)) {
        return ORTE_SUCCESS;
    }
    return ORTE_ERR_UNREACH;
}

/** Shutdown the communication system and clean up resources */
void orte_rml_API_finalize(void)
{
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:finalize()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->finalize) {
            active->module->finalize();
        }
    }
}

/** Get contact information for local process */
char* orte_rml_API_get_contact_info(void)
{
    char **rc = NULL, *tmp;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:get_contact_info()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->get_contact_info) {
            tmp = active->module->get_contact_info();
            if (NULL != tmp) {
                opal_argv_append_nosize(&rc, tmp);
                free(tmp);
            }
        }
    }
    if (NULL != rc) {
        tmp = opal_argv_join(rc, ';');
    } else {
        tmp = NULL;
    }
    return tmp;
}

/** Set contact information for remote process */
void orte_rml_API_set_contact_info(const char *contact_info)
{
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:set_contact_info()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and let all modules parse the info
     * to extract their relevant portions */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->set_contact_info) {
            active->module->set_contact_info(contact_info);
        }
    }
}

/** Ping process for connectivity check */
int orte_rml_API_ping(const char* contact_info,
                      const struct timeval* tv)
{
    int rc = ORTE_ERR_UNREACH;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:ping()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see if anyone can confirm connection */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->ping) {
            rc = active->module->ping(contact_info, tv);
            if (ORTE_SUCCESS == rc) {
                /* at least someone can reach this target */
                break;
            }
        }
    }
    return rc;
}

/** Send non-blocking iovec message */
int orte_rml_API_send_nb(orte_process_name_t* peer,
                         struct iovec* msg,
                         int count,
                         orte_rml_tag_t tag,
                         orte_rml_callback_fn_t cbfunc,
                         void* cbdata)
{
    int rc = ORTE_ERR_UNREACH;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:send_nb() to peer %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->send_nb) {
            rc = active->module->send_nb(peer, msg, count, tag, cbfunc, cbdata);
            if (ORTE_SUCCESS == rc) {
                /* someone was able to send it */
                break;
            }
        }
    }
    return rc;
}

/** Send non-blocking buffer message */
int orte_rml_API_send_buffer_nb(orte_process_name_t* peer,
                                struct opal_buffer_t* buffer,
                                orte_rml_tag_t tag,
                                orte_rml_buffer_callback_fn_t cbfunc,
                                void* cbdata)
{
    int rc = ORTE_ERR_UNREACH;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:send_buffer_nb()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->send_buffer_nb) {
            if (ORTE_SUCCESS == (rc = active->module->send_buffer_nb(peer, buffer, tag, cbfunc, cbdata))) {
                break;
            }
        }
    }
    return rc;
}


/** post a receive for an IOV message */
void orte_rml_API_recv_nb(orte_process_name_t* peer,
                          orte_rml_tag_t tag,
                          bool persistent,
                          orte_rml_callback_fn_t cbfunc,
                          void* cbdata)
{
    orte_rml_base_active_t *active;
    orte_rml_recv_request_t *req;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_recv_nb for peer %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));

    /* cycle thru the actives and give each module a chance
     * to do whatever module-specific things they need to do */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->recv_nb) {
            active->module->recv_nb(peer, tag, persistent, cbfunc, cbdata);
        }
    }
    /* now push the request into the event base so we can add
     * the receive to our list of posted recvs */
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

/** Receive non-blocking buffer message */
void orte_rml_API_recv_buffer_nb(orte_process_name_t* peer,
                                 orte_rml_tag_t tag,
                                 bool persistent,
                                 orte_rml_buffer_callback_fn_t cbfunc,
                                 void* cbdata)
{
    orte_rml_base_active_t *active;
    orte_rml_recv_request_t *req;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_recv_buffer_nb for peer %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));

    /* cycle thru the actives and give each module a chance
     * to do whatever module-specific things they need to do */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->recv_buffer_nb) {
            active->module->recv_buffer_nb(peer, tag, persistent, cbfunc, cbdata);
        }
    }

    /* now push the request into the event base so we can add
     * the receive to our list of posted recvs */
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

/** Cancel posted non-blocking receive */
void orte_rml_API_recv_cancel(orte_process_name_t* peer, orte_rml_tag_t tag)
{
    orte_rml_base_active_t *active;
    orte_rml_recv_request_t *req;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_recv_cancel for peer %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));

    /* cycle thru the actives and give each module a chance
     * to do whatever module-specific things they need to do */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->recv_cancel) {
            active->module->recv_cancel(peer,tag);
        }
    }

    /* now push the request into the event base so we can remove
     * the receive from our list of posted recvs */
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

/** Add callback for communication exception */
int orte_rml_API_add_exception_handler(orte_rml_exception_callback_t cbfunc)
{
    int rc = ORTE_ERROR;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:add_exception_handler()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->add_exception_handler) {
            if (ORTE_SUCCESS == (rc = active->module->add_exception_handler(cbfunc))) {
                break;
            }
        }
    }
    return rc;
}

/** Delete callback for communication exception */
int orte_rml_API_del_exception_handler(orte_rml_exception_callback_t cbfunc)
{
    int rc = ORTE_ERROR;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:del_exception_handler()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->del_exception_handler) {
            if (ORTE_SUCCESS == (rc = active->module->del_exception_handler(cbfunc))) {
                break;
            }
        }
    }
    return rc;
}

/** Fault tolerance handler */
int orte_rml_API_ft_event(int state)
{
   int rc = ORTE_ERROR;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:ft_event()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and let them all handle this event */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->ft_event) {
            if (ORTE_SUCCESS != (rc = active->module->ft_event(state))) {
                break;
            }
        }
    }
    return rc;
}


/** Purge information */
void orte_rml_API_purge(orte_process_name_t *peer)
{
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:purge() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and let everyone purge related info */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->purge) {
            active->module->purge(peer);
        }
    }
}
