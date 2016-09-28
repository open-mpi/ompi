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

/** Open a conduit  - check if the ORTE_RML_INCLUDE_COMP attribute is provided, this is     */
/*  a comma seperated list of components, try to open the conduit in this order.            */
/*  if the ORTE_RML_INCLUDE_COMP is not provided or this list was not able to open conduit  */
/*  call the open_conduit() of the component in priority order to see if they can use the   */
/*  attribute to open a conduit                                                             */
/*  Note:  The component takes care of checking for duplicate and returning the previously  */
/*  opened module* in case of duplicates. Currently we are saving it in a new conduit_id    */
/*  even if it is duplicate. [ToDo] compare the module* received from component to see if   */
/*  already present in array and return the prev conduit_id instead of adding it again to array */                                                  
/* @param[in]   attributes  The attributes is a list of opal_value_t of type OPAL_STRING    */
int orte_rml_API_open_conduit(opal_list_t *attributes)
{
    orte_rml_base_active_t *active, *next;
    orte_rml_component_t *comp;
    void *mod;
    char *comp_attrib = NULL, *comp_name = NULL;
    int rc;
    opal_value_t *attrib;

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                         "%s rml:base:open_conduit",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    /* Check if module name is specified then directly call it  */
    /* without going through priority order */
    if ( orte_get_attribute(attributes, ORTE_RML_INCLUDE_COMP_ATTRIB, (void**)&comp_attrib, OPAL_STRING)) {
        opal_output_verbose(2, orte_rml_base_framework.framework_output,
                                    "%s open_conduit called with attribute <include =  %s>",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), comp_attrib);
        comp_name = strtok(comp_attrib,",");
        while ( NULL != comp_name ) {
            OPAL_LIST_FOREACH_SAFE(active, next, &orte_rml_base.actives, orte_rml_base_active_t) {
                comp = (orte_rml_component_t *)active->component;
                if ( strcmp (active->component->mca_component_name, comp_name) == 0 ) {
                  if (NULL != comp->open_conduit) {
                        opal_output_verbose(2, orte_rml_base_framework.framework_output,
                                    "%s calling  open_conduit() for component %s ",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    comp_name);
                    if (NULL != (mod = (void*)comp->open_conduit(attributes))) {
                        opal_output_verbose(2, orte_rml_base_framework.framework_output,
                                    "%s rml:base:open_conduit Component %s provided a conduit",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    active->component->mca_component_name);
                        /* store this conduit in our array */
                        rc = opal_pointer_array_add(&orte_rml_base.conduits, mod);
                        return rc;
                    
                    }
                  }
                }
            } 
            comp_name = strtok(NULL,",");
        }
    }


    opal_output_verbose(2, orte_rml_base_framework.framework_output,
                                    "%s rml:base:open_conduit cycling through the actives to open a conduit",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    /* cycle thru the actives and let each one enable their comm */
    OPAL_LIST_FOREACH_SAFE(active, next, &orte_rml_base.actives, orte_rml_base_active_t) {
        comp = (orte_rml_component_t *)active->component;
        if (NULL != comp->open_conduit) {
            if (NULL != (mod = (void*)comp->open_conduit(attributes))) {
                opal_output_verbose(2, orte_rml_base_framework.framework_output,
                                    "%s rml:base:open_conduit Component %s provided a conduit",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    active->component->mca_component_name);
                /* store this conduit in our array */
                rc = opal_pointer_array_add(&orte_rml_base.conduits, mod);
                return rc;
            }
        }
    }
    /* we get here if nobody could support it */
    return ORTE_ERR_NOT_SUPPORTED;
}



/** Shutdown the communication system and clean up resources */
void orte_rml_API_finalize(void)
{
    orte_rml_base_module_t *mod;
    int idx, total_conduits = opal_pointer_array_get_size(&orte_rml_base.conduits);

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                         "%s rml:base:finalize()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* cycle thru the conduits opened and call each module's finalize */
    for (idx = 0; idx < total_conduits ; idx++)
    {
        if( NULL != (mod = (orte_rml_base_module_t*)opal_pointer_array_get_item(&orte_rml_base.conduits,idx))) {
          mod->finalize(mod);
        }          
        
    }

    orte_rml_base_comm_stop();   
}



/** Get contact information for local process */
char* orte_rml_API_get_contact_info(void)
{
      char **rc = NULL, *tmp;
    orte_rml_base_active_t *active;

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                         "%s rml:base:get_contact_info()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

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

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                         "%s rml:base:set_contact_info()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* cycle thru the actives and let all modules parse the info
     * to extract their relevant portions */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->set_contact_info) {
            active->module->set_contact_info(contact_info);
        }
    }
}

/** Ping process for connectivity check */
int orte_rml_API_ping_conduit(int conduit_id,
                      const char* contact_info,
                      const struct timeval* tv)
{
    int rc = ORTE_ERR_UNREACH;
    orte_rml_base_module_t *mod;

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                         "%s rml:base:ping(conduit-%d)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),conduit_id);

    /* get the module */
    if (NULL == (mod = (orte_rml_base_module_t*)opal_pointer_array_get_item(&orte_rml_base.conduits, conduit_id))) {
        return rc;
    }
    if (NULL == mod->ping) {
        return rc;
    }
    rc = mod->ping(mod, contact_info, tv);
    return rc;
}


/** Ping process for connectivity check */
int orte_rml_API_ping(const char* contact_info,
                      const struct timeval* tv)
{

    int conduit_id = orte_rml_base.def_conduit_id;
    return orte_rml_API_ping_conduit(conduit_id, contact_info, tv);
}

/** Send non-blocking iovec message through a specific conduit*/
int orte_rml_API_send_nb_conduit(int conduit_id,
                         orte_process_name_t* peer,
                         struct iovec* msg,
                         int count,
                         orte_rml_tag_t tag,
                         orte_rml_callback_fn_t cbfunc,
                         void* cbdata)
{
    int rc = ORTE_ERR_UNREACH;
    orte_rml_base_module_t *mod;

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                         "%s rml:base:send_nb_conduit() to peer %s through conduit %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer),conduit_id);
    /* get the module */
    if (NULL == (mod = (orte_rml_base_module_t*)opal_pointer_array_get_item(&orte_rml_base.conduits, conduit_id))) {
        return rc;
    }
    if (NULL == mod->send_nb) {
        return rc;
    }
    rc = mod->send_nb(mod, peer, msg, count, tag, cbfunc, cbdata);
    return rc;
}

/** Send non-blocking buffer message */
int orte_rml_API_send_buffer_nb_conduit(int conduit_id, 
                                orte_process_name_t* peer,
                                struct opal_buffer_t* buffer,
                                orte_rml_tag_t tag,
                                orte_rml_buffer_callback_fn_t cbfunc,
                                void* cbdata)
{
    int rc = ORTE_ERR_UNREACH;
    orte_rml_base_module_t *mod;

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                         "%s rml:base:send_buffer_nb_conduit() to peer %s through conduit %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer),conduit_id);
    /* get the module */
    if (NULL == (mod = (orte_rml_base_module_t*)opal_pointer_array_get_item(&orte_rml_base.conduits, conduit_id))) {
        return rc;
    }
    if (NULL == mod->send_buffer_nb) {
        return rc;
    }
    rc = mod->send_buffer_nb(mod, peer, buffer, tag, cbfunc, cbdata);
    return rc;
}

/** Send non-blocking iovec message through a specific conduit*/
int orte_rml_API_send_nb(orte_process_name_t* peer,
                         struct iovec* msg,
                         int count,
                         orte_rml_tag_t tag,
                         orte_rml_callback_fn_t cbfunc,
                         void* cbdata)
{

    int conduit_id = orte_rml_base.def_conduit_id;

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                         "%s rml:base:send_nb() to peer %s through conduit %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer),conduit_id);
    return orte_rml_API_send_nb_conduit(conduit_id, peer, msg, count, tag, cbfunc, cbdata);
}

/** Send non-blocking buffer message */
int orte_rml_API_send_buffer_nb(orte_process_name_t* peer,
                                struct opal_buffer_t* buffer,
                                orte_rml_tag_t tag,
                                orte_rml_buffer_callback_fn_t cbfunc,
                                void* cbdata)
{

    int conduit_id = orte_rml_base.def_conduit_id;
    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                         "%s rml:base:send_buffer_nb() to peer %s through conduit %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer),conduit_id);
    return orte_rml_API_send_buffer_nb_conduit(conduit_id, peer, buffer, tag, cbfunc, cbdata);
}


/** post a receive for an IOV message - this is done
 * strictly in the base, and so it does not go to a module */
void orte_rml_API_recv_nb(orte_process_name_t* peer,
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

    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s rml_recv_cancel for peer %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag);

    /* push the request into the event base so we can remove
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

/** Purge information */
void orte_rml_API_purge(orte_process_name_t *peer)
{
 orte_rml_base_active_t *active;

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                         "%s rml:base:purge() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* cycle thru the actives and let everyone purge related info */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->purge) {
            active->module->purge(peer);
        }
    }
 
}


int orte_rml_API_query_transports(opal_value_t **providers)
{

    int rc = ORTE_ERROR;
    orte_rml_base_active_t *active;
    orte_rml_component_t *comp;

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                         "%s rml:base:orte_rml_API_query_transports()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* cycle thru the actives and let them all handle this event */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        comp = (orte_rml_component_t *)active->component;
        if (NULL != comp->query_transports) {
            	  opal_output_verbose(10,orte_rml_base_framework.framework_output,
                                     "\n calling  module: %s->query_transports() \n",
                                          active->component->mca_component_name);
            if (ORTE_SUCCESS == (rc = comp->query_transports(providers))) {
                break;
            }
        }
    }
    return rc;

}



