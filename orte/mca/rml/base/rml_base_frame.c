/*
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/util/output.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"
#include "orte/util/name_fns.h"

#include "orte/mca/rml/base/base.h"

/* The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct. */
#include "orte/mca/rml/base/static-components.h"

orte_rml_module_t orte_rml;
orte_rml_base_t   orte_rml_base;

orte_rml_component_t *orte_rml_component = NULL;

static bool selected = false;
static char *orte_rml_base_wrapper = NULL;

static int orte_rml_base_register(mca_base_register_flag_t flags)
{
    int var_id;

    /* 
     * Which RML Wrapper component to use, if any
     *  - NULL or "" = No wrapper
     *  - ow. select that specific wrapper component
     */
    orte_rml_base_wrapper = NULL;
    var_id = mca_base_var_register("orte", "rml", "base", "wrapper",
                                   "Use a Wrapper component around the selected RML component",
                                   MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                   OPAL_INFO_LVL_9,
                                   MCA_BASE_VAR_SCOPE_READONLY,
                                   &orte_rml_base_wrapper);
    (void) mca_base_var_register_synonym(var_id, "orte", "rml",NULL,"wrapper", 0);

    return ORTE_SUCCESS;
}

static int orte_rml_base_close(void)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first(&orte_rml_base.posted_recvs))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_rml_base.posted_recvs);

    return mca_base_framework_components_close(&orte_rml_base_framework, NULL);
}

static int orte_rml_base_open(mca_base_open_flag_t flags)
{
    /* Initialize globals */
    OBJ_CONSTRUCT(&orte_rml_base.posted_recvs, opal_list_t);
    OBJ_CONSTRUCT(&orte_rml_base.unmatched_msgs, opal_list_t);
    
    /* Open up all available components */
    return mca_base_framework_components_open(&orte_rml_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, rml, "ORTE Run-Time Messaging Layer",
                           orte_rml_base_register, orte_rml_base_open, orte_rml_base_close,
                           mca_rml_base_static_components, 0);


int orte_rml_base_select(void)
{
    opal_list_item_t *item, *next;
    mca_base_component_list_item_t *cli;

    int selected_priority = -1;
    orte_rml_component_t *selected_component = NULL;
    orte_rml_module_t *selected_module = NULL;

    orte_rml_component_t *wrapper_component = NULL;
    bool return_silent=false;

    if (selected) {
        return ORTE_SUCCESS;
    }
    selected = true;

    OPAL_LIST_FOREACH(cli, &orte_rml_base_framework.framework_components, mca_base_component_list_item_t ) {
        orte_rml_component_t* component;
        component = (orte_rml_component_t *) cli->cli_component;

        opal_output_verbose(10, orte_rml_base_framework.framework_output, 
                            "orte_rml_base_select: initializing %s component %s",
                            component->rml_version.mca_type_name,
                            component->rml_version.mca_component_name);

        if (NULL == component->rml_init) {
            opal_output_verbose(10, orte_rml_base_framework.framework_output, 
                                "orte_rml_base_select: no init function; ignoring component");
        } else {
            int priority = 0;

            orte_rml_module_t* module = component->rml_init(&priority);
            if (NULL == module) {
                opal_output_verbose(10, orte_rml_base_framework.framework_output,
                                    "orte_rml_base_select: init returned failure");
                if (priority < 0) {
                    return_silent = true;
                }
                continue;
            }

            if(NULL != orte_rml_base_wrapper &&
               /* If this is a wrapper component then save it for later */
               RML_SELECT_WRAPPER_PRIORITY >= priority) {
                if( 0 == strncmp(component->rml_version.mca_component_name, 
                                 orte_rml_base_wrapper,
                                 strlen(orte_rml_base_wrapper) ) ) {
                    wrapper_component = component;
                }
            } else if (priority > selected_priority) {
                /* Otherwise this is a normal module and subject to normal selection */
                if (NULL != selected_module && NULL != selected_module->finalize) {
                    selected_module->finalize();
                }

                selected_priority = priority;
                selected_component = component;
                selected_module = module;
            }
        }
    }

    /* 
     * Unload all components that were not selected
     */
    OPAL_LIST_FOREACH_SAFE(item, next, &orte_rml_base_framework.framework_components, opal_list_item_t) {
        mca_base_component_list_item_t *cli = (mca_base_component_list_item_t *) item;
        orte_rml_component_t* component = (orte_rml_component_t *) cli->cli_component;

        /* Keep it if it is the wrapper component */
        if ((component == wrapper_component) || (component == selected_component)) {
            continue;
        }
        /* Not the selected component */
        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                            "orte_rml_base_select: module %s unloaded",
                            component->rml_version.mca_component_name);
        opal_list_remove_item(&orte_rml_base_framework.framework_components, item);
        mca_base_component_repository_release((mca_base_component_t *) component);
        OBJ_RELEASE(item);
    }

    /* setup reference to selected module */
    if (NULL != selected_module) {
        orte_rml = *selected_module;
        orte_rml_component = selected_component;
    }

    /* If a wrapper component was requested then 
     * Make sure it can switch out the selected module
     */
    if( NULL != wrapper_component) {
        wrapper_component->rml_init(NULL);
    }

    if (NULL == selected_component) {
        if (return_silent) {
            return ORTE_ERR_SILENT;
        }
        return ORTE_ERROR;
    }
    
    return ORTE_SUCCESS;
}

void orte_rml_send_callback(int status, orte_process_name_t *peer,
                            opal_buffer_t* buffer, orte_rml_tag_t tag,
                            void* cbdata)

{
    OBJ_RELEASE(buffer);
    if (ORTE_SUCCESS != status) {
        ORTE_ACTIVATE_PROC_STATE(peer, ORTE_PROC_STATE_UNABLE_TO_SEND_MSG);
    }
}

void orte_rml_recv_callback(int status, orte_process_name_t* sender,
                            opal_buffer_t *buffer,
                            orte_rml_tag_t tag, void *cbdata)
{
    orte_rml_recv_cb_t *blob = (orte_rml_recv_cb_t*)cbdata;

    /* transfer the sender */
    blob->name.jobid = sender->jobid;
    blob->name.vpid = sender->vpid;
    /* just copy the payload to the buf */
    opal_dss.copy_payload(&blob->data, buffer);
    /* flag as complete */
    blob->active = false;
}

/***   RML CLASS INSTANCES   ***/
static void send_cons(orte_rml_send_t *ptr)
{
    ptr->cbdata = NULL;
    ptr->iov = NULL;
    ptr->buffer = NULL;
    ptr->data = NULL;
}
OBJ_CLASS_INSTANCE(orte_rml_send_t,
                   opal_list_item_t,
                   send_cons, NULL);

static void send_req_cons(orte_rml_send_request_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->post, orte_rml_send_t);
}
OBJ_CLASS_INSTANCE(orte_rml_send_request_t,
                   opal_object_t,
                   send_req_cons, NULL);

static void recv_cons(orte_rml_recv_t *ptr)
{
    ptr->iov.iov_base = NULL;
}
static void recv_des(orte_rml_recv_t *ptr)
{
    if (NULL != ptr->iov.iov_base) {
        free(ptr->iov.iov_base);
    }
}
OBJ_CLASS_INSTANCE(orte_rml_recv_t,
                   opal_list_item_t,
                   recv_cons, recv_des);

static void rcv_cons(orte_rml_recv_cb_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->data, opal_buffer_t);
    ptr->active = false;
}
static void rcv_des(orte_rml_recv_cb_t *ptr)
{
    OBJ_DESTRUCT(&ptr->data);
}
OBJ_CLASS_INSTANCE(orte_rml_recv_cb_t, opal_object_t,
                   rcv_cons, rcv_des);

static void prcv_cons(orte_rml_posted_recv_t *ptr)
{
    ptr->cbdata = NULL;
}
OBJ_CLASS_INSTANCE(orte_rml_posted_recv_t,
                   opal_list_item_t,
                   prcv_cons, NULL);

static void prq_cons(orte_rml_recv_request_t *ptr)
{
    ptr->cancel = false;
    ptr->post = OBJ_NEW(orte_rml_posted_recv_t);
}
static void prq_des(orte_rml_recv_request_t *ptr)
{
    if (NULL != ptr->post) {
        OBJ_RELEASE(ptr->post);
    }
}
OBJ_CLASS_INSTANCE(orte_rml_recv_request_t,
                   opal_object_t,
                   prq_cons, prq_des);

