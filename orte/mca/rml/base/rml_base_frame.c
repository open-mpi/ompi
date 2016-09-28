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

#include "opal/dss/dss.h"
#include "orte/mca/mca.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/util/output.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"

#include "orte/mca/rml/base/base.h"

/* The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct. */
#include "orte/mca/rml/base/static-components.h"


/* Initialising stub fns in the global var used by other modules */
orte_rml_base_API_t orte_rml = {
    .finalize               = orte_rml_API_finalize,
    .get_contact_info       = orte_rml_API_get_contact_info,
    .set_contact_info       = orte_rml_API_set_contact_info,
    .ping                   = orte_rml_API_ping,
    .ping_conduit           = orte_rml_API_ping_conduit,
    .send_nb                = orte_rml_API_send_nb,
    .send_buffer_nb         = orte_rml_API_send_buffer_nb,
    .send_nb_conduit        = orte_rml_API_send_nb_conduit,
    .send_buffer_nb_conduit = orte_rml_API_send_buffer_nb_conduit,
    .recv_nb                = orte_rml_API_recv_nb,
    .recv_buffer_nb         = orte_rml_API_recv_buffer_nb,
    .recv_cancel            = orte_rml_API_recv_cancel,
    .purge                  = orte_rml_API_purge,
    .query_transports       = orte_rml_API_query_transports,
    .open_conduit           = orte_rml_API_open_conduit
};

orte_rml_base_t orte_rml_base = {{{0}}};
OPAL_TIMING_DECLARE(tm_rml)

orte_rml_component_t *orte_rml_component = NULL;

static bool selected = false;

static int orte_rml_base_register(mca_base_register_flag_t flags)
{
#if OPAL_ENABLE_TIMING
    orte_rml_base.timing = false;
    (void) mca_base_var_register ("orte", "rml", "base", "timing",
                                  "Enable RML timings",
                                  MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                  &orte_rml_base.timing);
#endif

    return ORTE_SUCCESS;
}

static void cleanup(int sd, short args, void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;

    OPAL_LIST_DESTRUCT(&orte_rml_base.posted_recvs);
    if (NULL != active) {
        *active = false;
    }
}

static int orte_rml_base_close(void)
{
    volatile bool active;
    int idx, total_conduits = opal_pointer_array_get_size(&orte_rml_base.conduits);
    orte_rml_base_active_t *active_module;
    orte_rml_base_module_t *mod;

    /* close the active modules */ 
/*
    OPAL_LIST_FOREACH(active_module, &orte_rml_base.actives, orte_rml_base_active_t)
    {
        if (NULL != active_module->module->finalize) {
            active_module->module->finalize();
        }
    }
*/

     /* cycle thru the conduits opened and call each module's finalize */
     /* The components finalise/close() will be responsible for freeing the module pointers   */
    for (idx = 0; idx < total_conduits ; idx++)
    {
        if( NULL != (mod = (orte_rml_base_module_t*)opal_pointer_array_get_item(&orte_rml_base.conduits,idx))) {
          mod->finalize(mod);
        }   
                  
    }

    OPAL_LIST_DESTRUCT(&orte_rml_base.actives)

    /* because the RML posted recvs list is in a separate
     * async thread for apps, we can't just destruct it here.
     * Instead, we push it into that event thread and destruct
     * it there */
     if (ORTE_PROC_IS_APP) {
        opal_event_t ev;
        active = true;
        opal_event_set(orte_event_base, &ev, -1,
                       OPAL_EV_WRITE, cleanup, (void*)&active);
        opal_event_set_priority(&ev, ORTE_ERROR_PRI);
        opal_event_active(&ev, OPAL_EV_WRITE, 1);
        ORTE_WAIT_FOR_COMPLETION(active);
     } else {
        /* we can call the destruct directly */
        cleanup(0, 0, NULL);
     }

    OPAL_TIMING_REPORT(orte_rml_base.timing, &tm_rml);

    return mca_base_framework_components_close(&orte_rml_base_framework, NULL);
}

static int orte_rml_base_open(mca_base_open_flag_t flags)
{
    /* Initialize globals */
    /* construct object for holding the active plugin modules */
    OBJ_CONSTRUCT(&orte_rml_base.actives, opal_list_t);
    OBJ_CONSTRUCT(&orte_rml_base.posted_recvs, opal_list_t);
    OBJ_CONSTRUCT(&orte_rml_base.unmatched_msgs, opal_list_t);
    OBJ_CONSTRUCT(&orte_rml_base.conduits, opal_pointer_array_t);
    opal_pointer_array_init(&orte_rml_base.conduits,1,INT_MAX,1);

    OPAL_TIMING_INIT(&tm_rml);
    /* Open up all available components */
    return mca_base_framework_components_open(&orte_rml_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, rml, "ORTE Run-Time Messaging Layer",
                           orte_rml_base_register, orte_rml_base_open, orte_rml_base_close,
                           mca_rml_base_static_components, 0);

OBJ_CLASS_INSTANCE(orte_rml_base_active_t,
                   opal_list_item_t,
                   NULL, NULL);

/**
 * Function for ordering the component(plugin) by priority
 */
int orte_rml_base_select(void)
{
   mca_base_component_list_item_t *cli=NULL;
   mca_base_component_t *component=NULL;
   mca_base_module_t *module=NULL;
   orte_rml_base_module_t *nmodule;
   orte_rml_base_active_t *newmodule, *mod;
   int priority;
   bool inserted;
   opal_list_t *conduit_attr;

   if (selected) {
      return ORTE_SUCCESS;
   }
   selected = true;

   OPAL_LIST_FOREACH(cli, &orte_rml_base_framework.framework_components, mca_base_component_list_item_t ) {
       component = (mca_base_component_t *) cli->cli_component;

       opal_output_verbose(10, orte_rml_base_framework.framework_output,
                           "orte_rml_base_select: Initializing %s component %s",
                            component->mca_type_name,
                            component->mca_component_name);

       if (NULL == ((orte_rml_component_t *)component)->rml_init) {
           opal_output_verbose(10, orte_rml_base_framework.framework_output,
                               "orte_rml_base_select: no init function; ignoring component [%s]",
                               component->mca_component_name);
       } else {
           module = (mca_base_module_t *) ((orte_rml_component_t *)component)->rml_init(&priority);
           if (NULL == module) {
               opal_output_verbose(10, orte_rml_base_framework.framework_output,
                                   "orte_rml_base_select: init returned failure [%s]",
                                   component->mca_component_name);
               continue;
           }

           /* based on priority add it to the actives list */
           nmodule = (orte_rml_base_module_t*) module;
           /* add to the list of selected modules */
           newmodule = OBJ_NEW(orte_rml_base_active_t);
           newmodule->pri = priority;
           newmodule->module = nmodule;
           newmodule->component = component;

           /* maintain priority order */
           inserted = false;
           OPAL_LIST_FOREACH(mod, &orte_rml_base.actives, orte_rml_base_active_t) {
               if (priority > mod->pri) {
                   opal_list_insert_pos(&orte_rml_base.actives,
                                (opal_list_item_t*)mod, &newmodule->super);
                   inserted = true;
                   break;
               }
           }
           if (!inserted) {
               /* must be lowest priority - add to end */
               opal_list_append(&orte_rml_base.actives, &newmodule->super);
           }
       }
   }
   if (4 < opal_output_get_verbosity(orte_rml_base_framework.framework_output)) {
        opal_output(0, "%s: Final rml priorities", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* show the prioritized list */
        OPAL_LIST_FOREACH(mod, &orte_rml_base.actives, orte_rml_base_active_t) {
            opal_output(0, "\tComponent: %s Priority: %d", mod->component->mca_component_name, mod->pri);
        }
   }
   /* Open the default oob conduit */
   opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Opening the default conduit - oob component",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
   conduit_attr = OBJ_NEW(opal_list_t);
   if( ORTE_SUCCESS == 
            ( orte_set_attribute( conduit_attr, ORTE_RML_INCLUDE_COMP_ATTRIB, ORTE_ATTR_GLOBAL,"oob",OPAL_STRING)))   {
        orte_rml_base.def_conduit_id = orte_rml_API_open_conduit(conduit_attr);
        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Default conduit (oob) opened with conduit id = %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), orte_rml_base.def_conduit_id);
   } else {
       opal_output_verbose(1, orte_rml_base_framework.framework_output,
                         "%s Failed to set attributes, default conduit (oob) could not be opened",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
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
    ptr->seq_num = 0xFFFFFFFF;
}
OBJ_CLASS_INSTANCE(orte_rml_send_t,
                   opal_list_item_t,
                   send_cons, NULL);


static void send_req_cons(orte_rml_send_request_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->send, orte_rml_send_t);
}
OBJ_CLASS_INSTANCE(orte_rml_send_request_t,
                   opal_object_t,
                   send_req_cons, NULL);

static void recv_cons(orte_rml_recv_t *ptr)
{
    ptr->iov.iov_base = NULL;
    ptr->iov.iov_len = 0;
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
