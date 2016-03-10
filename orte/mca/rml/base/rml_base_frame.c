/*
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Intel Corporation.  All rights reserved.
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
orte_rml_API_module_t orte_rml = {
 orte_rml_API_enable_comm,
 orte_rml_API_finalize,
 orte_rml_API_get_contact_info,
 orte_rml_API_set_contact_info,
 orte_rml_API_ping,
 orte_rml_API_send_nb,
 orte_rml_API_send_buffer_nb,
 orte_rml_API_recv_nb,
 orte_rml_API_recv_buffer_nb,
 orte_rml_API_recv_cancel,
 orte_rml_API_add_exception_handler,
 orte_rml_API_del_exception_handler,
 orte_rml_API_ft_event,
 orte_rml_API_purge,
 orte_rml_API_open_channel,
 orte_rml_API_send_channel_nb,
 orte_rml_API_send_buffer_channel_nb,
 orte_rml_API_close_channel
 };        

orte_rml_base_t   orte_rml_base = {{{0}}};
OPAL_TIMING_DECLARE(tm_rml)

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
    bool *active = (bool*)cbdata;

    OPAL_LIST_DESTRUCT(&orte_rml_base.posted_recvs);
    if (NULL != active) {
        *active = false;
    }
}

static int orte_rml_base_close(void)
{
    bool active;

     orte_rml_base_active_t *active_module;

    /*close the active modules */
    OPAL_LIST_FOREACH(active_module, &orte_rml_base.actives, orte_rml_base_active_t)
    {
        if (NULL != active_module->module->base_finalize) {
            active_module->module->base_finalize();
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
                       OPAL_EV_WRITE, cleanup, &active);
        opal_event_set_priority(&ev, ORTE_ERROR_PRI);
        opal_event_active(&ev, OPAL_EV_WRITE, 1);
        ORTE_WAIT_FOR_COMPLETION(active);
     } else {
        /* we can call the destruct directly */
        cleanup(0, 0, NULL);
     }

    OPAL_TIMING_REPORT(orte_rml_base.timing, &tm_rml);
    OBJ_DESTRUCT(&orte_rml_base.open_channels);

    return mca_base_framework_components_close(&orte_rml_base_framework, NULL);
}

static int orte_rml_base_open(mca_base_open_flag_t flags)
{
    /* Initialize globals */
    /* construct object for holding the active plugin modules */
    OBJ_CONSTRUCT(&orte_rml_base.actives, opal_list_t);
    OBJ_CONSTRUCT(&orte_rml_base.posted_recvs, opal_list_t);
    OBJ_CONSTRUCT(&orte_rml_base.unmatched_msgs, opal_list_t);
    OBJ_CONSTRUCT(&orte_rml_base.open_channels, opal_pointer_array_t);
    if (OPAL_SUCCESS != opal_pointer_array_init(&orte_rml_base.open_channels, 0,
                                                INT_MAX, 1)) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
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

/* 
 * The stub API interface implementation that cycles through the active list and 
 * calls into the plugin with highest priority that implements it.
 */

/** Enable communication once a process name has been assigned */
int orte_rml_API_enable_comm(void)
{
    int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:enable_comm calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_enable_comm) {
            if (ORTE_SUCCESS == (rc = active->module->base_enable_comm())) {
                break;
            }
        }
    }
    return rc;
}

/** Shutdown the communication system and clean up resources */
int orte_rml_API_finalize(void)
{
    int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:finalize() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_finalize) {
            if (ORTE_SUCCESS == (rc = active->module->base_finalize())) {
                break;
            }
        }
    }
    return rc;

}

/** Get contact information for local process */
char* orte_rml_API_get_contact_info(void)
{
    char* rc = NULL;
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:get_contact_info() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_get_contact_info) {
            if (ORTE_SUCCESS == (rc = active->module->base_get_contact_info())) {
                break;
            }
        }
    }
   return rc;
}

/** Set contact information for remote process */
void orte_rml_API_set_contact_info(const char *contact_info)
{
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:set_contact_info() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_set_contact_info) {
                return active->module->base_set_contact_info(contact_info);
        }
    }

}

/** Ping process for connectivity check */
int orte_rml_API_ping(const char* contact_info, const struct timeval* tv) {
    int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:ping() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_ping) {
            if (ORTE_SUCCESS == (rc = active->module->base_ping(contact_info,tv))) {
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
    int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:send_nb() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_send_nb) {
            if (ORTE_SUCCESS == (rc = active->module->base_send_nb(peer,msg,count,tag,cbfunc,cbdata))) {
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
    int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:send_buffer_nb() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_send_buffer_nb) {
            if (ORTE_SUCCESS == (rc = active->module->base_send_buffer_nb(peer,buffer,tag,cbfunc,cbdata))) {
                break;
            }
        }
    }
    return rc;
}


/** Receive non-blocking iovec message */
void orte_rml_API_recv_nb(orte_process_name_t* peer,
             orte_rml_tag_t tag,
             bool persistent,
             orte_rml_callback_fn_t cbfunc,
             void* cbdata)
{
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:recv_nb() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_recv_nb) {
                return active->module->base_recv_nb(peer,tag,persistent,cbfunc,cbdata);
        }
    }
}

/** Receive non-blocking buffer message */
void orte_rml_API_recv_buffer_nb(orte_process_name_t* peer,
                    orte_rml_tag_t tag,
                    bool persistent,
                    orte_rml_buffer_callback_fn_t cbfunc,
                    void* cbdata)
{ opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:recv_buffer_nb() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_recv_buffer_nb) {
                return active->module->base_recv_buffer_nb(peer,tag,persistent,cbfunc,cbdata);
        }
    }
}

/** Cancel posted non-blocking receive */
void orte_rml_API_recv_cancel(orte_process_name_t* peer, orte_rml_tag_t tag)
{
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:recv_cancel() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_recv_cancel) {
                return active->module->base_recv_cancel(peer,tag);
        }
    }

}

/** Add callback for communication exception */
int orte_rml_API_add_exception_handler(orte_rml_exception_callback_t cbfunc)
{
   int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:add_exception_handler() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_add_exception_handler) {
            if (ORTE_SUCCESS == (rc = active->module->base_add_exception_handler(cbfunc))) {
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
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:del_exception_handler() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_del_exception_handler) {
            if (ORTE_SUCCESS == (rc = active->module->base_del_exception_handler(cbfunc))) {
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
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:ft_event() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_ft_event) {
            if (ORTE_SUCCESS == (rc = active->module->base_ft_event(state))) {
                break;
            }
        }
    }
    return rc;

}


/** Purge information */
void orte_rml_API_purge(orte_process_name_t *peer)
{
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:purge() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_purge) {
                return active->module->base_purge(peer);
        }
    }

}

/** Open a qos messaging channel to a peer*/
int orte_rml_API_open_channel(orte_process_name_t* peer,
                 opal_list_t *qos_attributes,
                 orte_rml_channel_callback_fn_t cbfunc,
                 void* cbdata)
{
    int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:open_channel() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_open_channel) {
            if (ORTE_SUCCESS == (rc = active->module->base_open_channel(peer,qos_attributes,cbfunc,cbdata))) {
                break;
            }
        }
    }
    return rc;

}

/** send a non blocking iovec message over a channel */
int orte_rml_API_send_channel_nb(orte_rml_channel_num_t channel,
                    struct iovec* msg,
                    int count,
                    orte_rml_tag_t tag,
                    orte_rml_send_channel_callback_fn_t cbfunc,
                    void* cbdata)
{
    int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:send_channel_nb() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_send_channel_nb) {
            if (ORTE_SUCCESS == (rc = active->module->base_send_channel_nb(channel,msg,count,tag,cbfunc,cbdata))) {
                break;
            }
        }
    }
    return rc;
}

/** send a non blocking buffer message over a channel */
int orte_rml_API_send_buffer_channel_nb(orte_rml_channel_num_t channel,
                           struct opal_buffer_t * buffer,
                           orte_rml_tag_t tag,
                           orte_rml_send_buffer_channel_callback_fn_t cbfunc,
                           void* cbdata)
{
int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:send_buffer_channel_nb() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_send_buffer_channel_nb) {
            if (ORTE_SUCCESS == (rc = active->module->base_send_buffer_channel_nb(channel,buffer,tag,cbfunc,cbdata))) {
                break;
            }
        }
    }
    return rc;
}

/** close a qos messaging channel */
int orte_rml_API_close_channel(orte_rml_channel_num_t channel_num,
                  orte_rml_channel_callback_fn_t cbfunc,
                  void* cbdata)
{
   int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_rml_base_active_t *active;

    OPAL_OUTPUT_VERBOSE((1,orte_rml_base_framework.framework_output,
                         "%s rml:base:close_channel() - calling the respective plugin that implements this",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_rml_base.actives, orte_rml_base_active_t) {
        if (NULL != active->module->base_close_channel) {
            if (ORTE_SUCCESS == (rc = active->module->base_close_channel(channel_num,cbfunc,cbdata))) {
                break;
            }
        }
    }
    return rc;
}

/**
 * Function for selecting one component(plugin) from all those that are
 * available.
 */
int orte_rml_base_select(void)
{
   mca_base_component_list_item_t *cli=NULL;
   mca_base_component_t *component=NULL;
   mca_base_module_t *module=NULL;
   orte_rml_base_module_t *nmodule;
   orte_rml_base_active_t *newmodule, *mod;
   int rc, priority;
   bool inserted;

   orte_rml_component_t *wrapper_component = NULL;
    
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
                               "orte_rml_base_select: no init function; ignoring component [%s]",component->mca_component_name);
       } else {
           module = (mca_base_module_t *) ((orte_rml_component_t *)component)->rml_init(&priority);
           if (NULL == module) {
               opal_output_verbose(10, orte_rml_base_framework.framework_output,
                                   "orte_rml_base_select: init returned failure [%s]",component->mca_component_name);
               continue;
           }
            
           if (NULL != orte_rml_base_wrapper &&
               // If this is a wrapper component then save it for later
               RML_SELECT_WRAPPER_PRIORITY >= priority) {
               if ( 0 == strncmp(component->mca_component_name,
                                orte_rml_base_wrapper,
                                strlen(orte_rml_base_wrapper) ) ) {
                   wrapper_component = (orte_rml_component_t *) component;
               }
           }
           else
           {
               /* This is normal plugin component - based on priority add it to the actives list */
               nmodule = (orte_rml_base_module_t*) module;
               /* if the module fails to init, skip it */
               if (NULL == nmodule->base_enable_comm || ORTE_SUCCESS != nmodule->base_enable_comm()) {
                   continue;
               }

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
   }
   if (4 < opal_output_get_verbosity(orte_rml_base_framework.framework_output)) {
        opal_output(0, "%s: Final rml priorities", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* show the prioritized list */
        OPAL_LIST_FOREACH(mod, &orte_rml_base.actives, orte_rml_base_active_t) {
            opal_output(0, "\tComponent: %s Priority: %d", mod->component->mca_component_name, mod->pri);
        }
   }

   /* If a wrapper component was requested then
    * Make sure it can switch out the selected module
    */
   if( NULL != wrapper_component) {
        wrapper_component->rml_init(NULL);
   }
    
    /* Post a persistent recieve for open channel request */
    orte_rml.recv_buffer_nb (ORTE_NAME_WILDCARD, ORTE_RML_TAG_OPEN_CHANNEL_REQ,
                             ORTE_RML_PERSISTENT, orte_rml_open_channel_recv_callback,
                             NULL);
    /* post a persistent recieve for close channel request */
    orte_rml.recv_buffer_nb (ORTE_NAME_WILDCARD, ORTE_RML_TAG_CLOSE_CHANNEL_REQ,
                             ORTE_RML_PERSISTENT, orte_rml_close_channel_recv_callback,
                             NULL);

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
    ptr->channel = NULL;
    ptr->dst_channel = ORTE_RML_INVALID_CHANNEL_NUM;
    ptr->seq_num = 0xFFFFFFFF;
}
OBJ_CLASS_INSTANCE(orte_rml_send_t,
                   opal_list_item_t,
                   send_cons, NULL);

static void channel_cons(orte_rml_channel_t *ptr)
{
    ptr->channel_num = ORTE_RML_INVALID_CHANNEL_NUM;
    ptr->qos = NULL;
    ptr->qos_channel_ptr = NULL;
    ptr->recv = false;
}

OBJ_CLASS_INSTANCE(orte_rml_channel_t,
                   opal_list_item_t,
                   channel_cons, NULL);

static void open_channel_cons(orte_rml_open_channel_t *ptr)
{
    ptr->cbdata = NULL;
    ptr->qos_attributes = NULL;
}
OBJ_CLASS_INSTANCE(orte_rml_open_channel_t,
                   opal_list_item_t,
                   open_channel_cons, NULL);

static void close_channel_cons(orte_rml_close_channel_t *ptr)
{
    ptr->cbdata = NULL;
    ptr->channel = NULL;
}
OBJ_CLASS_INSTANCE(orte_rml_close_channel_t,
                   opal_list_item_t,
                   close_channel_cons, NULL);

static void send_req_cons(orte_rml_send_request_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->post.send, orte_rml_send_t);
    OBJ_CONSTRUCT(&ptr->post.open_channel, orte_rml_open_channel_t);
}
OBJ_CLASS_INSTANCE(orte_rml_send_request_t,
                   opal_object_t,
                   send_req_cons, NULL);

static void recv_cons(orte_rml_recv_t *ptr)
{
    ptr->iov.iov_base = NULL;
    ptr->iov.iov_len = 0;
    ptr->channel_num = ORTE_RML_INVALID_CHANNEL_NUM;
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
