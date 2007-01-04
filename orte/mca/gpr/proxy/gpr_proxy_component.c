/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI General Purpose Registry - Proxy component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"
#include "orte/dss/dss.h"
#include "opal/util/output.h"
#include "orte/util/proc_info.h"

#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/oob/oob_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"

#include "gpr_proxy.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_gpr_base_component_t mca_gpr_proxy_component = {
    {
    MCA_GPR_BASE_VERSION_1_0_0,

    "proxy", /* MCA module name */
    ORTE_MAJOR_VERSION,  /* MCA module major version */
    ORTE_MINOR_VERSION,  /* MCA module minor version */
    ORTE_RELEASE_VERSION,  /* MCA module release version */
    orte_gpr_proxy_open,  /* module open */
    orte_gpr_proxy_close /* module close */
    },
    {
    false /* checkpoint / restart */
    },
    orte_gpr_proxy_component_init,    /* module init */
    orte_gpr_proxy_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static orte_gpr_base_module_t orte_gpr_proxy = {
   /* INIT */
    orte_gpr_proxy_module_init,
   /* BLOCKING OPERATIONS */
    orte_gpr_proxy_get,
    orte_gpr_proxy_get_conditional,
    orte_gpr_proxy_put,
    orte_gpr_base_put_1,
    orte_gpr_base_put_N,
    orte_gpr_proxy_delete_entries,
    orte_gpr_proxy_delete_segment,
    orte_gpr_proxy_index,
    /* NON-BLOCKING OPERATIONS */
    orte_gpr_proxy_get_nb,
    orte_gpr_proxy_put_nb,
    orte_gpr_proxy_delete_entries_nb,
    orte_gpr_proxy_delete_segment_nb,
    orte_gpr_proxy_index_nb,
    /* GENERAL OPERATIONS */
    orte_gpr_base_create_value,
    orte_gpr_base_create_keyval,
    orte_gpr_proxy_preallocate_segment,
    orte_gpr_proxy_deliver_notify_msg,
    /* ARITHMETIC OPERATIONS */
    orte_gpr_proxy_increment_value,
    orte_gpr_proxy_decrement_value,
    /* SUBSCRIBE OPERATIONS */
    orte_gpr_proxy_subscribe,
    orte_gpr_base_subscribe_1,
    orte_gpr_base_subscribe_N,
    orte_gpr_base_define_trigger,
    orte_gpr_base_define_trigger_level,
    orte_gpr_proxy_unsubscribe,
    orte_gpr_proxy_cancel_trigger,
    /* COMPOUND COMMANDS */
    orte_gpr_proxy_begin_compound_cmd,
    orte_gpr_proxy_stop_compound_cmd,
    orte_gpr_proxy_exec_compound_cmd,
    /* DIAGNOSTIC OPERATIONS */
    orte_gpr_proxy_dump_all,
    orte_gpr_proxy_dump_segments,
    orte_gpr_proxy_dump_triggers,
    orte_gpr_proxy_dump_subscriptions,
    orte_gpr_proxy_dump_a_trigger,
    orte_gpr_proxy_dump_a_subscription,
    orte_gpr_proxy_dump_local_triggers,
    orte_gpr_proxy_dump_local_subscriptions,
    orte_gpr_proxy_dump_callbacks,
    orte_gpr_proxy_dump_notify_msg,
    orte_gpr_proxy_dump_notify_data,
    orte_gpr_proxy_dump_value,
    orte_gpr_proxy_dump_segment_size,
    /* CLEANUP OPERATIONS */
    orte_gpr_proxy_cleanup_job,
    orte_gpr_proxy_cleanup_proc
};


/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/*
 * globals needed within proxy component
 */
orte_gpr_proxy_globals_t orte_gpr_proxy_globals;

/* SUBSCRIBER */
/* constructor - used to initialize subscriber instance */
static void orte_gpr_proxy_subscriber_construct(orte_gpr_proxy_subscriber_t* req)
{
    req->callback = NULL;
    req->user_tag = NULL;
    req->id = 0;
    req->name = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_proxy_subscriber_destructor(orte_gpr_proxy_subscriber_t* req)
{
    if (NULL != req->name) free(req->name);
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
            orte_gpr_proxy_subscriber_t,            /* type name */
            opal_object_t,                          /* parent "class" name */
            orte_gpr_proxy_subscriber_construct,    /* constructor */
            orte_gpr_proxy_subscriber_destructor);  /* destructor */


/* TRIGGER */
/* constructor - used to initialize trigger instance */
static void orte_gpr_proxy_trigger_construct(orte_gpr_proxy_trigger_t* req)
{
    req->callback = NULL;
    req->user_tag = NULL;
    req->id = 0;
    req->name = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_proxy_trigger_destructor(orte_gpr_proxy_trigger_t* req)
{
    if (NULL != req->name) free(req->name);
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
            orte_gpr_proxy_trigger_t,            /* type name */
            opal_object_t,                          /* parent "class" name */
            orte_gpr_proxy_trigger_construct,    /* constructor */
            orte_gpr_proxy_trigger_destructor);  /* destructor */


/*
 * Open the component
 */
int orte_gpr_proxy_open(void)
{
    int id, tmp;

    id = mca_base_param_register_int("gpr", "proxy", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &tmp);
    if (tmp) {
        orte_gpr_proxy_globals.debug = true;
    } else {
        orte_gpr_proxy_globals.debug = false;
    }

    return ORTE_SUCCESS;
}

/*
 * Close the component
 */
int orte_gpr_proxy_close(void)
{
    return ORTE_SUCCESS;
}

orte_gpr_base_module_t*
orte_gpr_proxy_component_init(bool *allow_multi_user_threads, bool *have_hidden_threads,
                            int *priority)
{
    orte_process_name_t name;
    int ret, value;

    if (orte_gpr_proxy_globals.debug) {
        opal_output(0, "gpr_proxy_init called");
    }

    /* If we are NOT to host a replica, then we want to be selected, so do all
       the setup and return the module */
    if (NULL != orte_process_info.gpr_replica_uri) {

        if (orte_gpr_proxy_globals.debug) {
            opal_output(0, "[%lu,%lu,%lu] gpr_proxy_init: proxy selected",
                        ORTE_NAME_ARGS(orte_process_info.my_name));
        }

        /* setup the replica location */
       if(ORTE_SUCCESS != (ret = orte_rml.parse_uris(orte_process_info.gpr_replica_uri, &name, NULL))) {
           ORTE_ERROR_LOG(ret);
           return NULL;
       }
       if(ORTE_SUCCESS != (ret = orte_dss.copy((void**)&orte_process_info.gpr_replica, &name, ORTE_NAME))) {
           ORTE_ERROR_LOG(ret);
           return NULL;
        }

        /* Return a module (choose an arbitrary, positive priority --
           it's only relevant compared to other ns components).  If
           we're not the seed, then we don't want to be selected, so
           return NULL. */

        *priority = 10;

        /* We allow multi user threads but don't have any hidden threads */

        *allow_multi_user_threads = true;
        *have_hidden_threads = false;

        /* setup thread locks and condition variable */
        OBJ_CONSTRUCT(&orte_gpr_proxy_globals.mutex, opal_mutex_t);
        OBJ_CONSTRUCT(&orte_gpr_proxy_globals.wait_for_compound_mutex, opal_mutex_t);
        OBJ_CONSTRUCT(&orte_gpr_proxy_globals.compound_cmd_condition, opal_condition_t);

        /* initialize the registry compound mode */
        orte_gpr_proxy_globals.compound_cmd_mode = false;
        orte_gpr_proxy_globals.compound_cmd_waiting = 0;
        orte_gpr_proxy_globals.compound_cmd = NULL;

        /* initialize the subscription tracker */
        if (ORTE_SUCCESS != (ret = orte_pointer_array_init(&(orte_gpr_proxy_globals.subscriptions),
                                (orte_std_cntr_t)orte_gpr_array_block_size,
                                (orte_std_cntr_t)orte_gpr_array_max_size,
                                (orte_std_cntr_t)orte_gpr_array_block_size))) {
            ORTE_ERROR_LOG(ret);
            return NULL;
        }
        orte_gpr_proxy_globals.num_subs = 0;

        /* initialize the trigger counter */
        if (ORTE_SUCCESS != (ret = orte_pointer_array_init(&(orte_gpr_proxy_globals.triggers),
                                (orte_std_cntr_t)orte_gpr_array_block_size,
                                (orte_std_cntr_t)orte_gpr_array_max_size,
                                (orte_std_cntr_t)orte_gpr_array_block_size))) {
            ORTE_ERROR_LOG(ret);
            return NULL;
        }
        orte_gpr_proxy_globals.num_trigs = 0;

        /* check to see if we want timing information */
        mca_base_param_reg_int_name("orte", "timing",
                                    "Request that critical timing loops be measured",
                                    false, false, 0, &value);
        if (value != 0) {
            orte_gpr_proxy_globals.timing = true;
        }

        initialized = true;
        return &orte_gpr_proxy;
    } else {
        return NULL;
    }
}

int orte_gpr_proxy_module_init(void)
{
    /* issue the non-blocking receive */
    int rc;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_GPR_NOTIFY, ORTE_RML_PERSISTENT, orte_gpr_proxy_notify_recv, NULL);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}


/*
 * finalize routine
 */
int orte_gpr_proxy_finalize(void)
{
    orte_std_cntr_t i;
    orte_gpr_subscription_id_t j;
    orte_gpr_trigger_id_t k;
    orte_gpr_proxy_subscriber_t **lsubs;
    orte_gpr_proxy_trigger_t **ltrigs;
    
    if (orte_gpr_proxy_globals.debug) {
       opal_output(0, "[%lu,%lu,%lu] gpr_proxy_finalize called",
                        ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    if (initialized) {
        /* destruct the mutex and condition variables */
        OBJ_DESTRUCT(&orte_gpr_proxy_globals.mutex);
        OBJ_DESTRUCT(&orte_gpr_proxy_globals.wait_for_compound_mutex);
        OBJ_DESTRUCT(&orte_gpr_proxy_globals.compound_cmd_condition);

        /* clear the local subscriptions and triggers */
        if (NULL != orte_gpr_proxy_globals.subscriptions) {
            lsubs = (orte_gpr_proxy_subscriber_t**)(orte_gpr_proxy_globals.subscriptions)->addr;
            for (i=0, j=0; j < orte_gpr_proxy_globals.num_subs &&
                        i < (orte_gpr_proxy_globals.subscriptions)->size; i++) {
                if (NULL != lsubs[i]) {
                    j++;
                    OBJ_RELEASE(lsubs[i]);
                }
            }
            OBJ_RELEASE(orte_gpr_proxy_globals.subscriptions);
        }
    
        ltrigs = (orte_gpr_proxy_trigger_t**)(orte_gpr_proxy_globals.triggers)->addr;
        if (NULL != orte_gpr_proxy_globals.triggers) {
            for (i=0, k=0; k < orte_gpr_proxy_globals.num_trigs &&
                        i < (orte_gpr_proxy_globals.triggers)->size; i++) {
                if (NULL != ltrigs[i]) {
                    k++;
                    OBJ_RELEASE(ltrigs[i]);
                }
            }
            OBJ_RELEASE(orte_gpr_proxy_globals.triggers);
        }

       initialized = false;
    }

    /* All done */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_GPR_NOTIFY);
    return ORTE_SUCCESS;
}

/*
 * handle notify messages from replicas
 */

void orte_gpr_proxy_notify_recv(int status, orte_process_name_t* sender,
                   orte_buffer_t *buffer, orte_rml_tag_t tag,
                   void* cbdata)
{
    orte_gpr_cmd_flag_t command;
    orte_gpr_notify_message_t *msg;
    orte_std_cntr_t n;
    int rc;

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto RETURN_ERROR;
    }

    if (ORTE_GPR_NOTIFY_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        goto RETURN_ERROR;
    }

    msg = OBJ_NEW(orte_gpr_notify_message_t);
    if (NULL == msg) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        goto RETURN_ERROR;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &msg, &n, ORTE_GPR_NOTIFY_MSG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        goto RETURN_ERROR;
    }

    /* process the message */
    if (ORTE_SUCCESS != (rc = orte_gpr_proxy_deliver_notify_msg(msg))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        goto RETURN_ERROR;
     }

    /* release data */
    OBJ_RELEASE(msg);


RETURN_ERROR:
    return;
}

