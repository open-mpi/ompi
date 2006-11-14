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
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "orte/class/orte_bitmap.h"
#include "opal/class/opal_object.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/util/proc_info.h"

#include "orte/mca/rml/rml.h"

#include "orte/mca/gpr/replica/gpr_replica.h"
#include "orte/mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "orte/mca/gpr/replica/communications/gpr_replica_comm.h"
#include "orte/mca/errmgr/errmgr.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_gpr_base_component_t mca_gpr_replica_component = {
    {
    MCA_GPR_BASE_VERSION_1_0_0,

    "replica", /* MCA module name */
    ORTE_MAJOR_VERSION,  /* MCA module major version */
    ORTE_MINOR_VERSION,  /* MCA module minor version */
    ORTE_RELEASE_VERSION,  /* MCA module release version */
    orte_gpr_replica_open,  /* module open */
    orte_gpr_replica_close /* module close */
    },
    {
    false /* checkpoint / restart */
    },
    orte_gpr_replica_init,    /* module init */
    orte_gpr_replica_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static orte_gpr_base_module_t orte_gpr_replica_module = {
    /* INIT */
    orte_gpr_replica_module_init,
    /* BLOCKING OPERATIONS */
    orte_gpr_replica_get,
    orte_gpr_replica_get_conditional,
    orte_gpr_replica_put,
    orte_gpr_base_put_1,
    orte_gpr_base_put_N,
    orte_gpr_replica_delete_entries,
    orte_gpr_replica_delete_segment,
    orte_gpr_replica_index,
    /* NON-BLOCKING OPERATIONS */
    orte_gpr_replica_get_nb,
    orte_gpr_replica_put_nb,
    orte_gpr_replica_delete_entries_nb,
    orte_gpr_replica_delete_segment_nb,
    orte_gpr_replica_index_nb,
    /* GENERAL OPERATIONS */
    orte_gpr_base_create_value,
    orte_gpr_base_create_keyval,
    orte_gpr_replica_preallocate_segment,
    orte_gpr_replica_deliver_notify_msg,
    /* ARITHMETIC OPERATIONS */
    orte_gpr_replica_increment_value,
    orte_gpr_replica_decrement_value,
    /* SUBSCRIBE OPERATIONS */
    orte_gpr_replica_subscribe,
    orte_gpr_base_subscribe_1,
    orte_gpr_base_subscribe_N,
    orte_gpr_base_define_trigger,
    orte_gpr_base_define_trigger_level,
    orte_gpr_replica_unsubscribe,
    orte_gpr_replica_cancel_trigger,
    /* COMPOUND COMMANDS */
    orte_gpr_replica_begin_compound_cmd,
    orte_gpr_replica_stop_compound_cmd,
    orte_gpr_replica_exec_compound_cmd,
    /* DIAGNOSTIC OPERATIONS */
    orte_gpr_replica_dump_all,
    orte_gpr_replica_dump_segments,
    orte_gpr_replica_dump_triggers,
    orte_gpr_replica_dump_subscriptions,
    orte_gpr_replica_dump_a_trigger,
    orte_gpr_replica_dump_a_subscription,
    orte_gpr_replica_dump_local_triggers,
    orte_gpr_replica_dump_local_subscriptions,
    orte_gpr_replica_dump_callbacks,
    orte_gpr_replica_dump_notify_msg,
    orte_gpr_replica_dump_notify_data,
    orte_gpr_replica_dump_value,
    orte_gpr_replica_dump_segment_size,
    /* CLEANUP OPERATIONS */
    orte_gpr_replica_cleanup_job,
    orte_gpr_replica_cleanup_proc
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;


/*
 * globals needed within replica component
 */
orte_gpr_replica_t orte_gpr_replica;

orte_gpr_replica_globals_t orte_gpr_replica_globals;

/* instantiate the classes */
#include "orte/mca/gpr/replica/gpr_replica_class_instances.h"

int orte_gpr_replica_open(void)
{
    int id, tmp;

    OPAL_TRACE(5);

    id = mca_base_param_register_int("gpr", "replica", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &tmp);
    if (tmp) {
        orte_gpr_replica_globals.debug = true;
    } else {
        orte_gpr_replica_globals.debug = false;
    }

    id = mca_base_param_register_int("gpr", "replica", "isolate", NULL, 0);
    mca_base_param_lookup_int(id, &tmp);
    if (tmp) {
        orte_gpr_replica_globals.isolate = true;
    } else {
        orte_gpr_replica_globals.isolate = false;
    }

    return ORTE_SUCCESS;
}

/*
 * close function
 */
int orte_gpr_replica_close(void)
{
    OPAL_TRACE(5);

    return ORTE_SUCCESS;
}

orte_gpr_base_module_t *orte_gpr_replica_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority)
{
    int rc;

    OPAL_TRACE(5);

    /* If we are to host a replica, then we want to be selected, so do all the
       setup and return the module */

    if (NULL == orte_process_info.gpr_replica_uri) {

        /* Return a module (choose an arbitrary, positive priority --
           it's only relevant compared to other ns components).  If
           we're not the seed, then we don't want to be selected, so
           return NULL. */

        *priority = 50;

        /* We allow multi user threads but don't have any hidden threads */

        *allow_multi_user_threads = true;
        *have_hidden_threads = false;

        /* setup the thread locks and condition variables */
        OBJ_CONSTRUCT(&orte_gpr_replica_globals.mutex, opal_mutex_t);

        /* initialize the registry head */
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica.segments),
                                (orte_std_cntr_t)orte_gpr_array_block_size,
                                (orte_std_cntr_t)orte_gpr_array_max_size,
                                (orte_std_cntr_t)orte_gpr_array_block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica.num_segs = 0;

        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica.triggers),
                                (orte_std_cntr_t)orte_gpr_array_block_size,
                                (orte_std_cntr_t)orte_gpr_array_max_size,
                                (orte_std_cntr_t)orte_gpr_array_block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica.num_trigs = 0;

        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica.subscriptions),
                                (orte_std_cntr_t)orte_gpr_array_block_size,
                                (orte_std_cntr_t)orte_gpr_array_max_size,
                                (orte_std_cntr_t)orte_gpr_array_block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica.num_subs = 0;

        /* initialize the callback list head */
        OBJ_CONSTRUCT(&orte_gpr_replica.callbacks, opal_list_t);
        orte_gpr_replica.processing_callbacks = false;

        /* initialize the local subscription and trigger trackers */
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(
                                &(orte_gpr_replica_globals.local_subscriptions),
                                (orte_std_cntr_t)orte_gpr_array_block_size,
                                (orte_std_cntr_t)orte_gpr_array_max_size,
                                (orte_std_cntr_t)orte_gpr_array_block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica_globals.num_local_subs = 0;

        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(
                                &(orte_gpr_replica_globals.local_triggers),
                                (orte_std_cntr_t)orte_gpr_array_block_size,
                                (orte_std_cntr_t)orte_gpr_array_max_size,
                                (orte_std_cntr_t)orte_gpr_array_block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica_globals.num_local_trigs = 0;

        /* initialize the search arrays for temporarily storing search results */
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica_globals.sub_ptrs),
                                100, (orte_std_cntr_t)orte_gpr_array_max_size, 100))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }

        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica_globals.srch_cptr),
                                100, (orte_std_cntr_t)orte_gpr_array_max_size, 100))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica_globals.num_srch_cptr = 0;

        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica_globals.overwritten),
                                20, (orte_std_cntr_t)orte_gpr_array_max_size, 20))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica_globals.num_overwritten = 0;

        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica_globals.srch_ival),
                                100, (orte_std_cntr_t)orte_gpr_array_max_size, 100))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica_globals.num_srch_ival = 0;

        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica_globals.acted_upon),
                                100, (orte_std_cntr_t)orte_gpr_array_max_size, 100))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica_globals.num_acted_upon = 0;

        OBJ_CONSTRUCT(&(orte_gpr_replica_globals.srch_itag), orte_bitmap_t);

        if (orte_gpr_replica_globals.debug) {
            opal_output(0, "nb receive setup");
        }

        /* Return the module */

        initialized = true;
        return &orte_gpr_replica_module;
    } else {
        return NULL;
    }
}


int orte_gpr_replica_module_init(void)
{
    OPAL_TRACE(5);

    /* issue the non-blocking receive */
    if (!orte_gpr_replica_globals.isolate) {
        int rc = orte_rml.recv_buffer_nb(
            ORTE_NAME_WILDCARD, ORTE_RML_TAG_GPR, ORTE_RML_PERSISTENT, orte_gpr_replica_recv, NULL);
        if(rc < 0) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    return ORTE_SUCCESS;
}


/*
 * finalize routine
 */
int orte_gpr_replica_finalize(void)
{
    orte_std_cntr_t i;
    orte_gpr_subscription_id_t j;
    orte_gpr_trigger_id_t k;
    orte_gpr_replica_segment_t** seg;
    orte_gpr_replica_trigger_t** trig;
    orte_gpr_replica_subscription_t** subs;
    orte_gpr_replica_callbacks_t* cb;
    orte_gpr_replica_local_subscriber_t **lsubs;
    orte_gpr_replica_local_trigger_t **ltrigs;

    OPAL_TRACE(5);

    /* destruct the thread lock */
    OBJ_DESTRUCT(&orte_gpr_replica_globals.mutex);

    if (NULL != orte_gpr_replica.segments) {
        seg = (orte_gpr_replica_segment_t**)(orte_gpr_replica.segments)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_segs &&
                    i < (orte_gpr_replica.segments)->size; i++) {
            if (NULL != seg[i]) {
                j++;
                OBJ_RELEASE(seg[i]);
            }
        }
        OBJ_RELEASE(orte_gpr_replica.segments);
    }

    if (NULL != orte_gpr_replica.triggers) {
        trig = (orte_gpr_replica_trigger_t**)(orte_gpr_replica.triggers)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_trigs &&
                    i < (orte_gpr_replica.triggers)->size; i++) {
            if (NULL != trig[i]) {
                j++;
                OBJ_RELEASE(trig[i]);
            }
        }
        OBJ_RELEASE(orte_gpr_replica.triggers);
    }

    if (NULL != orte_gpr_replica.subscriptions) {
        subs = (orte_gpr_replica_subscription_t**)(orte_gpr_replica.subscriptions)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_subs &&
                    i < (orte_gpr_replica.subscriptions)->size; i++) {
            if (NULL != subs[i]) {
                j++;
                OBJ_RELEASE(subs[i]);
            }
        }
        OBJ_RELEASE(orte_gpr_replica.subscriptions);
    }

    while (NULL != (cb = (orte_gpr_replica_callbacks_t*)opal_list_remove_first(&orte_gpr_replica.callbacks))) {
        OBJ_RELEASE(cb);
    }
    OBJ_DESTRUCT(&orte_gpr_replica.callbacks);


    /* clear the local subscriptions and triggers */
    if (NULL != orte_gpr_replica_globals.local_subscriptions) {
        lsubs = (orte_gpr_replica_local_subscriber_t**)(orte_gpr_replica_globals.local_subscriptions)->addr;
        for (i=0, k=0; k < orte_gpr_replica_globals.num_local_subs &&
                    i < (orte_gpr_replica_globals.local_subscriptions)->size; i++) {
            if (NULL != lsubs[i]) {
                k++;
                OBJ_RELEASE(lsubs[i]);
            }
        }
        OBJ_RELEASE(orte_gpr_replica_globals.local_subscriptions);
    }

    if (NULL != orte_gpr_replica_globals.local_triggers) {
        ltrigs = (orte_gpr_replica_local_trigger_t**)(orte_gpr_replica_globals.local_triggers)->addr;
        for (i=0, j=0; j < orte_gpr_replica_globals.num_local_trigs &&
                    i < (orte_gpr_replica_globals.local_triggers)->size; i++) {
            if (NULL != ltrigs[i]) {
                j++;
                OBJ_RELEASE(ltrigs[i]);
            }
        }
        OBJ_RELEASE(orte_gpr_replica_globals.local_triggers);
    }

    /* clean up the globals */

    if (NULL != orte_gpr_replica_globals.srch_cptr) {
        OBJ_RELEASE(orte_gpr_replica_globals.srch_cptr);
    }

    if (NULL != orte_gpr_replica_globals.overwritten) {
        OBJ_RELEASE(orte_gpr_replica_globals.overwritten);
    }

    if (NULL != orte_gpr_replica_globals.sub_ptrs) {
        OBJ_RELEASE(orte_gpr_replica_globals.sub_ptrs);
    }

    if (NULL != orte_gpr_replica_globals.srch_ival) {
        OBJ_RELEASE(orte_gpr_replica_globals.srch_ival);
    }

    if (NULL != orte_gpr_replica_globals.acted_upon) {
        OBJ_RELEASE(orte_gpr_replica_globals.acted_upon);
    }

    OBJ_DESTRUCT(&(orte_gpr_replica_globals.srch_itag));

    /* All done */
    if (orte_gpr_replica_globals.isolate) {
        return ORTE_SUCCESS;
    }

    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_GPR);

    return ORTE_SUCCESS;
}
