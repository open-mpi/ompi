/*
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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "opal/util/output.h"

#include "orte/dss/dss_types.h"
#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/replica/api_layer/gpr_replica_api.h"

int orte_gpr_replica_dump_all(void)
{
    orte_buffer_t *buffer;
    int rc;

    if (orte_gpr_replica_globals.debug) {
	   opal_output(0, "[%lu,%lu,%lu] gpr_replica_dump_all: entered",
		    ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    buffer = OBJ_NEW(orte_buffer_t);
    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_all_fn(buffer))) {
        ORTE_ERROR_LOG(rc);
    }

    if (ORTE_SUCCESS == rc) {
        orte_gpr_base_print_dump(buffer);
    }
    OBJ_RELEASE(buffer);

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}

int orte_gpr_replica_dump_segments(char *segment)
{
    orte_buffer_t *buffer;
    int rc;

    if (orte_gpr_replica_globals.debug) {
      opal_output(0, "[%lu,%lu,%lu] gpr_replica_dump_segments: entered",
         ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    buffer = OBJ_NEW(orte_buffer_t);
    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_segments_fn(buffer, segment))) {
        ORTE_ERROR_LOG(rc);
    }

    if (ORTE_SUCCESS == rc) {
        orte_gpr_base_print_dump(buffer);
    }
    OBJ_RELEASE(buffer);

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}

int orte_gpr_replica_dump_triggers(orte_gpr_trigger_id_t start)
{
    orte_buffer_t *buffer;
    int rc;

    if (orte_gpr_replica_globals.debug) {
      opal_output(0, "[%lu,%lu,%lu] gpr_replica_dump_triggers: entered",
         ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    buffer = OBJ_NEW(orte_buffer_t);
    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_triggers_fn(buffer, start))) {
        ORTE_ERROR_LOG(rc);
    }

    if (ORTE_SUCCESS == rc) {
        orte_gpr_base_print_dump(buffer);
    }
    OBJ_RELEASE(buffer);

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}

int orte_gpr_replica_dump_subscriptions(orte_gpr_subscription_id_t start)
{
    orte_buffer_t *buffer;
    int rc;

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    buffer = OBJ_NEW(orte_buffer_t);
    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_subscriptions_fn(buffer, start))) {
        ORTE_ERROR_LOG(rc);
    }

    if (ORTE_SUCCESS == rc) {
        orte_gpr_base_print_dump(buffer);
    }
    OBJ_RELEASE(buffer);

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}

int orte_gpr_replica_dump_a_trigger(
                            char *name,
                            orte_gpr_trigger_id_t id)
{
    orte_buffer_t buffer;
    orte_gpr_replica_trigger_t **trigs;
    orte_std_cntr_t i, j;
    int rc;

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    OBJ_CONSTRUCT(&buffer, orte_buffer_t);

    if (NULL == name) {  /* dump the trigger corresponding to the provided id */
        trigs = (orte_gpr_replica_trigger_t**)(orte_gpr_replica.triggers)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_trigs &&
                       i < (orte_gpr_replica.triggers)->size; i++) {
            if (NULL != trigs[i]) {
                j++;
                if (id == trigs[i]->index) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_trigger(&buffer, trigs[i]))) {
                        ORTE_ERROR_LOG(rc);
                    }
                    goto PROCESS;
                }
            }
        }
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OBJ_DESTRUCT(&buffer);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return ORTE_ERR_NOT_FOUND;
    } else { /* dump the named trigger */
        trigs = (orte_gpr_replica_trigger_t**)(orte_gpr_replica.triggers)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_trigs &&
                       i < (orte_gpr_replica.triggers)->size; i++) {
            if (NULL != trigs[i]) {
                j++;
                if (0 == strcmp(name, trigs[i]->name)) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_trigger(&buffer, trigs[i]))) {
                        ORTE_ERROR_LOG(rc);
                    }
                    goto PROCESS;
                }
            }
        }
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OBJ_DESTRUCT(&buffer);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return ORTE_ERR_NOT_FOUND;
    }
    
PROCESS:
    if (ORTE_SUCCESS == rc) {
        orte_gpr_base_print_dump(&buffer);
    }
    OBJ_DESTRUCT(&buffer);

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}


int orte_gpr_replica_dump_a_subscription(char *name,
                            orte_gpr_subscription_id_t id)
{
    orte_buffer_t buffer;
    orte_gpr_replica_subscription_t **subs;
    orte_std_cntr_t i, j;
    int rc;

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    OBJ_CONSTRUCT(&buffer, orte_buffer_t);

    if (NULL == name) {  /* dump the subscription corresponding to the provided id */
        subs = (orte_gpr_replica_subscription_t**)(orte_gpr_replica.subscriptions)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_subs &&
                       i < (orte_gpr_replica.subscriptions)->size; i++) {
            if (NULL != subs[i]) {
                j++;
                if (id == subs[i]->index) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_subscription(&buffer, subs[i]))) {
                        ORTE_ERROR_LOG(rc);
                    }
                    goto PROCESS;
                }
            }
        }
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OBJ_DESTRUCT(&buffer);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return ORTE_ERR_NOT_FOUND;
    } else { /* dump the named subscription */
        subs = (orte_gpr_replica_subscription_t**)(orte_gpr_replica.subscriptions)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_subs &&
                       i < (orte_gpr_replica.subscriptions)->size; i++) {
            if (NULL != subs[i]) {
                j++;
                if (0 == strcmp(name, subs[i]->name)) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_subscription(&buffer, subs[i]))) {
                        ORTE_ERROR_LOG(rc);
                    }
                    goto PROCESS;
                }
            }
        }
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OBJ_DESTRUCT(&buffer);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return ORTE_ERR_NOT_FOUND;
    }
    
PROCESS:
    if (ORTE_SUCCESS == rc) {
        orte_gpr_base_print_dump(&buffer);
    }
    OBJ_DESTRUCT(&buffer);

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}


int orte_gpr_replica_dump_callbacks(void)
{
    orte_buffer_t *buffer;
    int rc;

    if (orte_gpr_replica_globals.debug) {
      opal_output(0, "[%lu,%lu,%lu] gpr_replica_dump_callbacks: entered",
         ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    buffer = OBJ_NEW(orte_buffer_t);
    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_callbacks_fn(buffer))) {
        ORTE_ERROR_LOG(rc);
    }

    if (ORTE_SUCCESS == rc) {
        orte_gpr_base_print_dump(buffer);
    }
    OBJ_RELEASE(buffer);

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}

int orte_gpr_replica_dump_notify_msg(orte_gpr_notify_message_t *msg)
{
    orte_buffer_t *answer;
    int rc;
    
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
     return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_dump_notify_msg(answer, msg))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
       return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_base_print_dump(answer))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(answer);
    return rc;
}


int orte_gpr_replica_dump_notify_data(orte_gpr_notify_data_t *data)
{
    orte_buffer_t *answer;
    int rc;
    
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
     return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_dump_notify_data(answer, data))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
       return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_base_print_dump(answer))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(answer);
    return rc;
}

int orte_gpr_replica_dump_value(orte_gpr_value_t *value)
{
    orte_buffer_t *answer;
    int rc;
    
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
     return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_dump_value(answer, value))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
       return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_base_print_dump(answer))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(answer);
    return rc;
}

int orte_gpr_replica_dump_segment_size(char *segment)
{
    orte_buffer_t *buffer;
    int rc;

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    buffer = OBJ_NEW(orte_buffer_t);
    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_segment_size_fn(buffer, segment))) {
        ORTE_ERROR_LOG(rc);
    }

    if (ORTE_SUCCESS == rc) {
        orte_gpr_base_print_dump(buffer);
    }
    OBJ_RELEASE(buffer);

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}
