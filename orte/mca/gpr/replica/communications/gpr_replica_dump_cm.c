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

#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/replica/communications/gpr_replica_comm.h"


int orte_gpr_replica_recv_dump_all_cmd(orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DUMP_ALL_CMD;
    int rc;
    
    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    rc = orte_gpr_replica_dump_all_fn(answer);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_gpr_replica_recv_dump_segments_cmd(orte_buffer_t *input_buffer, orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DUMP_SEGMENTS_CMD;
    char *segment;
    orte_std_cntr_t n;
    int rc;
    
    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &segment, &n, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_gpr_replica_dump_segments_fn(answer, segment);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_gpr_replica_recv_dump_triggers_cmd(orte_buffer_t *input_buffer,
                                            orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DUMP_TRIGGERS_CMD;
    orte_gpr_trigger_id_t start;
    orte_std_cntr_t n;
    int rc;
    
    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &start, &n, ORTE_GPR_TRIGGER_ID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = orte_gpr_replica_dump_triggers_fn(answer, start);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_gpr_replica_recv_dump_subscriptions_cmd(orte_buffer_t *input_buffer,
                                                 orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DUMP_SUBSCRIPTIONS_CMD;
    orte_gpr_subscription_id_t start;
    orte_std_cntr_t n;
    int rc;
    
    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &start, &n, ORTE_GPR_SUBSCRIPTION_ID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = orte_gpr_replica_dump_subscriptions_fn(answer, start);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_gpr_replica_recv_dump_a_trigger_cmd(orte_buffer_t *input_buffer,
                                            orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DUMP_A_TRIGGER_CMD;
    char *name;
    orte_gpr_trigger_id_t id;
    orte_gpr_replica_trigger_t **trigs;
    orte_std_cntr_t n, i, j;
    int rc;
    
    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &name, &n, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &id, &n, ORTE_GPR_TRIGGER_ID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (NULL == name) {  /* dump the trigger corresponding to the provided id */
        trigs = (orte_gpr_replica_trigger_t**)(orte_gpr_replica.triggers)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_trigs &&
                       i < (orte_gpr_replica.triggers)->size; i++) {
            if (NULL != trigs[i]) {
                j++;
                if (id == trigs[i]->index) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_trigger(answer, trigs[i]))) {
                        ORTE_ERROR_LOG(rc);
                    }
                    return rc;
                }
            }
        }
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
        
    } else { /* dump the named trigger */
        trigs = (orte_gpr_replica_trigger_t**)(orte_gpr_replica.triggers)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_trigs &&
                       i < (orte_gpr_replica.triggers)->size; i++) {
            if (NULL != trigs[i]) {
                j++;
                if (0 == strcmp(name, trigs[i]->name)) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_trigger(answer, trigs[i]))) {
                        ORTE_ERROR_LOG(rc);
                    }
                    return rc;
                }
            }
        }
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    }

    return rc;
}

int orte_gpr_replica_recv_dump_a_subscription_cmd(orte_buffer_t *input_buffer,
                                            orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DUMP_A_SUBSCRIPTION_CMD;
    orte_gpr_replica_subscription_t **subs;
    orte_gpr_subscription_id_t id;
    orte_std_cntr_t n, i, j;
    char *name;
    int rc;
    
    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &name, &n, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &id, &n, ORTE_GPR_SUBSCRIPTION_ID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (NULL == name) {  /* dump the subscription corresponding to the provided id */
        subs = (orte_gpr_replica_subscription_t**)(orte_gpr_replica.subscriptions)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_subs &&
                       i < (orte_gpr_replica.subscriptions)->size; i++) {
            if (NULL != subs[i]) {
                j++;
                if (id == subs[i]->index) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_subscription(answer, subs[i]))) {
                        ORTE_ERROR_LOG(rc);
                    }
                    return rc;
                }
            }
        }
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
        
    } else { /* dump the named subscription */
        subs = (orte_gpr_replica_subscription_t**)(orte_gpr_replica.subscriptions)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_subs &&
                       i < (orte_gpr_replica.subscriptions)->size; i++) {
            if (NULL != subs[i]) {
                j++;
                if (0 == strcmp(name, subs[i]->name)) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_subscription(answer, subs[i]))) {
                        ORTE_ERROR_LOG(rc);
                    }
                    free(name);
                    return rc;
                }
            }
        }
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    }

    return rc;
}

int orte_gpr_replica_recv_dump_callbacks_cmd(orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DUMP_CALLBACKS_CMD;
    int rc;
    
    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    rc = orte_gpr_replica_dump_callbacks_fn(answer);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_gpr_replica_recv_dump_segment_size_cmd(orte_buffer_t *input_buffer, orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DUMP_SEGMENT_SIZE_CMD;
    char *segment;
    orte_std_cntr_t n;
    int rc;

    OPAL_TRACE(3);

    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &segment, &n, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_gpr_replica_dump_segment_size_fn(answer, segment);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}
