/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "dps/dps.h"
#include "mca/errmgr/errmgr.h"
#include "util/proc_info.h"
#include "opal/util/output.h"

#include "gpr_replica_comm.h"

int orte_gpr_replica_recv_subscribe_cmd(orte_process_name_t* sender,
                                        orte_buffer_t *input_buffer,
                                        orte_buffer_t *output_buffer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_SUBSCRIBE_CMD;
    orte_data_type_t type;
    int rc, ret;
    size_t n, num_subs, num_trigs;
    orte_gpr_trigger_t **trigs=NULL;
    orte_gpr_subscription_t **subscriptions=NULL;
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.peek(input_buffer, &type, &n))) {
        ORTE_ERROR_LOG(rc);
        goto RETURN_ERROR;
    }
    
    if (0 < n) {
        /* create the space for the subscriptions */
        subscriptions = (orte_gpr_subscription_t**)malloc(n * sizeof(orte_gpr_subscription_t*));
        if (NULL == subscriptions) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto RETURN_ERROR;
        }
        
        if (ORTE_SUCCESS != (rc = orte_dps.unpack(input_buffer, subscriptions, &n, ORTE_GPR_SUBSCRIPTION))) {
            ORTE_ERROR_LOG(rc);
            goto RETURN_ERROR;
        }
    }
    num_subs = n;
    
    if (ORTE_SUCCESS != (rc = orte_dps.peek(input_buffer, &type, &n))) {
        ORTE_ERROR_LOG(rc);
        goto RETURN_ERROR;
    }
    
    /* if the original command did not provide any triggers, then we put a size_t value in the buffer of "zero"
     * to avoid causing buffer problems. thus, we need to check to see if the type is size_t vs trigger vs
     * something else. if it is trigger, then we need the number to be greater than 0, which should always
     * be true (we check it just to be safe). if it is size_t, then the value should be zero - anything else
     * generates an error.
     */
    if (ORTE_SIZE == type) {
        /* this case means that there were no triggers, so we need to clear the value from the buffer
         * and continue on
         */
        n=1;
        if (ORTE_SUCCESS != orte_dps.unpack(input_buffer, &num_trigs, &n, ORTE_SIZE)) {
            ORTE_ERROR_LOG(rc);
            goto RETURN_ERROR;
        }
        /* if the returned number of triggers isn't zero, then we have a problem */
        if (0 != num_trigs) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            rc = ORTE_ERR_COMM_FAILURE;
            goto RETURN_ERROR;
        }
    } else if (ORTE_GPR_TRIGGER == type && 0 < n) {
        /* create the space for the triggers */
        trigs = (orte_gpr_trigger_t**)malloc(n * sizeof(orte_gpr_trigger_t*));
        if (NULL == trigs) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto RETURN_ERROR;
        }
    
        if (ORTE_SUCCESS != orte_dps.unpack(input_buffer, trigs, &n, ORTE_GPR_TRIGGER)) {
            ORTE_ERROR_LOG(rc);
            goto RETURN_ERROR;
        }
        num_trigs = n;
    } else {
        /* we must have an error condition - it either wasn't the right type, or we had the type okay
         * but don't have a good number of elements. report the error and move on
         */
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        rc = ORTE_ERR_COMM_FAILURE;
        goto RETURN_ERROR;
    }


    /* register subscriptions */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_subscribe_fn(sender,
                                        num_subs, subscriptions,
                                        num_trigs, trigs))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_events())) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

 RETURN_ERROR:
    if (ORTE_SUCCESS != (ret = orte_dps.pack(output_buffer, &rc, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    return rc;
}

int orte_gpr_replica_recv_unsubscribe_cmd(orte_process_name_t *sender,
                                          orte_buffer_t *input_buffer,
                                          orte_buffer_t *output_buffer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_UNSUBSCRIBE_CMD;
    orte_gpr_subscription_id_t sub_number=0;
    int rc, ret;
    size_t n;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(input_buffer, &sub_number, &n,
                                    ORTE_GPR_SUBSCRIPTION_ID))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    ret = orte_gpr_replica_remove_subscription(sender, sub_number);
    if (ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
    }

RETURN_ERROR:
    if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ret;
}


int orte_gpr_replica_recv_cancel_trigger_cmd(orte_process_name_t *sender,
                                          orte_buffer_t *input_buffer,
                                          orte_buffer_t *output_buffer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_CANCEL_TRIGGER_CMD;
    orte_gpr_trigger_id_t trig_number=0;
    int rc, ret;
    size_t n;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(input_buffer, &trig_number, &n,
                                    ORTE_GPR_TRIGGER_ID))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    ret = orte_gpr_replica_remove_trigger(sender, trig_number);
    if (ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
    }

RETURN_ERROR:
    if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ret;
}

