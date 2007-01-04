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

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/proc_info.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/mca/gpr/replica/communications/gpr_replica_comm.h"

int orte_gpr_replica_recv_subscribe_cmd(orte_process_name_t* sender,
                                        orte_buffer_t *input_buffer,
                                        orte_buffer_t *output_buffer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_SUBSCRIBE_CMD;
    int rc, ret;
    orte_std_cntr_t n, num_subs, num_trigs;
    orte_gpr_trigger_t **trigs=NULL;
    orte_gpr_subscription_t **subscriptions=NULL;
    
    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(output_buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* get the number of subscriptions */
    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &num_subs, &n, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        goto RETURN_ERROR;
    }
    
    /* create the space for the subscriptions, if any are there - and unpack them */
    if (0 < num_subs) {
        subscriptions = (orte_gpr_subscription_t**)malloc(num_subs * sizeof(orte_gpr_subscription_t*));
        if (NULL == subscriptions) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto RETURN_ERROR;
        }
        n = num_subs;
        if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, subscriptions, &n, ORTE_GPR_SUBSCRIPTION))) {
            ORTE_ERROR_LOG(rc);
            goto RETURN_ERROR;
        }
        num_subs = n;
    }

    /* get the number of triggers */
    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &num_trigs, &n, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        goto RETURN_ERROR;
    }
    
    /* create the space for the triggers, if any are there - and unpack them */
    if (0 < num_trigs) {
        trigs = (orte_gpr_trigger_t**)malloc(num_trigs * sizeof(orte_gpr_trigger_t*));
        if (NULL == trigs) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto RETURN_ERROR;
        }
        n = num_trigs;
        if (ORTE_SUCCESS != orte_dss.unpack(input_buffer, trigs, &n, ORTE_GPR_TRIGGER)) {
            ORTE_ERROR_LOG(rc);
            goto RETURN_ERROR;
        }
        num_trigs = n;
    }

    /* register subscriptions */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_subscribe_fn(sender,
                                        num_subs, subscriptions,
                                        num_trigs, trigs))) {
        ORTE_ERROR_LOG(rc);
        goto RETURN_ERROR;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_events())) {
        ORTE_ERROR_LOG(rc);
    }

 RETURN_ERROR:
    /* release the subscription objects, if any */
    if (NULL != subscriptions) {
        for (n=0; n < num_subs; n++) OBJ_RELEASE(subscriptions[n]);
        if (NULL != subscriptions) free(subscriptions);
    }
    /* release the trigger objects, if any */
    if (NULL != trigs) {
        for (n=0; n < num_trigs; n++) OBJ_RELEASE(trigs[n]);
        if (NULL != trigs) free(trigs);
    }
    
    if (ORTE_SUCCESS != (ret = orte_dss.pack(output_buffer, &rc, 1, ORTE_INT))) {
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
    orte_std_cntr_t n;

    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(output_buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &sub_number, &n,
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
    if (ORTE_SUCCESS != (rc = orte_dss.pack(output_buffer, &ret, 1, ORTE_INT))) {
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
    orte_std_cntr_t n;

    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(output_buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &trig_number, &n,
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
    if (ORTE_SUCCESS != (rc = orte_dss.pack(output_buffer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ret;
}

