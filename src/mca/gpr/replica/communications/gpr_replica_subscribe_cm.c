/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "gpr_replica_comm.h"

int orte_gpr_replica_recv_subscribe_cmd(orte_process_name_t* sender,
                                        orte_buffer_t *input_buffer,
                                        orte_buffer_t *output_buffer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_SUBSCRIBE_CMD;
    orte_data_type_t type;
    orte_gpr_notify_id_t local_idtag=0, idtag=0;
    int rc, ret, num_subs, num_trigs;
    size_t n;
    orte_gpr_notify_action_t action;
    orte_gpr_value_t **trigs;
    orte_gpr_subscription_t **subscriptions;
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(input_buffer, &action, &n, ORTE_NOTIFY_ACTION))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.peek(input_buffer, &type, &n))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* create the space for the subscriptions */
    subscriptions = (orte_gpr_subscription_t**)malloc(n * sizeof(orte_gpr_subscription_t*));
    if (NULL == subscriptions) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(input_buffer, subscriptions, &n, ORTE_GPR_SUBSCRIPTION))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    num_subs = (int)n;
    
    if (ORTE_SUCCESS != (rc = orte_dps.peek(input_buffer, &type, &n))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* create the space for the triggers */
    trigs = (orte_gpr_value_t**)malloc(n * sizeof(orte_gpr_value_t*));
    if (NULL == trigs) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != orte_dps.unpack(input_buffer, trigs, &n, ORTE_GPR_VALUE)) {
        ORTE_ERROR_LOG(rc);
        goto RETURN_ERROR;
    }
    num_trigs = (int)n;
    
    n = 1;
    if (ORTE_SUCCESS != orte_dps.unpack(input_buffer, &idtag, &n, ORTE_GPR_NOTIFY_ID)) {
        ORTE_ERROR_LOG(rc);
        goto RETURN_ERROR;
    }

    /*******   LOCK    *****/
    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    if (NULL != sender) {  /* remote sender */

        if (orte_gpr_replica_globals.debug) {
            ompi_output(0, "[%d,%d,%d] subscribe requested for remote sender [%d,%d,%d] for idtag %d",
                       ORTE_NAME_ARGS(orte_process_info.my_name), ORTE_NAME_ARGS(sender), idtag);
        }
      
        /* enter request on local notify tracking system */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_enter_notify_request(&local_idtag,
                                        sender, idtag, num_subs, subscriptions))) {
            ORTE_ERROR_LOG(rc);
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            goto RETURN_ERROR;
        }

        /* register subscriptions */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_subscribe_fn(action, 
                                        num_subs, subscriptions,
                                        num_trigs, trigs,
                                        local_idtag))) {
            ORTE_ERROR_LOG(rc);
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            goto RETURN_ERROR;
        }

        /* pack the local idtag for return to sender */
        if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &local_idtag, 1, ORTE_GPR_NOTIFY_ID))) {
            ORTE_ERROR_LOG(rc);
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            goto RETURN_ERROR;
        }

    } else {  /* local sender - idtag is for local notify tracking system */
        /* register subscription */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_subscribe_fn(action,
                                        num_subs, subscriptions,
                                        num_trigs, trigs,
                                        idtag))) {
            ORTE_ERROR_LOG(rc);
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            goto RETURN_ERROR;
        }
        
        /* pack the local idtag for return to local sender */
        if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &idtag, 1, ORTE_GPR_NOTIFY_ID))) {
            ORTE_ERROR_LOG(rc);
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            goto RETURN_ERROR;
        }

    }

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
    /******     UNLOCK     ******/

 RETURN_ERROR:
    if (ORTE_SUCCESS != (ret = orte_dps.pack(output_buffer, &rc, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    return rc;
}

int orte_gpr_replica_recv_unsubscribe_cmd(orte_buffer_t *input_buffer,
                                          orte_buffer_t *output_buffer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_UNSUBSCRIBE_CMD;
    orte_gpr_notify_id_t sub_number=0;
    int rc, ret;
    size_t n;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(input_buffer, &sub_number, &n, ORTE_GPR_NOTIFY_ID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /*******   LOCK    *****/
    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    ret = orte_gpr_replica_unsubscribe_fn(sub_number);
    if (ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
    /******     UNLOCK     ******/

    if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ret;
}

