/*
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
 
#include "orte_config.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "mca/errmgr/errmgr.h"
#include "dps/dps_internal.h"

#include "mca/gpr/base/base.h"

/*
 * GPR CMD
 */
int orte_gpr_base_pack_cmd(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, src, num_vals, ORTE_GPR_CMD_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * SUBSCRIPTION ID
 */
int orte_gpr_base_pack_subscription_id(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, src, num_vals, ORTE_GPR_SUBSCRIPTION_ID_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * TRIGGER ID
 */
int orte_gpr_base_pack_trigger_id(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, src, num_vals, ORTE_GPR_TRIGGER_ID_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * NOTIFY ACTION
 */
int orte_gpr_base_pack_notify_action(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, src, num_vals, ORTE_GPR_NOTIFY_ACTION_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * TRIGGER ACTION
 */
int orte_gpr_base_pack_trigger_action(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, src, num_vals, ORTE_GPR_TRIGGER_ACTION_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * ADDR MODE
 */
int orte_gpr_base_pack_addr_mode(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, src, num_vals, ORTE_GPR_ADDR_MODE_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * KEYVAL
 */
int orte_gpr_base_pack_keyval(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_keyval_t **keyval;
    size_t i;

    /* array of pointers to keyval objects - need to pack the
       objects */
    keyval = (orte_gpr_keyval_t**) src;

    for (i=0; i < num_vals; i++) {
        /* pack the key */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(keyval[i]->key)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the data type so we can read it for unpacking */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, &(keyval[i]->type), 1,
                         ORTE_DATA_TYPE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the value */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, &(keyval[i]->value), 1,
                         keyval[i]->type))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;
}

/*
 * VALUE
 */
int orte_gpr_base_pack_value(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_value_t **values;
    size_t i;

    /* array of pointers to value objects - need to pack the objects */
    values = (orte_gpr_value_t**) src;
    for (i=0; i<num_vals; i++) {
        /* pack the address mode */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(values[i]->addr_mode)), 1, ORTE_GPR_ADDR_MODE))) {
            ORTE_ERROR_LOG(rc);
            return ORTE_ERROR;
        }
        
        /* pack the segment name */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(values[i]->segment)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return ORTE_ERROR;
        }

        /* pack the number of tokens so we can read it for unpacking */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(values[i]->num_tokens)), 1, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return ORTE_ERROR;
        }

        /* if there are tokens, pack them */
        if (0 < values[i]->num_tokens) {
            if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                            (void*)((values[i]->tokens)), values[i]->num_tokens, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return ORTE_ERROR;
            }
        }
        
        /* pack the number of keyval pairs so we can read it for unpacking */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(values[i]->cnt)), 1, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return ORTE_ERROR;
        }

        /* if there are keyval pairs, pack them */
        if (0 < values[i]->cnt) {
            if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                            (void*)((values[i]->keyvals)), values[i]->cnt, ORTE_KEYVAL))) {
                ORTE_ERROR_LOG(rc);
                return ORTE_ERROR;
            }
        }
    }
    
    return ORTE_SUCCESS;
}

/*
 * SUBSCRIPTION
 */
int orte_gpr_base_pack_subscription(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_subscription_t **subs;
    size_t i;

    /* array of pointers to subscription objects - need to pack the objects */
    subs = (orte_gpr_subscription_t**) src;
    for (i=0; i<num_vals; i++) {
        /* pack the subscription name */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(subs[i]->name)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the subscription id */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(subs[i]->id)), 1, ORTE_GPR_SUBSCRIPTION_ID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the notify action */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(subs[i]->action)), 1, ORTE_GPR_NOTIFY_ACTION))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of values so we can read it for unpacking */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(subs[i]->cnt)), 1, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are values, pack them */
        if (0 < subs[i]->cnt) {
            if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                            (void*)((subs[i]->values)), subs[i]->cnt, ORTE_GPR_VALUE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* skip the pointers for cb_func and user_tag */
    }

    return ORTE_SUCCESS;
}

/*
 * TRIGGER
 */
int orte_gpr_base_pack_trigger(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_trigger_t **trigs;
    size_t i;

    /* array of pointers to trigger objects - need to pack the objects */
    trigs = (orte_gpr_trigger_t**) src;
    for (i=0; i<num_vals; i++) {
        /* pack the trigger name */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(trigs[i]->name)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the trigger id */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(trigs[i]->id)), 1, ORTE_GPR_TRIGGER_ID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the trigger action */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(trigs[i]->action)), 1, ORTE_GPR_TRIGGER_ACTION))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of values so we can read it for unpacking */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(trigs[i]->cnt)), 1, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are values, pack the values */
        if (0 < trigs[i]->cnt) {
            if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                            (void*)((trigs[i]->values)), trigs[i]->cnt, ORTE_GPR_VALUE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    return ORTE_SUCCESS;
}

/*
 * NOTIFY DATA
 */
int orte_gpr_base_pack_notify_data(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_notify_data_t **data;
    size_t i;

    /* array of pointers to notify data objects - need to pack the objects */
    data = (orte_gpr_notify_data_t**) src;

    for (i=0; i<num_vals; i++) {

        /* pack the subscription number */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(data[i]->id)), 1, ORTE_GPR_SUBSCRIPTION_ID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of values so we can read it for unpacking */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(data[i]->cnt)), 1, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are values, pack the values */
        if (0 < data[i]->cnt) {
            if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                            (void*)((data[i]->values)), data[i]->cnt, ORTE_GPR_VALUE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    return ORTE_SUCCESS;
}
