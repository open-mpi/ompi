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
int orte_gpr_base_unpack_cmd(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_CMD_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * NOTIFY ID
 */
int orte_gpr_base_unpack_notify_id(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_NOTIFY_ID_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * NOTIFY ACTION
 */
int orte_gpr_base_unpack_notify_action(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_NOTIFY_ACTION_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * ADDR MODE
 */
int orte_gpr_base_unpack_addr_mode(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_ADDR_MODE_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * KEYVAL
 */
int orte_gpr_base_unpack_keyval(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_keyval_t **keyval;
    size_t i, max_n=1;

    /* unpack into an array of keyval objects */
    keyval = (orte_gpr_keyval_t**) dest;
    for (i=0; i < *num_vals; i++) {
        /* allocate the memory storage */
        keyval[i] = OBJ_NEW(orte_gpr_keyval_t);
        if (NULL == keyval[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack the key */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(keyval[i]->key),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the data type so we can unpack the value */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(keyval[i]->type),
                    &max_n, ORTE_DATA_TYPE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the value */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(keyval[i]->value),
                    &max_n, keyval[i]->type))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * VALUE
 */
int orte_gpr_base_unpack_value(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_value_t **values;
    size_t i, max_n=1;

    /* unpack into array of value objects */
    values = (orte_gpr_value_t**) dest;
    for (i=0; i < *num_vals; i++) {
        /* create the value object */
        values[i] = OBJ_NEW(orte_gpr_value_t);
        if (NULL == values[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack the address mode */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(values[i]->addr_mode),
                    &max_n, ORTE_GPR_ADDR_MODE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the segment name */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(values[i]->segment),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of tokens */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(values[i]->num_tokens),
                    &max_n, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are tokens, allocate the required space for the char * pointers */
        if (0 < values[i]->num_tokens) {
            values[i]->tokens = (char **)malloc(values[i]->num_tokens * sizeof(char*));
            if (NULL == values[i]->tokens) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

            /* and unpack them */
            if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, values[i]->tokens,
                        &(values[i]->num_tokens), ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        /* get the number of keyval pairs */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(values[i]->cnt),
                    &max_n, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are keyvals, allocate the required space for the keyval object pointers */
        if(0 < values[i]->cnt) {
            values[i]->keyvals = (orte_gpr_keyval_t**)malloc(values[i]->cnt * sizeof(orte_gpr_keyval_t*));
            if (NULL == values[i]->keyvals) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

            /* unpack the keyval pairs */
            if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, values[i]->keyvals,
                    &(values[i]->cnt), ORTE_KEYVAL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } 
    }
    
    return ORTE_SUCCESS;
}

/*
 * SUBSCRIPTION
 */
int orte_gpr_base_unpack_subscription(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_subscription_t **subs;
    size_t i, max_n=1;

    /* unpack into array of subscription objects */
    subs = (orte_gpr_subscription_t**) dest;
    for (i=0; i < *num_vals; i++) {
        /* create the subscription object */
        subs[i] = OBJ_NEW(orte_gpr_subscription_t);
        if (NULL == subs[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack the address mode */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(subs[i]->addr_mode),
                    &max_n, ORTE_GPR_ADDR_MODE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the segment name */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(subs[i]->segment),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of tokens */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(subs[i]->num_tokens),
                    &max_n, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are tokens, allocate the required space for the char * pointers */
        if (0 < subs[i]->num_tokens) {
            subs[i]->tokens = (char **)malloc(subs[i]->num_tokens * sizeof(char*));
            if (NULL == subs[i]->tokens) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

            /* and unpack them */
            if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, subs[i]->tokens,
                        &(subs[i]->num_tokens), ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        /* get the number of keys */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(subs[i]->num_keys),
                    &max_n, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are keys, allocate the required space for the char * pointers */
        if (0 < subs[i]->num_keys) {
            subs[i]->keys = (char **)malloc(subs[i]->num_keys * sizeof(char*));
            if (NULL == subs[i]->keys) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

            /* and unpack them */
            if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, subs[i]->keys,
                        &(subs[i]->num_keys), ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        /* the pointer fields for cb_func and user_tag were NOT packed
         * so ignore them here as well
         */
    }

    return ORTE_SUCCESS;
}

/*
 * NOTIFY DATA
 */
int orte_gpr_base_unpack_notify_data(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_notify_data_t **data;
    size_t i, max_n=1;

    /* unpack into array of notify_data objects */
    data = (orte_gpr_notify_data_t**) dest;

    for (i=0; i < *num_vals; i++) {
        /* create the data object */
        data[i] = OBJ_NEW(orte_gpr_notify_data_t);
        if (NULL == data[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack the callback number */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(data[i]->cb_num),
                    &max_n, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the address mode */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(data[i]->addr_mode),
                    &max_n, ORTE_GPR_ADDR_MODE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the segment name */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(data[i]->segment),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of values */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(data[i]->cnt),
                    &max_n, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are values, allocate the required space for the value pointers */
        if (0 < data[i]->cnt) {
            data[i]->values = (orte_gpr_value_t**)malloc(data[i]->cnt * sizeof(orte_gpr_value_t*));
            if (NULL == data[i]->values) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

            /* and unpack them */
            if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, data[i]->values,
                        &(data[i]->cnt), ORTE_GPR_VALUE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    return ORTE_SUCCESS;
}
