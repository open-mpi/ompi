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

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/base/base.h"

/*
 * STANDARD SIZE FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int orte_gpr_base_std_size(size_t *size, void *src, orte_data_type_t type)
{
    switch(type) {
        case ORTE_GPR_CMD:
            *size = sizeof(orte_gpr_cmd_flag_t);
            break;

        case ORTE_GPR_SUBSCRIPTION_ID:
            *size = sizeof(orte_gpr_subscription_id_t);
            break;

        case ORTE_GPR_TRIGGER_ID:
            *size = sizeof(orte_gpr_trigger_id_t);
            break;

        case ORTE_GPR_NOTIFY_ACTION:
            *size = sizeof(orte_gpr_notify_action_t);
            break;

        case ORTE_GPR_TRIGGER_ACTION:
            *size = sizeof(orte_gpr_trigger_action_t);
            break;

        case ORTE_GPR_NOTIFY_MSG_TYPE:
            *size = sizeof(orte_gpr_notify_msg_type_t);
            break;

        case ORTE_GPR_ADDR_MODE:
            *size = sizeof(orte_gpr_addr_mode_t);
            break;

        default:
            *size = 0;
            ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
            return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    return ORTE_SUCCESS;
}

/* SIZE FUNCTIONS FOR COMPLEX TYPES */

/* KEYVAL */
int orte_gpr_base_size_keyval(size_t *size, orte_gpr_keyval_t *src, orte_data_type_t type)
{
    size_t data_size;
    int rc;

    /* account for the object itself */
    *size = sizeof(orte_gpr_keyval_t);

    /* if the src is NULL, then that's the only thing we return */
    if (NULL == src) return ORTE_SUCCESS;

    /*...and its payload */
    if (NULL != src->key) {
        *size += strlen(src->key);
    }

    /* size the data itself - use the appropriate size function here */
    if (ORTE_SUCCESS != (rc = orte_dss.size(&data_size, src->value, ORTE_DATA_VALUE))) {
        ORTE_ERROR_LOG(rc);
        *size = 0;
        return rc;
    }
    *size += data_size;

    return ORTE_SUCCESS;
}

/* VALUE */
int orte_gpr_base_size_gpr_value(size_t *size, orte_gpr_value_t *src, orte_data_type_t type)
{
    orte_std_cntr_t i;
    size_t data_size;
    int rc;

    /* account for the object itself */
    *size = sizeof(orte_gpr_value_t);

    /* if the src is NULL, then that's the only thing we return */
    if (NULL == src) return ORTE_SUCCESS;

    /*...and its payload */
    if (NULL != src->segment) {
        *size += strlen(src->segment);
    }

    /* size the keyvals - use the appropriate size function here */
    if (0 < src->cnt) {
        for (i=0; i < src->cnt; i++) {
            *size += sizeof(orte_gpr_keyval_t*);  /* account for size of object pointer */
            if (ORTE_SUCCESS != (rc = orte_gpr_base_size_keyval(&data_size, src->keyvals[i],
                                                                ORTE_GPR_KEYVAL))) {
                ORTE_ERROR_LOG(rc);
                *size = 0;
                return rc;
            }
            *size += data_size;
        }
    }

    /* size the tokens - use the appropriate size function here */
    if (0 < src->num_tokens) {
        for (i=0; i < src->num_tokens; i++) {
            *size += sizeof(char*);  /* account for size of string pointer */
            if (NULL != src->tokens[i]) {
                *size += strlen(src->tokens[i]);
            }
        }
    }

    return ORTE_SUCCESS;
}


/* SUBSCRIPTION */
int orte_gpr_base_size_subscription(size_t *size, orte_gpr_subscription_t *src, orte_data_type_t type)
{
    orte_std_cntr_t i;
    size_t data_size;
    int rc;

    /* account for the object itself */
    *size = sizeof(orte_gpr_subscription_t);

    /* if the src is NULL, then that's the only thing we return */
    if (NULL == src) return ORTE_SUCCESS;

    /*...and its payload */
    if (NULL != src->name) {
        *size += strlen(src->name);
    }

    /* size the values - use the appropriate size function here */
    if (0 < src->cnt) {
        for (i=0; i < src->cnt; i++) {
            *size += sizeof(orte_gpr_value_t*);  /* account for object pointer */
            if (ORTE_SUCCESS != (rc = orte_gpr_base_size_gpr_value(&data_size, src->values[i], ORTE_GPR_VALUE))) {
                ORTE_ERROR_LOG(rc);
                *size = 0;
                return rc;
            }
            *size += data_size;
        }
    }

    return ORTE_SUCCESS;
}

/* TRIGGER */
int orte_gpr_base_size_trigger(size_t *size, orte_gpr_trigger_t *src, orte_data_type_t type)
{
    orte_std_cntr_t i;
    size_t data_size;
    int rc;

    /* account for the object itself */
    *size = sizeof(orte_gpr_trigger_t);

    /* if the src is NULL, then that's the only thing we return */
    if (NULL == src) return ORTE_SUCCESS;

    /*...and its payload */
    if (NULL != src->name) {
        *size += strlen(src->name);
    }

    /* size the values - use the appropriate size function here */
    if (0 < src->cnt) {
        for (i=0; i < src->cnt; i++) {
            *size += sizeof(orte_gpr_value_t*);  /* account for object pointer */
            if (ORTE_SUCCESS != (rc = orte_gpr_base_size_gpr_value(&data_size, src->values[i], ORTE_GPR_VALUE))) {
                ORTE_ERROR_LOG(rc);
                *size = 0;
                return rc;
            }
            *size += data_size;
        }
    }

    return ORTE_SUCCESS;
}

/* NOTIFY DATA */
int orte_gpr_base_size_notify_data(size_t *size, orte_gpr_notify_data_t *src, orte_data_type_t type)
{
    orte_std_cntr_t j, k;
    size_t data_size;
    orte_gpr_value_t **val;
    int rc;

    /* account for the object itself */
    *size = sizeof(orte_gpr_notify_data_t);

    /* if the src is NULL, then that's the only thing we return */
    if (NULL == src) return ORTE_SUCCESS;

    /*...and its payload */
    if (NULL != src->target) {
        *size += strlen(src->target);
    }
    *size += sizeof(orte_pointer_array_t);  /* account for size of pointer array object */
    *size += (src->values)->size * sizeof(void*);  /* account for size of pointer array storage */

    /* size the value entries */
    if (0 < src->cnt) {
        val = (orte_gpr_value_t**)(src->values)->addr;
        for (j=0, k=0; j < src->cnt &&
                  k < (src->values)->size; k++) {
            if (NULL != val[k]) {
                j++;
                /* account for size of value */
                if (ORTE_SUCCESS != (rc = orte_gpr_base_size_gpr_value(&data_size, val[k], ORTE_GPR_VALUE))) {
                    ORTE_ERROR_LOG(rc);
                    *size = 0;
                    return rc;
                }
                *size += data_size;
            }
        }
    }

    return ORTE_SUCCESS;
}

/* NOTIFY MSG */
int orte_gpr_base_size_notify_msg(size_t *size, orte_gpr_notify_message_t *src, orte_data_type_t type)
{
    orte_std_cntr_t j, k;
    size_t data_size;
    orte_gpr_notify_data_t **val;
    int rc;

    /* account for the object itself */
    *size = sizeof(orte_gpr_notify_message_t);

    /* if the src is NULL, then that's the only thing we return */
    if (NULL == src) return ORTE_SUCCESS;

    /*...and its payload */
    if (NULL != src->target) {
        *size += strlen(src->target);
    }
    *size += sizeof(orte_pointer_array_t);  /* account for size of pointer array object */
    *size += (src->data)->size * sizeof(void*);  /* account for size of pointer array storage */

    /* size the notify data entries */
    if (0 < src->cnt) {
        val = (orte_gpr_notify_data_t**)(src->data)->addr;
        for (j=0, k=0; j < src->cnt &&
                  k < (src->data)->size; k++) {
            if (NULL != val[k]) {
                j++;
                /* account for size of notify data */
                if (ORTE_SUCCESS != (rc = orte_gpr_base_size_notify_data(&data_size, val[k], ORTE_GPR_NOTIFY_DATA))) {
                    ORTE_ERROR_LOG(rc);
                    *size = 0;
                    return rc;
                }
                *size += data_size;
            }
        }
    }

    return ORTE_SUCCESS;
}
