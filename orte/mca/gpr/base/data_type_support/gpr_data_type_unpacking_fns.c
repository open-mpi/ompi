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

#include "orte_config.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/util/trace.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/gpr/base/base.h"

/*
 * GPR CMD
 */
int orte_gpr_base_unpack_cmd(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;

    OPAL_TRACE(4);

    if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_CMD_T))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

/*
 * SUBSCRIPTION ID
 */
int orte_gpr_base_unpack_subscription_id(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;

    OPAL_TRACE(4);

    if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_SUBSCRIPTION_ID_T))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

/*
 * TRIGGER ID
 */
int orte_gpr_base_unpack_trigger_id(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;

    OPAL_TRACE(4);

    if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_TRIGGER_ID_T))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

/*
 * NOTIFY ACTION
 */
int orte_gpr_base_unpack_notify_action(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;

    OPAL_TRACE(4);

    if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_NOTIFY_ACTION_T))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

/*
 * TRIGGER ACTION
 */
int orte_gpr_base_unpack_trigger_action(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;

    OPAL_TRACE(4);

    if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_TRIGGER_ACTION_T))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

/*
 * NOTIFY MSG TYPE
 */
int orte_gpr_base_unpack_notify_msg_type(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;

    OPAL_TRACE(4);

    if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_NOTIFY_MSG_TYPE_T))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

/*
 * ADDR MODE
 */
int orte_gpr_base_unpack_addr_mode(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;

    OPAL_TRACE(4);

    if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_ADDR_MODE_T))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

/*
 * KEYVAL
 */
int orte_gpr_base_unpack_keyval(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_keyval_t **keyval;
    orte_std_cntr_t i, max_n;

    OPAL_TRACE(4);

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
        max_n=1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(keyval[i]->key),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the data value */
        max_n=1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(keyval[i]->value),
                                                         &max_n, ORTE_DATA_VALUE))) {
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
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_value_t **values;
    orte_std_cntr_t i, max_n=1;

    OPAL_TRACE(4);

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
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(values[i]->addr_mode),
                    &max_n, ORTE_GPR_ADDR_MODE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the segment name */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(values[i]->segment),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of tokens */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(values[i]->num_tokens),
                    &max_n, ORTE_STD_CNTR))) {
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
            if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, values[i]->tokens,
                        &(values[i]->num_tokens), ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* get the number of keyval pairs */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(values[i]->cnt),
                    &max_n, ORTE_STD_CNTR))) {
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
            if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, values[i]->keyvals,
                    &(values[i]->cnt), ORTE_GPR_KEYVAL))) {
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
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_subscription_t **subs;
    orte_std_cntr_t i, max_n=1;

    OPAL_TRACE(4);

    /* unpack into array of subscription objects */
    subs = (orte_gpr_subscription_t**) dest;
    for (i=0; i < *num_vals; i++) {
        /* create the subscription object */
        subs[i] = OBJ_NEW(orte_gpr_subscription_t);
        if (NULL == subs[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack the subscription name */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(subs[i]->name),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the subscription id */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(subs[i]->id),
                    &max_n, ORTE_GPR_SUBSCRIPTION_ID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the subscription action */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(subs[i]->action),
                    &max_n, ORTE_GPR_NOTIFY_ACTION))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of values */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(subs[i]->cnt),
                    &max_n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are values, allocate the required space for the value pointers */
        if (0 < subs[i]->cnt) {
            subs[i]->values = (orte_gpr_value_t**)malloc(subs[i]->cnt * sizeof(orte_gpr_value_t*));
            if (NULL == subs[i]->values) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

            /* and unpack them */
            if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, subs[i]->values,
                        &(subs[i]->cnt), ORTE_GPR_VALUE))) {
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
 * TRIGGER
 */
int orte_gpr_base_unpack_trigger(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_trigger_t **trigs;
    orte_std_cntr_t i, max_n=1;

    OPAL_TRACE(4);

    /* unpack into array of trigger objects */
    trigs = (orte_gpr_trigger_t**) dest;
    for (i=0; i < *num_vals; i++) {
        /* create the trigger object */
        trigs[i] = OBJ_NEW(orte_gpr_trigger_t);
        if (NULL == trigs[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack the trigger name */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(trigs[i]->name),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the trigger id */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(trigs[i]->id),
                    &max_n, ORTE_GPR_TRIGGER_ID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the trigger action */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(trigs[i]->action),
                    &max_n, ORTE_GPR_TRIGGER_ACTION))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of values */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(trigs[i]->cnt),
                    &max_n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are values, allocate the required space for the value pointers */
        if (0 < trigs[i]->cnt) {
            trigs[i]->values = (orte_gpr_value_t**)malloc(trigs[i]->cnt * sizeof(orte_gpr_value_t*));
            if (NULL == trigs[i]->values) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

            /* and unpack them */
            if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, trigs[i]->values,
                        &(trigs[i]->cnt), ORTE_GPR_VALUE))) {
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
int orte_gpr_base_unpack_notify_data(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_notify_data_t **data;
    orte_gpr_value_t **values;
    orte_std_cntr_t i, j, max_n=1;

    OPAL_TRACE(4);

    /* unpack into array of notify_data objects */
    data = (orte_gpr_notify_data_t**) dest;

    for (i=0; i < *num_vals; i++) {
        /* create the data object */
        data[i] = OBJ_NEW(orte_gpr_notify_data_t);
        if (NULL == data[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack the subscription name */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(data[i]->target),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the subscription number */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(data[i]->id),
                    &max_n, ORTE_GPR_SUBSCRIPTION_ID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the remove flag */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(data[i]->remove),
                    &max_n, ORTE_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of values */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(data[i]->cnt),
                    &max_n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are values, unpack them to the value array */
        if (0 < data[i]->cnt) {
            if (ORTE_SUCCESS != (rc = orte_pointer_array_set_size(data[i]->values, data[i]->cnt))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            values = (orte_gpr_value_t**)(data[i]->values)->addr;
            for (j=0; j < data[i]->cnt; j++) {
                max_n = 1;
                if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(values[j]),
                           &max_n, ORTE_GPR_VALUE))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
    }

    return ORTE_SUCCESS;
}


/*
 * NOTIFY MSG
 */
int orte_gpr_base_unpack_notify_msg(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_gpr_notify_message_t **msg;
    orte_gpr_notify_data_t **data;
    orte_std_cntr_t i, j, max_n=1;

    OPAL_TRACE(4);

    /* unpack into array of notify_data objects */
    msg = (orte_gpr_notify_message_t**) dest;

    for (i=0; i < *num_vals; i++) {
        /* create the data object */
        msg[i] = OBJ_NEW(orte_gpr_notify_message_t);
        if (NULL == msg[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack the message type */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(msg[i]->msg_type),
                    &max_n, ORTE_GPR_NOTIFY_MSG_TYPE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the trigger name */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(msg[i]->target),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the trigger number */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(msg[i]->id),
                    &max_n, ORTE_GPR_TRIGGER_ID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the remove flag */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(msg[i]->remove),
                    &max_n, ORTE_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of datagrams */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(msg[i]->cnt),
                    &max_n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are datagrams, unpack them to the data array */
        if (0 < msg[i]->cnt) {
            if (ORTE_SUCCESS != (rc = orte_pointer_array_set_size(msg[i]->data, msg[i]->cnt))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            data = (orte_gpr_notify_data_t**)(msg[i]->data)->addr;
            for (j=0; j < msg[i]->cnt; j++) {
                max_n = 1;
                if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(data[j]),
                           &max_n, ORTE_GPR_NOTIFY_DATA))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
    }

    return ORTE_SUCCESS;
}
