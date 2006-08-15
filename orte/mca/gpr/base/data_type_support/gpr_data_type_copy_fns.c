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
 * COPY FUNCTIONS FOR EVERYTHING NON-STRUCTURED
 */
int orte_gpr_base_copy_cmd(orte_gpr_cmd_flag_t **dest, orte_gpr_cmd_flag_t *src, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)dest, (void*)src, ORTE_GPR_CMD_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

int orte_gpr_base_copy_subscription_id(orte_gpr_subscription_id_t **dest, orte_gpr_subscription_id_t *src, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)dest, (void*)src, ORTE_GPR_SUBSCRIPTION_ID_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

int orte_gpr_base_copy_trigger_id(orte_gpr_trigger_id_t **dest, orte_gpr_trigger_id_t *src, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)dest, (void*)src, ORTE_GPR_TRIGGER_ID_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

int orte_gpr_base_copy_notify_action(orte_gpr_notify_action_t **dest, orte_gpr_notify_action_t *src, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)dest, (void*)src, ORTE_GPR_NOTIFY_ACTION_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

int orte_gpr_base_copy_trigger_action(orte_gpr_trigger_action_t **dest, orte_gpr_trigger_action_t *src, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)dest, (void*)src, ORTE_GPR_TRIGGER_ACTION_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

int orte_gpr_base_copy_notify_msg_type(orte_gpr_notify_msg_type_t **dest, orte_gpr_notify_msg_type_t *src, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)dest, (void*)src, ORTE_GPR_NOTIFY_MSG_TYPE_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

int orte_gpr_base_copy_addr_mode(orte_gpr_addr_mode_t **dest, orte_gpr_addr_mode_t *src, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)dest, (void*)src, ORTE_GPR_ADDR_MODE_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}


/* COPY FUNCTIONS FOR COMPLEX TYPES */
/* KEYVAL */
int orte_gpr_base_copy_keyval(orte_gpr_keyval_t **dest, orte_gpr_keyval_t *src, orte_data_type_t type)
{
    orte_gpr_keyval_t *kval;
    int rc;

    /* create the new object */
    kval = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == kval) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    kval->value = OBJ_NEW(orte_data_value_t);
    if (NULL == kval->value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (NULL != src->key) {
        kval->key = strdup(src->key);
    }

    /* if there is data in the src, copy it */
    if (NULL != src->value) {
        kval->value->type = src->value->type;

        /* copy the data itself - use the appropriate copy function here */
        if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&(kval->value->data), src->value->data, src->value->type))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(kval);
            *dest = NULL;
            return rc;
        }
    }

    *dest = kval;
    return ORTE_SUCCESS;
}

/* VALUE */
int orte_gpr_base_copy_gpr_value(orte_gpr_value_t **dest, orte_gpr_value_t *src, orte_data_type_t type)
{
    int rc;
    orte_gpr_keyval_t **kvals;
    char **tokens;
    orte_std_cntr_t i;

    /* create the new object */
    *dest = OBJ_NEW(orte_gpr_value_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* copy the data into it */
    (*dest)->addr_mode = src->addr_mode;
    if (NULL != src->segment) {
        (*dest)->segment = strdup(src->segment);
    }
    (*dest)->cnt = src->cnt;
    ;
    /* copy the keyvals - use the appropriate copy function here */
    if (0 < src->cnt) {
        kvals = (orte_gpr_keyval_t**)malloc(src->cnt * sizeof(orte_gpr_keyval_t*));  /* allocate space for the pointers */
        if (NULL == kvals) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(*dest);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        (*dest)->keyvals = kvals;
        for (i=0; i < src->cnt; i++) {
            if (ORTE_SUCCESS != (rc = orte_gpr_base_copy_keyval(&kvals[i],
                                                    src->keyvals[i], ORTE_GPR_KEYVAL))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(*dest);
                return rc;
            }
        }
    }

    (*dest)->num_tokens = src->num_tokens;
    /* copy the tokens - use the appropriate copy function here */
    if (0 < src->num_tokens) {
        tokens = (char**)malloc(src->num_tokens * sizeof(char*));  /* allocate space for the pointers */
        if (NULL == tokens) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(*dest);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        (*dest)->tokens = tokens;
        for (i=0; i < src->num_tokens; i++) {
            tokens[i] = strdup(src->tokens[i]);
        }
    }

    return ORTE_SUCCESS;
}


/* SUBSCRIPTION */
int orte_gpr_base_copy_subscription(orte_gpr_subscription_t **dest, orte_gpr_subscription_t *src, orte_data_type_t type)
{
    orte_std_cntr_t i;
    orte_gpr_value_t **values;
    int rc;

    /* create the new object */
    *dest = OBJ_NEW(orte_gpr_subscription_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (NULL != src->name) {
        (*dest)->name = strdup(src->name);
    }
    (*dest)->id = src->id;
    (*dest)->action = src->action;
    (*dest)->cnt = src->cnt;

    /* copy the values - use the appropriate copy function here */
    if (0 < src->cnt) {
        values = (orte_gpr_value_t**)malloc(src->cnt * sizeof(orte_gpr_value_t*));
        if (NULL == values) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(*dest);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        (*dest)->values = values;
        for (i=0; i < src->cnt; i++) {
            if (ORTE_SUCCESS != (rc = orte_gpr_base_copy_gpr_value(&values[i], src->values[i], ORTE_GPR_VALUE))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(*dest);
                return rc;
            }
        }
    }

    (*dest)->cbfunc = src->cbfunc;
    (*dest)->user_tag = src->user_tag;

    return ORTE_SUCCESS;
}

/* TRIGGER */
int orte_gpr_base_copy_trigger(orte_gpr_trigger_t **dest, orte_gpr_trigger_t *src, orte_data_type_t type)
{
    orte_std_cntr_t i;
    orte_gpr_value_t **values;
    int rc;

    /* create the new object */
    *dest = OBJ_NEW(orte_gpr_trigger_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (NULL != src->name) {
        (*dest)->name = strdup(src->name);
    }
    (*dest)->id = src->id;
    (*dest)->action = src->action;
    (*dest)->cnt = src->cnt;

    /* copy the values - use the appropriate copy function here */
    if (0 < src->cnt) {
        values = (orte_gpr_value_t**)malloc(src->cnt * sizeof(orte_gpr_value_t*));
        if (NULL == values) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(*dest);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        (*dest)->values = values;
        for (i=0; i < src->cnt; i++) {
            if (ORTE_SUCCESS != (rc = orte_gpr_base_copy_gpr_value(&values[i], src->values[i], ORTE_GPR_VALUE))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(*dest);
                return rc;
            }
        }
    }

    (*dest)->cbfunc = src->cbfunc;
    (*dest)->user_tag = src->user_tag;

    return ORTE_SUCCESS;
}

/* NOTIFY DATA */
int orte_gpr_base_copy_notify_data(orte_gpr_notify_data_t **dest, orte_gpr_notify_data_t *src, orte_data_type_t type)
{
    orte_std_cntr_t j, k, index;
    orte_gpr_value_t **val, *ptr;
    int rc;

    /* create the new object */
    *dest = OBJ_NEW(orte_gpr_notify_data_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

        /* copy the data to it */
    if (NULL != src->target) {
        (*dest)->target = strdup(src->target);
    }
    (*dest)->id = src->id;
    (*dest)->remove = src->remove;
    (*dest)->cnt = src->cnt;

    /* copy the values */
    val = (orte_gpr_value_t**)(src->values)->addr;
    for (j=0, k=0; j < src->cnt &&
              k < (src->values)->size; k++) {
        if (NULL != val[k]) {
            j++;
            /* copy the value object */
            if (ORTE_SUCCESS != (rc = orte_gpr_base_copy_gpr_value(&ptr, val[k], ORTE_GPR_VALUE))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(*dest);
                return rc;
            }
            /* ...and add it to the pointer array */
            if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&index, (*dest)->values, ptr))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(*dest);
                return rc;
            }
        }
    }

    return ORTE_SUCCESS;
}

/* NOTIFY MSG */
int orte_gpr_base_copy_notify_msg(orte_gpr_notify_message_t **dest, orte_gpr_notify_message_t *src, orte_data_type_t type)
{
    orte_std_cntr_t j, k, index;
    orte_gpr_notify_data_t **val, *ptr;
    int rc;

    /* create the new object */
    *dest = OBJ_NEW(orte_gpr_notify_message_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    (*dest)->msg_type = src->msg_type;
    if (NULL != src->target) {
        (*dest)->target = strdup(src->target);
    }
    (*dest)->id = src->id;
    (*dest)->remove = src->remove;
    (*dest)->cnt = src->cnt;

    /* copy the notify data entries */
    val = (orte_gpr_notify_data_t**)(src->data)->addr;
    for (j=0, k=0; j < src->cnt &&
              k < (src->data)->size; k++) {
        if (NULL != val[k]) {
            j++;
            /* copy the data object */
            if (ORTE_SUCCESS != (rc = orte_gpr_base_copy_notify_data(&ptr, val[k], ORTE_GPR_NOTIFY_DATA))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(*dest);
                return rc;
            }
            /* ...and add it to the pointer array */
            if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&index, (*dest)->data, ptr))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(*dest);
                return rc;
            }
        }
    }

    return ORTE_SUCCESS;
}
