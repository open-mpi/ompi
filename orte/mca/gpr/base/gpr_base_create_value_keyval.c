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
/** @file
 *
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/base/base.h"


int orte_gpr_base_create_value(orte_gpr_value_t **value,
                               orte_gpr_addr_mode_t addr_mode,
                               char *segment,
                               orte_std_cntr_t cnt,  /**< Number of keyval objects */
                               orte_std_cntr_t num_tokens)
{
    orte_gpr_value_t *val;

    OPAL_TRACE(1);

    *value = OBJ_NEW(orte_gpr_value_t);
    if (NULL == *value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val = *value;

    /* get space for the specified number of keyvals */
    if (0 < cnt) {
        val->keyvals = (orte_gpr_keyval_t**)malloc(cnt * sizeof(orte_gpr_keyval_t*));
        if (NULL == val->keyvals) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(val);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* initialize it to NULL */
        memset(val->keyvals, 0, cnt * sizeof(orte_gpr_keyval_t*));
    }

    /* get space for the specified number of tokens */
    if (0 < num_tokens) {
        val->tokens = (char**)malloc((1+num_tokens) * sizeof(char*));
        if (NULL == val->tokens) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(val);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* initialize it to NULL and ensure that the array is NULL terminated */
        memset(val->tokens, 0, (1+num_tokens) * sizeof(char*));
    }
    
    val->addr_mode = addr_mode;
    if (NULL != segment) {
        val->segment = strdup(segment);
    }
    val->cnt = cnt;
    val->num_tokens = num_tokens;

    return ORTE_SUCCESS;
}


int orte_gpr_base_create_keyval(orte_gpr_keyval_t **keyval,
                                char *key,
                                orte_data_type_t type,
                                void *data)
{
    orte_gpr_keyval_t *kv;
    int rc;

    OPAL_TRACE(1);

    *keyval = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == *keyval) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    kv = *keyval;

    /* if the type is ORTE_UNDEF, then we don't actually want to create a data_value. This
     * is the case, for example, when we are doing subscriptions as the keyval is used simply
     * to transmit the key - the data_value field must remain NULL
     */
    if (ORTE_UNDEF != type) {
        kv->value = OBJ_NEW(orte_data_value_t);
        if (NULL == kv->value) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(kv);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        kv->value->type = type;
        if (NULL != data) {
            if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&(kv->value->data), data, type))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(kv);
                return rc;
            }
        }
    }

    if (NULL != key) {
        kv->key = strdup(key);
    }

    return ORTE_SUCCESS;
}

