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


int orte_gpr_base_put_1(orte_gpr_addr_mode_t addr_mode,
                               char *segment, char **tokens,
                               char *key, orte_data_value_t *data_value)
{
    orte_gpr_value_t *values;
    orte_gpr_value_t value = ORTE_GPR_VALUE_EMPTY;
    orte_gpr_keyval_t *keyval;
    orte_std_cntr_t i;
    int rc;

    OPAL_TRACE(1);

    value.addr_mode = addr_mode;
    value.segment = segment;
    value.cnt = 1;
    value.keyvals = &keyval;
    if (ORTE_SUCCESS != (rc = orte_gpr_base_create_keyval(&keyval, key,
                                                          data_value->type, data_value->data))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    value.tokens = tokens;
    /* must count the number of tokens */
    value.num_tokens = 0;
    if (NULL != tokens) {
        for (i=0; NULL != tokens[i]; i++) {
            (value.num_tokens)++;
        }
    }
    values = &value;

    /* put the value on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(keyval);
        return rc;
    }

    /* cleanup */
    OBJ_RELEASE(keyval);
    
    return ORTE_SUCCESS;
}


int orte_gpr_base_put_N(orte_gpr_addr_mode_t addr_mode,
                               char *segment, char **tokens,
                               orte_std_cntr_t n, char **keys,
                               orte_data_value_t **data_values)
{
    orte_gpr_value_t *value;
    orte_std_cntr_t i, num_tokens;
    int rc;

    OPAL_TRACE(1);

    /* must count the number of tokens */
    num_tokens = 0;
    if (NULL != tokens) {
        for (i=0; NULL != tokens[i]; i++) {
            num_tokens++;
        }
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_create_value(&value, addr_mode, segment, n, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    for (i=0; i < n; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_create_keyval(&(value->keyvals[i]), keys[i], data_values[i]->type, data_values[i]->data))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            return rc;
        }
    }

    for (i=0; i < value->num_tokens; i++) {
        value->tokens[i] = strdup(tokens[i]);
    }

    /* put the value on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
    }

    /* clean up memory */
    OBJ_RELEASE(value);

    return rc;
}

