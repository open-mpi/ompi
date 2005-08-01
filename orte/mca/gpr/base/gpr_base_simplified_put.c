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
#include "orte/include/orte_constants.h"

#include "opal/util/output.h"

#include "orte/dps/dps.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/base/base.h"


int orte_gpr_base_put_1(orte_gpr_addr_mode_t addr_mode,
                               char *segment, char **tokens,
                               char *key, orte_data_type_t type,
                               orte_gpr_value_union_t data_value)
{
    orte_gpr_value_t *values;
    orte_gpr_value_t value = { {OBJ_CLASS(opal_object_t),0},
                                ORTE_GPR_TOKENS_AND,
                                NULL, 0, NULL, 0, NULL };
    orte_gpr_keyval_t *keyvals;
    orte_gpr_keyval_t keyval = { {OBJ_CLASS(opal_object_t),0},
                                  NULL,
                                  0 };
    int rc;
    
    value.addr_mode = addr_mode;
    value.segment = segment;
    value.cnt = 1;
    keyvals = &keyval;
    value.keyvals = &keyvals;
    keyval.key = key;
    keyval.type = type;
    keyval.value = data_value;
    
    value.tokens = tokens;
    values = &value;
    
    /* put the value on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* no memory to clean up since we didn't allocate any */
    return ORTE_SUCCESS;
}


int orte_gpr_base_put_N(orte_gpr_addr_mode_t addr_mode,
                               char *segment, char **tokens,
                               size_t n, char **keys,
                               orte_data_type_t *types,
                               orte_gpr_value_union_t *data_values)
{
    orte_gpr_value_t *values;
    orte_gpr_value_t value = { {OBJ_CLASS(opal_object_t),0},
                                ORTE_GPR_TOKENS_AND,
                                NULL, 0, NULL, 0, NULL };
    size_t i, j;
    int rc;
    
    value.addr_mode = addr_mode;
    value.segment = segment;
    value.cnt = n;
    value.keyvals = (orte_gpr_keyval_t**)malloc(n * sizeof(orte_gpr_keyval_t*));
    if (NULL == value.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    for (i=0; i < n; i++) {
        value.keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        if (NULL == value.keyvals[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            for (j=0; j < i; j++) OBJ_RELEASE(value.keyvals[j]);
            free(value.keyvals);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        value.keyvals[i]->key = keys[i];
        value.keyvals[i]->type = types[i];
        value.keyvals[i]->value = data_values[i];
    }
    
    value.tokens = tokens;
    values = &value;
    
    /* put the value on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* clean up memory - very carefully!
     * We can't use the object destructors because we didn't
     * copy input data fields into the objects. Thus, only
     * release the data that we explicitly allocated
     */
    for (i=0; i < n; i++) free(value.keyvals[i]);
    free(value.keyvals);
    
    return rc;
}

