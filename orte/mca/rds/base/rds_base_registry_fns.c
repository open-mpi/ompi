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
/** @file:
 */

/*
 * includes
 */
#include "orte_config.h"
#include "include/orte_constants.h"

#include <string.h>

#include "class/ompi_list.h"
#include "util/output.h"
#include "mca/errmgr/errmgr.h"
#include "mca/gpr/gpr.h"
#include "mca/schema/schema.h"

#include "mca/rds/base/base.h"

int orte_rds_base_store_resource(ompi_list_t *resources)
{
    orte_rds_cell_desc_t *cell;
    ompi_list_item_t *item;
    orte_gpr_value_t **values;
    orte_rds_cell_attr_t *attr;
    size_t i, j, num_vals;
    int rc;
    
    if (NULL == resources) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    num_vals = ompi_list_get_size(resources);
    if (0 == num_vals) {  /* nothing to do */
        return ORTE_SUCCESS;
    }

    values = (orte_gpr_value_t**)malloc(num_vals * sizeof(orte_gpr_value_t*));
    if (NULL == values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    for (i=0; i < num_vals && NULL != (cell = (orte_rds_cell_desc_t*)ompi_list_remove_first(resources)); i++) {
        values[i] = OBJ_NEW(orte_gpr_value_t);
        if (NULL == values[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
    
        values[i]->addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
        values[i]->segment = strdup(ORTE_RESOURCE_SEGMENT);
    
        if (ORTE_SUCCESS != (rc = orte_schema.get_cell_tokens(&(values[i]->tokens),
                                  &(values[i]->num_tokens), cell->cellid))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }

        values[i]->cnt = ompi_list_get_size(&cell->attributes);
        values[i]->keyvals = (orte_gpr_keyval_t**)malloc(values[i]->cnt * sizeof(orte_gpr_keyval_t*));
        if (NULL == values[i]->keyvals) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
    
        for (j=0, item = ompi_list_get_first(&cell->attributes);
             j < values[i]->cnt && item != ompi_list_get_end(&cell->attributes);
             j++, item = ompi_list_get_next(item)) {
            attr = (orte_rds_cell_attr_t*)item;
            
            values[i]->keyvals[j] = OBJ_NEW(orte_gpr_keyval_t);
            if (NULL == values[i]->keyvals[j]) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
    
            values[i]->keyvals[j]->key = strdup(attr->keyval.key);
            values[i]->keyvals[j]->type = attr->keyval.type;
            if (ORTE_SUCCESS != (rc = orte_gpr.xfer_payload(
                                        &(values[i]->keyvals[j]->value),
                                        &(attr->keyval.value),
                                        values[i]->keyvals[j]->type))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
        }
        OBJ_RELEASE(cell);
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.put(num_vals, values))) {
        ORTE_ERROR_LOG(rc);
    }
    
CLEANUP:
    for (i=0; i < num_vals; i++) {
        if (NULL != values[i]) OBJ_RELEASE(values[i]);
    }
    
    return rc;
}
