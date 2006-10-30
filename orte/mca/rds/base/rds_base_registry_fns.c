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
/** @file:
 */

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include <string.h>

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/schema/schema.h"

#include "orte/mca/rds/base/rds_private.h"
#include "orte/mca/rds/base/base.h"

int orte_rds_base_store_resource(opal_list_t *resources)
{
    orte_rds_cell_desc_t *cell;
    opal_list_item_t *item;
    orte_gpr_value_t **values;
    orte_rds_cell_attr_t *attr;
    orte_std_cntr_t i, j, num_vals, num_attr;
    int rc;

    if (NULL == resources) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    num_vals = (orte_std_cntr_t)opal_list_get_size(resources);
    if (0 == num_vals) {  /* nothing to do */
        return ORTE_SUCCESS;
    }

    values = (orte_gpr_value_t**)malloc(num_vals * sizeof(orte_gpr_value_t*));
    if (NULL == values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    for (i=0; i < num_vals && NULL != (cell = (orte_rds_cell_desc_t*)opal_list_remove_first(resources)); i++) {
        num_attr = (orte_std_cntr_t)opal_list_get_size(&cell->attributes);
        if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&(values[i]), ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR,
                                                        ORTE_RESOURCE_SEGMENT, num_attr, 0))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }

        if (ORTE_SUCCESS != (rc = orte_schema.get_cell_tokens(&(values[i]->tokens),
                                  &(values[i]->num_tokens), cell->cellid))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }

        for (j=0, item = opal_list_get_first(&cell->attributes);
             j < values[i]->cnt && item != opal_list_get_end(&cell->attributes);
             j++, item = opal_list_get_next(item)) {
            attr = (orte_rds_cell_attr_t*)item;

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[i]->keyvals[j]), attr->keyval.key, attr->keyval.value->type, attr->keyval.value->data))) {
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
    if (NULL != values) free(values);

    return rc;
}
