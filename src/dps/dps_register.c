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
#include "include/orte_types.h"

#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns.h"

#include "dps/dps.h"
#include "dps/dps_internal.h"


int orte_dps_register(orte_dps_pack_fn_t pack_fn,
                      orte_dps_unpack_fn_t unpack_fn,
                      const char *name, orte_data_type_t *type)
{
    int ret;
    orte_dps_type_info_t *info;

    /* Check for bozo cases */

    if (NULL == pack_fn || NULL == unpack_fn || NULL == name || NULL == type) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* if type is given (i.e., *type > 0), then just use it.
     * otherwise, go and get a new type id from the name
     * service
     */
    if (0 >= *type) {
        if (ORTE_SUCCESS != (ret = orte_ns.define_data_type(name, type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }
        
    /* Add a new entry to the table */
    info = (orte_dps_type_info_t*)malloc(sizeof(orte_dps_type_info_t));
    if (NULL == info) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    info->odti_name = strdup(name);
    info->odti_pack_fn = pack_fn;
    info->odti_unpack_fn = unpack_fn;
    if (ORTE_SUCCESS != (ret = orte_pointer_array_set_item(orte_dps_types, *type, info))) {
        ORTE_ERROR_LOG(ret);
    }

    /* All done */

    return ret;
}
