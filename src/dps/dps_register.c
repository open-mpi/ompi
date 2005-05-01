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

#include "mca/errmgr/errmgr.h"

#include "dps/dps.h"
#include "dps/dps_internal.h"


int orte_dps_register(orte_dps_pack_fn_t pack_fn,
                      orte_dps_unpack_fn_t unpack_fn,
                      const char *name, orte_data_type_t type)
{
    int ret;
    orte_dps_type_info_t info;

    /* Check for bozo cases */

    if (NULL == pack_fn || NULL == unpack_fn || NULL == type || NULL == name) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* Add a new entry to the table */

    info.odti_name = strdup(name);
    info.odti_num = type;
    info.odti_pack_fn = pack_fn;
    info.odti_unpack_fn = unpack_fn;
    ret = orte_value_array_append_item(&orte_dps_types, &info);

    /* All done */

    return ret;
}
