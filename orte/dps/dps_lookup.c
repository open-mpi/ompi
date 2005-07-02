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

#include "dps/dps.h"
#include "dps/dps_internal.h"


char *orte_dps_lookup_data_type(orte_data_type_t type)
{
    orte_dps_type_info_t *info;
    char *name;
    
    info = orte_pointer_array_get_item(orte_dps_types, type);
    if (NULL != info) { /* type found on list */
        name = strdup(info->odti_name);
        return name;
    }

    return NULL;
}
