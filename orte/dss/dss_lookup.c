/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
#include "orte/orte_types.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss.h"
#include "orte/dss/dss_internal.h"


char *orte_dss_lookup_data_type(orte_data_type_t type)
{
    orte_dss_type_info_t *info;
    char *name;

    if (!(type < orte_dss_types->size)) {
        return NULL;
    }

    info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, type);
    if (NULL != info) { /* type found on list */
        name = strdup(info->odti_name);
        return name;
    }

    return NULL;
}
