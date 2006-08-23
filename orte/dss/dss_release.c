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

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss_internal.h"
#include "opal/util/output.h"

void orte_dss_release(orte_data_value_t *value)
{
    orte_dss_type_info_t *info = NULL;

    /* check for error */
    if (NULL == value) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return;
    }

    /* Lookup the release function for this type and call it */

    if (!(value->type < orte_dss_types->size) ||
        (NULL == (info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, value->type)))) {
        ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
        return;
    }

    info->odti_release_fn(value);
}

/*
 * STANDARD RELEASE FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
void orte_dss_std_release(orte_data_value_t *value)
{
    free(value->data);
    value->data = NULL;
}

/*
 * STANDARD OBJECT RELEASE FUNCTION - WORKS FOR EVERYTHING
 */
void orte_dss_std_obj_release(orte_data_value_t *value)
{
   OBJ_RELEASE(value->data);
}


/*
 * ORTE_BYTE_OBJECT
 */
void orte_dss_release_byte_object(orte_data_value_t *value)
{
   orte_byte_object_t *bo;

   bo = (orte_byte_object_t*)value->data;
   free(bo->bytes);

   free(value->data);
   value->data = NULL;
}
