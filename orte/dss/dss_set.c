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

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss.h"
#include "orte/dss/dss_internal.h"

int orte_dss_set(orte_data_value_t *value, void *new_value, orte_data_type_t type)
{
    /* check for error */
    if (NULL == value || NULL == new_value) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* see if a value is already loaded - if so, that's just wrong. We can't
       release it ourselves because we don't know for certain that the data
       was stored dynamically
    */
    if (NULL != value->data) {
        ORTE_ERROR_LOG(ORTE_ERR_DATA_OVERWRITE_ATTEMPT);
        return ORTE_ERR_DATA_OVERWRITE_ATTEMPT;
    }

    /* set the type */
    value->type = type;

    /* point the value to the data object */
    value->data = new_value;

    return ORTE_SUCCESS;
}

