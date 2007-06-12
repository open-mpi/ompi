/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

#include "orte_config.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/rml/base/base.h"

/*
 * RML TAG
 */
int orte_rml_base_pack_tag(orte_buffer_t *buffer, const void *src,
                           orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;

    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_RML_TAG_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}
