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

#include "orte/dss/dss_types.h"

#include "orte/mca/odls/base/odls_private.h"

/*
 * STANDARD RELEASE FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
void orte_odls_std_release(orte_data_value_t *value)
{
    free(value->data);
    value->data = NULL;
}
