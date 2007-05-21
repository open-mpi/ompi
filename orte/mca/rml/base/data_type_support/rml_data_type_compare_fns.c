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

#include "orte/mca/rml/base/base.h"

/*
 * RML tags
 */
int orte_rml_base_compare_tags(orte_rml_tag_t *value1, orte_rml_tag_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) {
        return ORTE_VALUE1_GREATER;
    } else if (*value1 < *value2) {
        return ORTE_VALUE2_GREATER;
    } else {
        return ORTE_EQUAL;
    }
}
