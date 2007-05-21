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

#include "orte/mca/rml/base/base.h"

/*
 * RML TAG
 */
int orte_rml_base_print_tag(char **output, char *prefix, orte_rml_tag_t *src, orte_data_type_t type)
{
    char *pfx2;
    
    /* set default result */
    *output = NULL;

    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }

    asprintf(output, "%sRML tag value %ld", pfx2, (long)*src);
    free(pfx2);

    return ORTE_SUCCESS;
}
