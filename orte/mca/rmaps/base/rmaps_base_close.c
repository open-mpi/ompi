/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
#include "orte/constants.h"

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/rmaps/base/base.h"

int orte_rmaps_base_close(void)
{
    opal_list_item_t *item;

    /* cleanup globals */
    while (NULL != (item = opal_list_remove_first(&orte_rmaps_base.selected_modules))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_rmaps_base.selected_modules);

    mca_base_components_close(orte_rmaps_base.rmaps_output, 
                              &orte_rmaps_base.available_components, NULL);

    return ORTE_SUCCESS;
}
