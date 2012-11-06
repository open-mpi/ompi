/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
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
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"


int orte_odls_base_close(void)
{
    int i;
    orte_proc_t *proc;
    opal_list_item_t *item;

    /* cleanup ODLS globals */
    while (NULL != (item = opal_list_remove_first(&orte_odls_globals.xterm_ranks))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_odls_globals.xterm_ranks);
    
    /* cleanup the global list of local children and job data */
    for (i=0; i < orte_local_children->size; i++) {
        if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            OBJ_RELEASE(proc);
        }
    }
    OBJ_RELEASE(orte_local_children);

    /* Close all available components (only one in this case)  */

    mca_base_components_close(orte_odls_globals.output, 
                              &orte_odls_base.available_components, NULL);

    /* Close the framework output */
    opal_output_close (orte_odls_globals.output);
    orte_odls_globals.output = -1;

    /* All done */

    return ORTE_SUCCESS;
}
