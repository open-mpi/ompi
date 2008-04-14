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

#include "opal/util/trace.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/runtime/orte_globals.h"

#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"


int orte_odls_base_close(void)
{
    int i;
    char **nodes;
    
    OPAL_TRACE(5);
    
    /* cleanup globals */
    OBJ_DESTRUCT(&orte_odls_globals.mutex);
    OBJ_DESTRUCT(&orte_odls_globals.cond);
    OBJ_DESTRUCT(&orte_odls_globals.children);
    OBJ_DESTRUCT(&orte_odls_globals.jobs);

    nodes = (char**)orte_daemonmap.addr;
    for (i=0; i < orte_daemonmap.size; i++) {
        if (NULL != nodes[i]) {
            free(nodes[i]);
        }
    }
    OBJ_DESTRUCT(&orte_daemonmap);

    /* if no components are available, then punt */
    if (!orte_odls_base.components_available) {
        return ORTE_SUCCESS;
    }
    
    /* If we have a selected component and module, then finalize it */

    if (orte_odls_base.selected) {
        orte_odls_base.selected_component.finalize();
    }

    /* Close all available components (only one in this case)  */

    mca_base_components_close(orte_odls_globals.output, 
                            &orte_odls_base.available_components, NULL);

    /* All done */

    return ORTE_SUCCESS;
}
