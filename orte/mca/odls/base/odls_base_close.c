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

#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"


int orte_odls_base_close(void)
{
    /* cleanup globals */
    OBJ_DESTRUCT(&orte_odls_globals.mutex);
    OBJ_DESTRUCT(&orte_odls_globals.cond);
    OBJ_DESTRUCT(&orte_odls_globals.children);
    OBJ_DESTRUCT(&orte_odls_globals.jobs);
    if (NULL != orte_odls_globals.dmap && NULL != orte_odls_globals.dmap->bytes) {
        free(orte_odls_globals.dmap->bytes);
        free(orte_odls_globals.dmap);
    }
    
    /* if no components are available, then punt */
    if (!orte_odls_base.components_available) {
        return ORTE_SUCCESS;
    }
    
    /* Close all available components (only one in this case)  */

    mca_base_components_close(orte_odls_globals.output, 
                              &orte_odls_base.available_components, NULL);

    /* All done */

    return ORTE_SUCCESS;
}
