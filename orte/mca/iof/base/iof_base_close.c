/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"


int orte_iof_base_close(void)
{
    /* finalize the module */
    if (NULL != orte_iof.finalize) {
        orte_iof.finalize();
    }
    
    /* shutdown any remaining opened components */
    if (0 != opal_list_get_size(&orte_iof_base.iof_components_opened)) {
        mca_base_components_close(orte_iof_base.iof_output, 
                              &orte_iof_base.iof_components_opened, NULL);
    }
    OBJ_DESTRUCT(&orte_iof_base.iof_components_opened);

    OBJ_DESTRUCT(&orte_iof_base.iof_write_output_lock);


    return ORTE_SUCCESS;
}

