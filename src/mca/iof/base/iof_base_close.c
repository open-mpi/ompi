/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "event/event.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"


int mca_iof_base_close(void)
{
    /* close any remaining opened components */
    if (0 != ompi_list_get_size(&mca_iof_base_components_opened)) {
        mca_base_components_close(mca_iof_base_output, 
                              &mca_iof_base_components_opened, NULL);
    }
    return OMPI_SUCCESS;
}

