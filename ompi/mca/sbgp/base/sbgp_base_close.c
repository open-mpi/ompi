/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#include <stdio.h>

#include "ompi/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/sbgp/sbgp.h"
#include "ompi/mca/sbgp/base/base.h"
#include "ompi/include/ompi/constants.h"


int mca_sbgp_base_close(void)
{

    /* Close all remaining available modules */

    mca_base_components_close(mca_sbgp_base_output,
                              &mca_sbgp_base_components_opened, NULL);

    /* All done */

    return OMPI_SUCCESS;
}

