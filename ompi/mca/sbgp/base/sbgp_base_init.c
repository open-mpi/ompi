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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/sbgp/sbgp.h"
#include "ompi/mca/sbgp/base/base.h"
#include "ompi/include/ompi/constants.h"

int mca_sbgp_base_init(bool enable_progress_threads, bool enable_mpi_threads)
{
    mca_sbgp_base_component *sbgp_component = NULL;
    mca_base_component_list_item_t *cli;
    opal_list_item_t *item;
    int ret;

    /* loop over component initialization functions */
    for (item = opal_list_get_first((opal_list_t *) &mca_sbgp_base_components_in_use);
            opal_list_get_end((opal_list_t *) &mca_sbgp_base_components_in_use) != item;
            item = opal_list_get_next(item)) {

        cli = (mca_base_component_list_item_t *) item;
        sbgp_component = (mca_sbgp_base_component *)cli->cli_component;

        ret = sbgp_component->sbgp_init_query(true, true);
        if( OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    return OMPI_SUCCESS;
}

