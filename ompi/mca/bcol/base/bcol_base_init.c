/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/include/ompi/constants.h"

int mca_bcol_base_init(bool enable_progress_threads, bool enable_mpi_threads)
{
    mca_bcol_base_component_t *bcol_component;
    mca_base_component_list_item_t *cli;
    int ret;

    OPAL_LIST_FOREACH(cli, &mca_bcol_base_components_in_use, mca_base_component_list_item_t) {
        bcol_component = (mca_bcol_base_component_t *) cli->cli_component;

        if (false == bcol_component->init_done) {
            ret = bcol_component->collm_init_query(true, true);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }

            bcol_component->init_done = true;
        }
    }

    return OMPI_SUCCESS;
}



