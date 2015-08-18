/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"

#include "orte/util/show_help.h"

#include "orte/mca/rmaps/base/base.h"
#include "rmaps_staged.h"

/*
 * Local functions
 */

static int orte_rmaps_staged_open(void);
static int orte_rmaps_staged_close(void);
static int orte_rmaps_staged_query(mca_base_module_t **module, int *priority);

orte_rmaps_base_component_t mca_rmaps_staged_component = {
    .base_version = {
        ORTE_RMAPS_BASE_VERSION_2_0_0,

        .mca_component_name = "staged",
        MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                              ORTE_RELEASE_VERSION),
        .mca_open_component = orte_rmaps_staged_open,
        .mca_close_component = orte_rmaps_staged_close,
        .mca_query_component = orte_rmaps_staged_query,
    },
    .base_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};


/**
  * component open/close/init function
  */
static int orte_rmaps_staged_open(void)
{
    return ORTE_SUCCESS;
}


static int orte_rmaps_staged_query(mca_base_module_t **module, int *priority)
{
    *priority = 5;
    *module = (mca_base_module_t *)&orte_rmaps_staged_module;
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_staged_close(void)
{
    return ORTE_SUCCESS;
}


