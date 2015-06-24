/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/mca/base/base.h"

#include "orte/util/proc_info.h"

#include "iof_mrorted.h"

/*
 * Local functions
 */
static int mr_orted_open(void);
static int mr_orted_close(void);
static int mr_orted_query(mca_base_module_t **module, int *priority);


/*
 * Public string showing the iof mr_orted component version number
 */
const char *mca_iof_mr_orted_component_version_string =
"Open MPI mr_orted iof MCA component version " ORTE_VERSION;


orte_iof_mrorted_component_t mca_iof_mr_orted_component = {
    {
        .iof_version = {
            ORTE_IOF_BASE_VERSION_2_0_0,

            .mca_component_name = "mr_orted",
            MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                                  ORTE_RELEASE_VERSION),

            /* Component open, close, and query functions */
            .mca_open_component = mr_orted_open,
            .mca_close_component = mr_orted_close,
            .mca_query_component = mr_orted_query,
        },
        .iof_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
    }
};

/**
  * component open/close/init function
  */
static int mr_orted_open(void)
{
    /* Nothing to do */
    return ORTE_SUCCESS;
}

static int mr_orted_close(void)
{
    return ORTE_SUCCESS;
}


static int mr_orted_query(mca_base_module_t **module, int *priority)
{
    if (ORTE_PROC_IS_DAEMON && orte_map_reduce) {
        *priority = 1000;
        *module = (mca_base_module_t *) &orte_iof_mrorted_module;
        return ORTE_SUCCESS;
    }

    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}

