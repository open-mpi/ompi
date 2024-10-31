/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2024      NVIDIA CORPORATION. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/util/show_help.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "coll_adapt.h"
#include "coll_adapt_algorithms.h"

/*
 * Public string showing the coll ompi_adapt component version number
 */
const char *mca_coll_adapt_component_version_string =
    "Open MPI ADAPT collective MCA component version " OMPI_VERSION;

/*
 * Local functions
 */
static int adapt_open(void);
static int adapt_close(void);
static int adapt_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_coll_adapt_component_t mca_coll_adapt_component = {
    /* First, fill in the super */
    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */
        .collm_version = {
            MCA_COLL_BASE_VERSION_2_4_0,

            /* Component name and version */
            .mca_component_name = "adapt",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),

            /* Component functions */
            .mca_open_component = adapt_open,
            .mca_close_component = adapt_close,
            .mca_register_component_params = adapt_register,
        },
        .collm_data = {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */
        .collm_init_query = ompi_coll_adapt_init_query,
        .collm_comm_query = ompi_coll_adapt_comm_query,
    },

    /* adapt-component specific information */

    0, /* (default) priority */

    0, /* (default) output stream */
    0, /* (default) verbose level */

    /* default values for non-MCA parameters */
    /* Not specifying values here gives us all 0's */
};

/* Open the component */
static int adapt_open(void)
{
    mca_coll_adapt_component_t *cs = &mca_coll_adapt_component;

    if (cs->adapt_verbose > 0) {
        cs->adapt_output = opal_output_open(NULL);
        opal_output_set_verbosity(cs->adapt_output, cs->adapt_verbose);
    }

    return OMPI_SUCCESS;
}


/* Shut down the component */
static int adapt_close(void)
{
    ompi_coll_adapt_ibcast_fini();
    ompi_coll_adapt_ireduce_fini();

    return OMPI_SUCCESS;
}

static int adapt_verify_mca_variables(void)
{
    return OMPI_SUCCESS;
}

/*
 * Register MCA params
 */
static int adapt_register(void)
{
    mca_base_component_t *c = &mca_coll_adapt_component.super.collm_version;
    mca_coll_adapt_component_t *cs = &mca_coll_adapt_component;

    /* If we want to be selected (i.e., all procs on one node), then
       we should have a high priority */
    cs->adapt_priority = 0;
    (void) mca_base_component_var_register(c, "priority", "Priority of the adapt coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_ALL, &cs->adapt_priority);

    cs->adapt_verbose = ompi_coll_base_framework.framework_verbose;
    (void) mca_base_component_var_register(c, "verbose",
                                           "Verbose level (default set to the collective framework verbosity)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_ALL, &cs->adapt_verbose);

    cs->adapt_context_free_list_min = 64;
    (void) mca_base_component_var_register(c, "context_free_list_min",
                                           "Minimum number of segments in context free list",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_ALL,
                                           &cs->adapt_context_free_list_min);

    cs->adapt_context_free_list_max = 1024;
    (void) mca_base_component_var_register(c, "context_free_list_max",
                                           "Maximum number of segments in context free list",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_ALL,
                                           &cs->adapt_context_free_list_max);

    cs->adapt_context_free_list_inc = 32;
    (void) mca_base_component_var_register(c, "context_free_list_inc",
                                           "Increasement number of segments in context free list",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_ALL,
                                           &cs->adapt_context_free_list_inc);
    ompi_coll_adapt_ibcast_register();
    ompi_coll_adapt_ireduce_register();

    return adapt_verify_mca_variables();
}
