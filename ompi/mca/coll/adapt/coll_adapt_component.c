/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
            MCA_COLL_BASE_VERSION_2_0_0,

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

    /* (default) priority */
    0,

    /* (default) verbose level */
    0,

    /* default values for non-MCA parameters */
    /* Not specifying values here gives us all 0's */
};

/* Open the component */
static int adapt_open(void)
{
    int param;
    mca_coll_adapt_component_t *cs = &mca_coll_adapt_component;

    /*
     * Get the global coll verbosity: it will be ours
     */
    param = mca_base_var_find("ompi", "coll", "base", "verbose");
    if (param >= 0) {
        const int *verbose = NULL;
        mca_base_var_get_value(param, &verbose, NULL, NULL);
        if (verbose && verbose[0] > 0) {
            cs->adapt_output = opal_output_open(NULL);
            opal_output_set_verbosity(cs->adapt_output, verbose[0]);
        }
    }

    opal_output_verbose(1, cs->adapt_output,
                        "coll:adapt:component_open: done!");

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
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->adapt_priority);

    int adapt_verbose = 0;
    (void) mca_base_component_var_register(c, "verbose",
                                           "Verbose level",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &adapt_verbose);
    cs->adapt_output = opal_output_open(NULL);
    opal_output_set_verbosity(cs->adapt_output, adapt_verbose);

    cs->adapt_context_free_list_min = 10;
    (void) mca_base_component_var_register(c, "context_free_list_min",
                                           "Minimum number of segments in context free list",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->adapt_context_free_list_min);

    cs->adapt_context_free_list_max = 10000;
    (void) mca_base_component_var_register(c, "context_free_list_max",
                                           "Maximum number of segments in context free list",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->adapt_context_free_list_max);

    cs->adapt_context_free_list_inc = 10;
    (void) mca_base_component_var_register(c, "context_free_list_inc",
                                           "Increasement number of segments in context free list",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->adapt_context_free_list_inc);
    ompi_coll_adapt_ibcast_init();
    ompi_coll_adapt_ireduce_init();

    return adapt_verify_mca_variables();
}
