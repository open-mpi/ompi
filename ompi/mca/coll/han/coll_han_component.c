/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Most of the description of the data layout is in the
 * coll_han_module.c file.
 */

#include "ompi_config.h"

#include "opal/util/show_help.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "coll_han.h"

/*
 * Public string showing the coll ompi_han component version number
 */
const char *mca_coll_han_component_version_string =
    "Open MPI han collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */
static int han_open(void);
static int han_close(void);
static int han_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_coll_han_component_t mca_coll_han_component = {

    /* First, fill in the super */

    {
     /* First, the mca_component_t struct containing meta
        information about the component itself */

     .collm_version = {
                       MCA_COLL_BASE_VERSION_2_0_0,

                       /* Component name and version */
                       .mca_component_name = "han",
                       MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                             OMPI_RELEASE_VERSION),

                       /* Component functions */
                       .mca_open_component = han_open,
                       .mca_close_component = han_close,
                       .mca_register_component_params = han_register,
                       },
     .collm_data = {
                    /* The component is not checkpoint ready */
                    MCA_BASE_METADATA_PARAM_NONE},

     /* Initialization / querying functions */

     .collm_init_query = mca_coll_han_init_query,
     .collm_comm_query = mca_coll_han_comm_query,
     },

    /* han-component specifc information */

    /* (default) priority */
    20,
};

/*
 * Init the component
 */
static int han_open(void)
{
    mca_coll_han_component_t *cs = &mca_coll_han_component;
    if (cs->han_auto_tune) {
        cs->han_auto_tuned =
            (selection *) malloc(2 * cs->han_auto_tune_n * cs->han_auto_tune_c *
                                 cs->han_auto_tune_m * sizeof(selection));
        char *filename = "/home/dycz0fx/results/auto/auto_tuned.bin";
        FILE *file = fopen(filename, "r");
        fread(cs->han_auto_tuned, sizeof(selection),
              2 * cs->han_auto_tune_n * cs->han_auto_tune_c * cs->han_auto_tune_m, file);
        fclose(file);
    }
    return OMPI_SUCCESS;
}


/*
 * Shut down the component
 */
static int han_close(void)
{
    mca_coll_han_component_t *cs = &mca_coll_han_component;
    if (cs->han_auto_tune && cs->han_auto_tuned != NULL) {
        free(cs->han_auto_tuned);
        cs->han_auto_tuned = NULL;
    }
    return OMPI_SUCCESS;
}


/*
 * Register MCA params
 */
static int han_register(void)
{
    mca_base_component_t *c = &mca_coll_han_component.super.collm_version;
    mca_coll_han_component_t *cs = &mca_coll_han_component;

    cs->han_priority = 50;
    (void) mca_base_component_var_register(c, "priority", "Priority of the han coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_priority);

    int coll_han_verbose = 0;
    (void) mca_base_component_var_register(c, "verbose",
                                           "Verbose level",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &coll_han_verbose);
    cs->han_output = opal_output_open(NULL);
    opal_output_set_verbosity(cs->han_output, coll_han_verbose);

    cs->han_bcast_segsize = 65536;
    (void) mca_base_component_var_register(c, "bcast_segsize",
                                           "segment size for bcast",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_bcast_segsize);

    cs->han_bcast_up_module = 0;
    (void) mca_base_component_var_register(c, "bcast_up_module",
                                           "up level module for bcast, 0 libnbc, 1 adapt",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_bcast_up_module);

    cs->han_bcast_low_module = 0;
    (void) mca_base_component_var_register(c, "bcast_low_module",
                                           "low level module for bcast, 0 sm, 1 solo",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_bcast_low_module);

    cs->han_reduce_segsize = 524288;
    (void) mca_base_component_var_register(c, "reduce_segsize",
                                           "segment size for reduce",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_reduce_segsize);

    cs->han_reduce_up_module = 0;
    (void) mca_base_component_var_register(c, "reduce_up_module",
                                           "up level module for allreduce, 0 libnbc, 1 adapt",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->han_reduce_up_module);

    cs->han_reduce_low_module = 0;
    (void) mca_base_component_var_register(c, "reduce_low_module",
                                           "low level module for allreduce, 0 sm, 1 shared",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->han_reduce_low_module);
    cs->han_allreduce_segsize = 524288;
    (void) mca_base_component_var_register(c, "allreduce_segsize",
                                           "segment size for allreduce",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_allreduce_segsize);

    cs->han_allreduce_up_module = 0;
    (void) mca_base_component_var_register(c, "allreduce_up_module",
                                           "up level module for allreduce, 0 libnbc, 1 adapt",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->han_allreduce_up_module);

    cs->han_allreduce_low_module = 0;
    (void) mca_base_component_var_register(c, "allreduce_low_module",
                                           "low level module for allreduce, 0 sm, 1 shared",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->han_allreduce_low_module);

    cs->han_allgather_up_module = 0;
    (void) mca_base_component_var_register(c, "allgather_up_module",
                                           "up level module for allgather, 0 libnbc, 1 adapt",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->han_allgather_up_module);

    cs->han_allgather_low_module = 0;
    (void) mca_base_component_var_register(c, "allgather_low_module",
                                           "low level module for allgather, 0 sm, 1 shared",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->han_allgather_low_module);

    cs->han_gather_up_module = 0;
    (void) mca_base_component_var_register(c, "gather_up_module",
                                           "up level module for gather, 0 libnbc, 1 adapt",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_gather_up_module);

    cs->han_gather_low_module = 0;
    (void) mca_base_component_var_register(c, "gather_low_module",
                                           "low level module for gather, 0 sm, 1 shared",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_gather_low_module);

    cs->han_scatter_up_module = 0;
    (void) mca_base_component_var_register(c, "scatter_up_module",
                                           "up level module for scatter, 0 libnbc, 1 adapt",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_scatter_up_module);

    cs->han_scatter_low_module = 0;
    (void) mca_base_component_var_register(c, "scatter_low_module",
                                           "low level module for scatter, 0 sm, 1 shared",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->han_scatter_low_module);

    cs->han_auto_tune = 0;
    (void) mca_base_component_var_register(c, "auto_tune",
                                           "whether enable auto tune, 0 disable, 1 enable, default 0",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_auto_tune);

    cs->han_auto_tune_n = 5;
    (void) mca_base_component_var_register(c, "auto_tune_n",
                                           "auto tune n",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_auto_tune_n);

    cs->han_auto_tune_c = 3;
    (void) mca_base_component_var_register(c, "auto_tune_c",
                                           "auto tune c",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_auto_tune_c);

    cs->han_auto_tune_m = 21;
    (void) mca_base_component_var_register(c, "auto_tune_m",
                                           "auto tune n",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_auto_tune_m);

    return OMPI_SUCCESS;
}
