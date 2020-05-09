/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
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
#include "coll_han_dynamic.h"
#include "coll_han_dynamic_file.h"

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
    int param;
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

    /*
     * Get the global coll verbosity: it will be ours
     */
    cs->han_output = ompi_coll_base_framework.framework_output;
    opal_output_verbose(1, cs->han_output,
                        "coll:han:component_open: done!");

    cs->topo_level = GLOBAL_COMMUNICATOR;
    return mca_coll_han_init_dynamic_rules();
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
    mca_coll_han_free_dynamic_rules();
    return OMPI_SUCCESS;
}

static bool is_simple_implemented(COLLTYPE_T coll)
{
    switch(coll) {
        case ALLGATHER:
        case ALLREDUCE:
        case BCAST:
        case GATHER:
        case REDUCE:
            return true;
        default:
            return false;
    }
}

const char* mca_coll_han_topo_lvl_to_str(TOPO_LVL_T topo_lvl)
{
    switch(topo_lvl) {
        case INTRA_NODE:
            return "intra_node";
        case INTER_NODE:
            return "inter_node";
        case GLOBAL_COMMUNICATOR:
            return "global_communicator";
        case NB_TOPO_LVL:
        default:
            return "invalid topologic level";
    }
}
const char* mca_coll_han_colltype_to_str(COLLTYPE_T coll)
{
    switch(coll) {
        case ALLGATHER:
            return "allgather";
        case ALLGATHERV:
            return "allgatherv";
        case ALLREDUCE:
            return "allreduce";
        case ALLTOALL:
            return "alltoall";
        case ALLTOALLV:
            return "alltoallv";
        case ALLTOALLW:
            return "alltoallw";
        case BARRIER:
            return "barrier";
        case BCAST:
            return "bcast";
        case EXSCAN:
            return "exscan";
        case GATHER:
            return "gather";
        case GATHERV:
            return "gatherv";
        case REDUCE:
            return "reduce";
        case REDUCESCATTER:
            return "reduce_scatter";
        case REDUCESCATTERBLOCK:
            return "reduce_scatter_block";
        case SCAN:
            return "scan";
        case SCATTER:
            return "scatter";
        case SCATTERV:
            return "scatterv";
        case NEIGHBOR_ALLGATHER:
            return "neighbor_allgather";
        case NEIGHBOR_ALLGATHERV:
            return "neighbor_allgatherv";
        case NEIGHBOR_ALLTOALL:
            return "neighbor_alltoall";
        case NEIGHBOR_ALLTOALLV:
            return "neighbor_alltoallv";
        case NEIGHBOR_ALLTOALLW:
            return "neighbor_alltoallw";
        default:
            return "";
    }
}

/*
 * Register MCA params
 */
static int han_register(void)
{
    mca_base_component_t *c = &mca_coll_han_component.super.collm_version;
    mca_coll_han_component_t *cs = &mca_coll_han_component;

    /* Generated parameters name and description */
    char param_name[100] = "";
    char param_desc[300] = "";
    int param_desc_size;
    COLLTYPE_T coll;
    TOPO_LVL_T topo_lvl;
    COMPONENT_T component;

    cs->han_priority = 0;
    (void) mca_base_component_var_register(c, "priority", "Priority of the han coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_priority);

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

    cs->han_reproducible = 0;
    (void) mca_base_component_var_register(c, "reproducible",
                                           "whether we need reproducible results "
                                           "(enabling this disables optimisations using topology)"
                                           "0 disable 1 enable, default 0",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->han_reproducible);

    /* Simple algorithms MCA parameters */
    for(coll = 0 ; coll < COLLCOUNT ; coll++) {
        cs->use_simple_algorithm[coll] = false;
        if(is_simple_implemented(coll)) {
            snprintf(param_name, 100, "use_simple_%s",
                     mca_coll_han_colltype_to_str(coll));
            snprintf(param_desc, 300, "whether to enable simple algo for %s",
                     mca_coll_han_colltype_to_str(coll));
            mca_base_component_var_register(c, param_name,
                                            param_desc,
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_5,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &(cs->use_simple_algorithm[coll]));
        }
    }

    /* Dynamic rules MCA parameters */
    /* TODO: Find a way to avoid unused entried */
    memset(cs->mca_rules, 0,
           COLLCOUNT * (GLOBAL_COMMUNICATOR+1) * sizeof(COMPONENT_T));
    for(coll = 0 ; coll < COLLCOUNT ; coll++) {
        if(!mca_coll_han_is_coll_dynamic_implemented(coll)) {
            continue;
        }
        /*
         * Default values
         * Do not avoid to set correct default parameters
         */
        cs->mca_rules[coll][INTRA_NODE] = TUNED;
        cs->mca_rules[coll][INTER_NODE] = BASIC;
        cs->mca_rules[coll][GLOBAL_COMMUNICATOR] = HAN;

        for(topo_lvl = 0 ; topo_lvl < NB_TOPO_LVL ; topo_lvl++) {

            snprintf(param_name, 100, "%s_dynamic_%s_module",
                     mca_coll_han_colltype_to_str(coll),
                     mca_coll_han_topo_lvl_to_str(topo_lvl));

            param_desc_size = snprintf(param_desc, 300,
                                       "Collective module to use for "
                                       "collective %s on %s topological level: ",
                                       mca_coll_han_colltype_to_str(coll),
                                       mca_coll_han_topo_lvl_to_str(topo_lvl));
            /*
             * Exhaustive description:
             * 0 = self; 1 = basic; 2 = libnbc; ...
             * FIXME: Do not print component not providing this collective
             */
            for(component = 0 ; component < COMPONENTS_COUNT ; component++) {
                if(HAN == component && GLOBAL_COMMUNICATOR != topo_lvl) {
                    /* Han can only be used on the global communicator */
                    continue;
                }
                param_desc_size += snprintf(param_desc+param_desc_size, 300,
                                            "%d = %s; ",
                                            component,
                                            components_name[component]);
            }

            mca_base_component_var_register(c, param_name, param_desc,
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &(cs->mca_rules[coll][topo_lvl]));
        }
    }

    /*
     * TODO: remove the following lines when auto-tune is added back to the code
     */
    cs->han_auto_tune = 0;

    cs->han_auto_tune_n = 5;
    cs->han_auto_tune_c = 3;
    cs->han_auto_tune_m = 21;
#if 0
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
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->han_auto_tune_m);
#endif

    /* Dynamic rules */
    cs->use_dynamic_file_rules = false;
    (void) mca_base_component_var_register(&mca_coll_han_component.super.collm_version,
                                           "use_dynamic_file_rules",
                                           "Switch used to decide if we use "
                                           "dynamic module choice rules "
                                           "defines by file",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &(cs->use_dynamic_file_rules));

    cs->dynamic_rules_filename = NULL;
    (void) mca_base_component_var_register(&mca_coll_han_component.super.collm_version,
                                           "dynamic_rules_filename",
                                           "Filename of configuration file that "
                                           "contains the dynamic module choice rules",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &(cs->dynamic_rules_filename));

    cs->dump_dynamic_rules = false;
    (void) mca_base_component_var_register(&mca_coll_han_component.super.collm_version,
                                           "dump_dynamic_rules",
                                           "Switch used to decide if we dump "
                                           "dynamic rules provided by "
                                           "configuration file",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &(cs->dump_dynamic_rules));

    if((cs->dump_dynamic_rules || NULL != cs->dynamic_rules_filename)
       && !cs->use_dynamic_file_rules) {
        opal_output_verbose(0, cs->han_output,
                            "coll:han:han_register "
                            "you asked for dynamic rules "
                            "but they are not activated. "
                            "Check coll_han_use_dynamic_file_rules "
                            "MCA parameter");
    }

    cs->max_dynamic_errors = 10;
    (void) mca_base_component_var_register(&mca_coll_han_component.super.collm_version,
                                           "max_dynamic_errors",
                                           "Number of dynamic rules module/function "
                                           "errors printed on rank 0 "
                                           "with a 0 verbosity."
                                           "Useless if coll_base_verbose is 30 or more.",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &(cs->max_dynamic_errors));


    return OMPI_SUCCESS;
}
