/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * Copyright (c) 2020-2022 Bull S.A.S. All rights reserved.
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
#include "opal/util/argv.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "coll_han.h"
#include "coll_han_dynamic.h"
#include "coll_han_dynamic_file.h"
#include "coll_han_algorithms.h"
#include "ompi/mca/coll/base/coll_base_util.h"

/*
 * Public string showing the coll ompi_han component version number
 */
const char *mca_coll_han_component_version_string =
    "Open MPI HAN collective MCA component version " OMPI_VERSION;

ompi_coll_han_components ompi_coll_han_available_components[COMPONENTS_COUNT] = {
    { SELF, "self",  NULL },
    { BASIC, "basic", NULL },
    { LIBNBC, "libnbc", NULL },
    { TUNED, "tuned", NULL },
    { SM, "sm", NULL },
    { ADAPT, "adapt", NULL },
    { HAN, "han", NULL }
};

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
            MCA_COLL_BASE_VERSION_2_4_0,

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

    /* han-component specific information */

    /* (default) priority */
    .han_priority = 35,
    /* workaround for nvcc compiler */
    .dynamic_rules_filename = NULL,
};

/*
 * Init the component
 */
static int han_open(void)
{
    /* Get the global coll verbosity: it will be ours */
    if (mca_coll_han_component.han_output_verbose) {
        mca_coll_han_component.han_output = opal_output_open(NULL);
        opal_output_set_verbosity(mca_coll_han_component.han_output,
                                  mca_coll_han_component.han_output_verbose);
    } else {
        mca_coll_han_component.han_output = ompi_coll_base_framework.framework_output;
    }



    return mca_coll_han_init_dynamic_rules();
}

/*
 * Shut down the component
 */
static int han_close(void)
{
    mca_coll_han_free_dynamic_rules();

    free(mca_coll_han_component.han_op_module_name.bcast.han_op_up_module_name);
    mca_coll_han_component.han_op_module_name.bcast.han_op_up_module_name = NULL;
    free(mca_coll_han_component.han_op_module_name.bcast.han_op_low_module_name);
    mca_coll_han_component.han_op_module_name.bcast.han_op_low_module_name = NULL;

    free(mca_coll_han_component.han_op_module_name.reduce.han_op_up_module_name);
    mca_coll_han_component.han_op_module_name.reduce.han_op_up_module_name = NULL;
    free(mca_coll_han_component.han_op_module_name.reduce.han_op_low_module_name);
    mca_coll_han_component.han_op_module_name.reduce.han_op_low_module_name = NULL;

    free(mca_coll_han_component.han_op_module_name.allreduce.han_op_up_module_name);
    mca_coll_han_component.han_op_module_name.allreduce.han_op_up_module_name = NULL;
    free(mca_coll_han_component.han_op_module_name.allreduce.han_op_low_module_name);
    mca_coll_han_component.han_op_module_name.allreduce.han_op_low_module_name = NULL;

    free(mca_coll_han_component.han_op_module_name.allgather.han_op_up_module_name);
    mca_coll_han_component.han_op_module_name.allgather.han_op_up_module_name = NULL;
    free(mca_coll_han_component.han_op_module_name.allgather.han_op_low_module_name);
    mca_coll_han_component.han_op_module_name.allgather.han_op_low_module_name = NULL;

    free(mca_coll_han_component.han_op_module_name.gather.han_op_up_module_name);
    mca_coll_han_component.han_op_module_name.gather.han_op_up_module_name = NULL;
    free(mca_coll_han_component.han_op_module_name.gather.han_op_low_module_name);
    mca_coll_han_component.han_op_module_name.gather.han_op_low_module_name = NULL;

    free(mca_coll_han_component.han_op_module_name.gatherv.han_op_up_module_name);
    mca_coll_han_component.han_op_module_name.gatherv.han_op_up_module_name = NULL;
    free(mca_coll_han_component.han_op_module_name.gatherv.han_op_low_module_name);
    mca_coll_han_component.han_op_module_name.gatherv.han_op_low_module_name = NULL;

    free(mca_coll_han_component.han_op_module_name.scatter.han_op_up_module_name);
    mca_coll_han_component.han_op_module_name.scatter.han_op_up_module_name = NULL;
    free(mca_coll_han_component.han_op_module_name.scatter.han_op_low_module_name);
    mca_coll_han_component.han_op_module_name.scatter.han_op_low_module_name = NULL;

    free(mca_coll_han_component.han_op_module_name.scatterv.han_op_up_module_name);
    mca_coll_han_component.han_op_module_name.scatterv.han_op_up_module_name = NULL;
    free(mca_coll_han_component.han_op_module_name.scatterv.han_op_low_module_name);
    mca_coll_han_component.han_op_module_name.scatterv.han_op_low_module_name = NULL;

    return OMPI_SUCCESS;
}

/*
 * @return true if the collective has a simple version that does not use tasks.
 */
static bool is_simple_implemented(COLLTYPE_T coll)
{
    switch(coll) {
        case ALLGATHER:
        case ALLREDUCE:
        case BCAST:
        case GATHER:
        case REDUCE:
        case SCATTER:
            return true;
        default:
            return false;
    }
}

/**
 * topo level conversions both ways; str <-> id
 * An enum is used for conversions.
 */
static mca_base_var_enum_value_t level_enumerator[] = {
    { INTRA_NODE, "intra_node" },
    { INTER_NODE, "inter_node" },
    { GLOBAL_COMMUNICATOR, "global_communicator" },
    { 0 }
};

/*
 * Stringifier for topological level
 */
const char* mca_coll_han_topo_lvl_to_str(TOPO_LVL_T topo_lvl_id)
{
    for (int i = 0; level_enumerator[i].string != NULL; i++) {
        if (topo_lvl_id == (TOPO_LVL_T) level_enumerator[i].value) {
            return level_enumerator[i].string;
        }
    }
    return "invalid topologic level";
}
int mca_coll_han_topo_lvl_name_to_id(const char *topo_level_name)
{
    for (int i = 0; level_enumerator[i].string != NULL; i++) {
        if (0 == strcmp(topo_level_name, level_enumerator[i].string)) {
            return i;
        }
    }
    return -1;
}

static int
mca_coll_han_query_module_from_mca(mca_base_component_t* c,
                                   const char* param_name,
                                   const char* param_doc,
                                   int info_level,
                                   uint32_t* module_id,
                                   char** storage)
{
    char *module_name, *endptr = NULL;

    int mod_id = COMPONENTS_COUNT-1;
    mod_id = (*module_id > (uint32_t)mod_id) ? mod_id : (int)*module_id; /* stay in range */

    *storage = ompi_coll_han_available_components[mod_id].component_name;

    (void) mca_base_component_var_register(c, param_name, param_doc,
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           info_level,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);
    module_name = *storage;
    mod_id = strtol(module_name, &endptr, 10);
    if( module_name == endptr ) {  /* no conversion, maybe we got a module name instead */
        /* Convert module name to id */
        mod_id = mca_coll_han_component_name_to_id(module_name);
    }
    /* Keep the module in the range */
    *module_id = (mod_id < 0) ? 0 : mod_id;

    return OMPI_SUCCESS;
}

/*
 * Register MCA params
 */
static int han_register(void)
{
    mca_base_component_t *c = &mca_coll_han_component.super.collm_version;
    mca_coll_han_component_t *cs = &mca_coll_han_component;

    /* Generated parameters name and description */
    char param_name[128], param_desc[256];
    int param_desc_size;
    COLLTYPE_T coll;
    TOPO_LVL_T topo_lvl;
    COMPONENT_T component;

    (void) mca_base_component_var_register(c, "priority", "Priority of the HAN coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_priority);

    cs->han_output_verbose = 0;
    (void) mca_base_component_var_register(c, "verbose", "Verbosity of the HAN coll component (use coll base verbosity if not set)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_output_verbose);

    cs->han_bcast_segsize = 65536;
    (void) mca_base_component_var_register(c, "bcast_segsize",
                                           "segment size for bcast",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_bcast_segsize);

    cs->han_bcast_up_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "bcast_up_module",
                                              "up level module for bcast, 0 libnbc, 1 adapt",
                                              OPAL_INFO_LVL_9, &cs->han_bcast_up_module,
                                              &cs->han_op_module_name.bcast.han_op_up_module_name);

    cs->han_bcast_low_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "bcast_low_module",
                                              "low level module for bcast, 0 tuned, 1 sm",
                                              OPAL_INFO_LVL_9,
                                              &cs->han_bcast_low_module,
                                              &cs->han_op_module_name.bcast.han_op_low_module_name);

    cs->han_reduce_segsize = 65536;
    (void) mca_base_component_var_register(c, "reduce_segsize",
                                           "segment size for reduce",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_reduce_segsize);

    cs->han_reduce_up_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "reduce_up_module",
                                              "up level module for allreduce, 0 libnbc, 1 adapt",
                                              OPAL_INFO_LVL_9, &cs->han_reduce_up_module,
                                              &cs->han_op_module_name.reduce.han_op_up_module_name);

    cs->han_reduce_low_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "reduce_low_module",
                                              "low level module for allreduce, 0 tuned, 1 sm",
                                              OPAL_INFO_LVL_9, &cs->han_reduce_low_module,
                                              &cs->han_op_module_name.reduce.han_op_low_module_name);

    cs->han_allreduce_segsize = 65536;
    (void) mca_base_component_var_register(c, "allreduce_segsize",
                                           "segment size for allreduce",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_allreduce_segsize);

    cs->han_allreduce_up_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "allreduce_up_module",
                                              "up level module for allreduce, 0 libnbc, 1 adapt",
                                              OPAL_INFO_LVL_9, &cs->han_allreduce_up_module,
                                              &cs->han_op_module_name.allreduce.han_op_up_module_name);

    cs->han_allreduce_low_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "allreduce_low_module",
                                              "low level module for allreduce, 0 tuned, 1 sm",
                                              OPAL_INFO_LVL_9, &cs->han_allreduce_low_module,
                                              &cs->han_op_module_name.allreduce.han_op_low_module_name);

    cs->han_allgather_up_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "allgather_up_module",
                                              "up level module for allgather, 0 libnbc, 1 adapt",
                                              OPAL_INFO_LVL_9, &cs->han_allgather_up_module,
                                              &cs->han_op_module_name.allgather.han_op_up_module_name);

    cs->han_allgather_low_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "allgather_low_module",
                                              "low level module for allgather, 0 tuned, 1 sm",
                                              OPAL_INFO_LVL_9, &cs->han_allgather_low_module,
                                              &cs->han_op_module_name.allgather.han_op_low_module_name);

    cs->han_gather_up_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "gather_up_module",
                                              "up level module for gather, 0 libnbc, 1 adapt",
                                              OPAL_INFO_LVL_9, &cs->han_gather_up_module,
                                              &cs->han_op_module_name.gather.han_op_up_module_name);

    cs->han_gather_low_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "gather_low_module",
                                              "low level module for gather, 0 tuned, 1 sm",
                                              OPAL_INFO_LVL_9, &cs->han_gather_low_module,
                                              &cs->han_op_module_name.gather.han_op_low_module_name);

    cs->han_gatherv_up_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "gatherv_up_module",
                                              "up level module for gatherv, 0 basic",
                                              OPAL_INFO_LVL_9, &cs->han_gatherv_up_module,
                                              &cs->han_op_module_name.gatherv.han_op_up_module_name);

    cs->han_gatherv_low_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "gatherv_low_module",
                                              "low level module for gatherv, 0 basic",
                                              OPAL_INFO_LVL_9, &cs->han_gatherv_low_module,
                                              &cs->han_op_module_name.gatherv.han_op_low_module_name);

    cs->han_scatter_up_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "scatter_up_module",
                                              "up level module for scatter, 0 libnbc, 1 adapt",
                                              OPAL_INFO_LVL_9, &cs->han_scatter_up_module,
                                              &cs->han_op_module_name.scatter.han_op_up_module_name);

    cs->han_scatter_low_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "scatter_low_module",
                                              "low level module for scatter, 0 tuned, 1 sm",
                                              OPAL_INFO_LVL_9, &cs->han_scatter_low_module,
                                              &cs->han_op_module_name.scatter.han_op_low_module_name);

    cs->han_scatterv_up_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "scatterv_up_module",
                                              "up level module for scatterv, 0 basic",
                                              OPAL_INFO_LVL_9, &cs->han_scatterv_up_module,
                                              &cs->han_op_module_name.scatterv.han_op_up_module_name);

    cs->han_scatterv_low_module = 0;
    (void) mca_coll_han_query_module_from_mca(c, "scatterv_low_module",
                                              "low level module for scatterv, 0 basic",
                                              OPAL_INFO_LVL_9, &cs->han_scatterv_low_module,
                                              &cs->han_op_module_name.scatterv.han_op_low_module_name);

    cs->han_reproducible = 0;
    (void) mca_base_component_var_register(c, "reproducible",
                                           "whether we need reproducible results "
                                           "(enabling this disables optimisations using topology)"
                                           "0 disable 1 enable, default 0",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->han_reproducible);
    /*
     * Han algorithms MCA parameters for each collective.
     * Shows algorithms thanks to enumerator
     */
    if (OMPI_ERROR == mca_coll_han_init_algorithms()) { // needs to be initialised here to show available algorithms
       return OMPI_ERROR;
    }

    mca_base_var_enum_t *new_enum;
    for(coll = 0 ; coll < COLLCOUNT ; coll++) {
        if (!mca_coll_han_is_coll_dynamic_implemented(coll)
            || (0 == mca_coll_han_component.num_available_algorithms[coll])) {
          continue;
        }
        cs->use_algorithm[coll] = 0; // default algorithm is 0
        snprintf(param_name, sizeof(param_name), "use_%s_algorithm",
                 mca_coll_base_colltype_to_str(coll));
        snprintf(param_desc, sizeof(param_desc), "which han algorithm is used for %s",
                 mca_coll_base_colltype_to_str(coll));
        // note: the enumerator is create in mca_coll_han_init_algorithms()
        (void) mca_base_var_enum_create(param_name,
                                        mca_coll_han_component.algorithm_enumerator[coll],
                                        &new_enum);
        cs->use_algorithm_param[coll] = mca_base_component_var_register(c,
                                        param_name,
                                        param_desc,
                                        MCA_BASE_VAR_TYPE_INT, new_enum, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                        OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_ALL,
                                        &(cs->use_algorithm[coll]));
        OBJ_RELEASE(new_enum);
    }

    /*
     * Simple algorithms MCA parameters :
     * using simple algorithms will just perform hierarchical communications.
     * By default communications are also split into tasks
     * to handle thread noise
     */
    for(coll = 0 ; coll < COLLCOUNT ; coll++) {
        if (coll != GATHER) {
            cs->use_simple_algorithm[coll] = false;
        } else {
            cs->use_simple_algorithm[coll] = true;
        }
        if(is_simple_implemented(coll)) {
            const char *collstr = mca_coll_base_colltype_to_str(coll);
            snprintf(param_name, sizeof(param_name), "use_simple_%s",
                     collstr);
            snprintf(param_desc, sizeof(param_desc), "whether to enable simple algorithm for %s. "
                     "Prefer use_%s_algorithm=simple or configuration file instead.",
                     collstr, collstr);
            mca_base_component_var_register(c, param_name,
                                            param_desc,
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0,
                                            MCA_BASE_VAR_FLAG_DEPRECATED,
                                            OPAL_INFO_LVL_5,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &(cs->use_simple_algorithm[coll]));
        }
    }

    /* Dynamic rules MCA parameters */
    memset(cs->mca_sub_components, 0,
           COLLCOUNT * (GLOBAL_COMMUNICATOR+1) * sizeof(COMPONENT_T));

    for(coll = 0; coll < COLLCOUNT; coll++) {
        if(!mca_coll_han_is_coll_dynamic_implemented(coll)) {
            continue;
        }
        /*
         * Default values
         */
        for (topo_lvl = 0 ; topo_lvl < GLOBAL_COMMUNICATOR ; topo_lvl++) {
            cs->mca_sub_components[coll][topo_lvl] = TUNED;
        }
        cs->mca_sub_components[coll][GLOBAL_COMMUNICATOR] = HAN;
    }
    /* Specific default values */

    /* Dynamic rule MCA var registration */
    for(coll = 0; coll < COLLCOUNT; coll++) {
        if(!mca_coll_han_is_coll_dynamic_implemented(coll)) {
            continue;
        }
        for(topo_lvl = 0; topo_lvl < NB_TOPO_LVL; topo_lvl++) {

            snprintf(param_name, sizeof(param_name), "%s_dynamic_%s_module",
                     mca_coll_base_colltype_to_str(coll),
                     mca_coll_han_topo_lvl_to_str(topo_lvl));

            param_desc_size = snprintf(param_desc, sizeof(param_desc),
                                       "Collective module to use for %s on %s topological level: ",
                                       mca_coll_base_colltype_to_str(coll),
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
                param_desc_size += snprintf(param_desc+param_desc_size, sizeof(param_desc) - param_desc_size,
                                            "%d = %s; ",
                                            component,
                                            ompi_coll_han_available_components[component].component_name);
            }

            mca_base_component_var_register(c, param_name, param_desc,
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &(cs->mca_sub_components[coll][topo_lvl]));
        }
    }

    /* Dynamic rules */
    cs->use_dynamic_file_rules = false;
    (void) mca_base_component_var_register(&mca_coll_han_component.super.collm_version,
                                           "use_dynamic_file_rules",
                                           "Enable the dynamic selection provided via the dynamic_rules_filename MCA",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &(cs->use_dynamic_file_rules));

    cs->dynamic_rules_filename = NULL;
    (void) mca_base_component_var_register(&mca_coll_han_component.super.collm_version,
                                           "dynamic_rules_filename",
                                           "Configuration file containing the dynamic selection rules",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &(cs->dynamic_rules_filename));

    cs->dump_dynamic_rules = false;
    (void) mca_base_component_var_register(&mca_coll_han_component.super.collm_version,
                                           "dump_dynamic_rules",
                                           "Switch used to decide if we dump  dynamic rules provided by configuration file",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &(cs->dump_dynamic_rules));

    if((cs->dump_dynamic_rules || NULL != cs->dynamic_rules_filename)
       && !cs->use_dynamic_file_rules) {
        opal_output_verbose(0, cs->han_output,
                            "HAN: dynamic rules for collectives are hot activated."
                            "Check coll_han_use_dynamic_file_rules MCA parameter");
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
