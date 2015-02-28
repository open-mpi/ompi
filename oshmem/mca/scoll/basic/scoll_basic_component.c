/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

#include "oshmem/constants.h"
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"
#include "scoll_basic.h"

/*
 * Public string showing the scoll basic component version number
 */
const char *mca_scoll_basic_component_version_string =
"Open SHMEM basic collective MCA component version " OSHMEM_VERSION;

/*
 * Global variable
 */
int mca_scoll_basic_priority_param = -1;
int mca_scoll_basic_param_barrier_algorithm = SCOLL_ALG_BARRIER_ADAPTIVE;
int mca_scoll_basic_param_broadcast_algorithm = SCOLL_ALG_BROADCAST_BINOMIAL;
int mca_scoll_basic_param_collect_algorithm =
        SCOLL_ALG_COLLECT_RECURSIVE_DOUBLING;
int mca_scoll_basic_param_reduce_algorithm = SCOLL_ALG_REDUCE_RECURSIVE_DOUBLING;

/*
 * Local function
 */
static int basic_register(void);
static int basic_open(void);
static int basic_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_scoll_base_component_t mca_scoll_basic_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        MCA_SCOLL_BASE_VERSION_2_0_0,

        /* Component name and version */
        "basic",
        OSHMEM_MAJOR_VERSION,
        OSHMEM_MINOR_VERSION,
        OSHMEM_RELEASE_VERSION,

        /* Component open and close functions */
        basic_open,
        basic_close,
        NULL,
        basic_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Initialization / querying functions */

    mca_scoll_basic_init,
    mca_scoll_basic_query
};

static int basic_register(void)
{
    char help_msg[200];
    mca_base_component_t *comp = &mca_scoll_basic_component.scoll_version;

    mca_scoll_basic_priority_param = 75;
    (void) mca_base_component_var_register(comp,
                                           "priority",
                                           "Priority of the basic scoll:basic component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_basic_priority_param);

    sprintf(help_msg,
            "Algorithm selection for Barrier (%d - Central Counter, %d - Tournament, %d - Recursive Doubling, %d - Dissemination, %d - Basic, %d - Adaptive)",
            SCOLL_ALG_BARRIER_CENTRAL_COUNTER,
            SCOLL_ALG_BARRIER_TOURNAMENT,
            SCOLL_ALG_BARRIER_RECURSIVE_DOUBLING,
            SCOLL_ALG_BARRIER_DISSEMINATION,
            SCOLL_ALG_BARRIER_BASIC,
            SCOLL_ALG_BARRIER_ADAPTIVE);
    (void) mca_base_component_var_register(comp,
                                           "barrier_alg",
                                           help_msg,
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_basic_param_barrier_algorithm);

    sprintf(help_msg,
            "Algorithm selection for Broadcast (%d - Central Counter, %d - Binomial)",
            SCOLL_ALG_BROADCAST_CENTRAL_COUNTER,
            SCOLL_ALG_BROADCAST_BINOMIAL);
    (void) mca_base_component_var_register(comp,
                                           "broadcast_alg",
                                           help_msg,
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_basic_param_broadcast_algorithm);

    sprintf(help_msg,
            "Algorithm selection for Collect (%d - Central Counter, %d - Tournament, %d - Recursive Doubling, %d - Ring)",
            SCOLL_ALG_COLLECT_CENTRAL_COUNTER,
            SCOLL_ALG_COLLECT_TOURNAMENT,
            SCOLL_ALG_COLLECT_RECURSIVE_DOUBLING,
            SCOLL_ALG_COLLECT_RING);
    (void) mca_base_component_var_register(comp,
                                           "collect_alg",
                                           help_msg,
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_basic_param_collect_algorithm);

    sprintf(help_msg,
            "Algorithm selection for Reduce (%d - Central Counter, %d - Tournament, %d - Recursive Doubling %d - Linear %d - Log)",
            SCOLL_ALG_REDUCE_CENTRAL_COUNTER,
            SCOLL_ALG_REDUCE_TOURNAMENT,
            SCOLL_ALG_REDUCE_RECURSIVE_DOUBLING,
            SCOLL_ALG_REDUCE_LEGACY_LINEAR,
            SCOLL_ALG_REDUCE_LEGACY_LOG);
    (void) mca_base_component_var_register(comp,
                                           "reduce_alg",
                                           help_msg,
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_basic_param_reduce_algorithm);

    return OSHMEM_SUCCESS;
}

static int basic_open(void)
{
    return OSHMEM_SUCCESS;
}

static int basic_close(void)
{
    return OSHMEM_SUCCESS;
}

OBJ_CLASS_INSTANCE(mca_scoll_basic_module_t,
                   mca_scoll_base_module_t,
                   NULL,
                   NULL);
