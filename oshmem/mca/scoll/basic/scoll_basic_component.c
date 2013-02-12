/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
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
int mca_scoll_basic_param_collect_algorithm = SCOLL_ALG_COLLECT_RECURSIVE_DOUBLING;
int mca_scoll_basic_param_reduce_algorithm = SCOLL_ALG_REDUCE_RECURSIVE_DOUBLING;

/*
 * Local function
 */
static int __basic_open(void);


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
        __basic_open,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Initialization / querying functions */

    mca_scoll_basic_init,
    mca_scoll_basic_query
};


static int __basic_open(void)
{
    char help_msg[200];
    mca_base_component_t *comp = &mca_scoll_basic_component.scoll_version;
    int default_value = 75;
    int param_value = default_value;

    /* We'll always be picked if there's only one process in the
       communicator */
    mca_scoll_basic_priority_param =
        mca_base_param_reg_int(&mca_scoll_basic_component.scoll_version, "priority", NULL, false, false, default_value, &param_value);
    
    sprintf( help_msg, 
             "Algoritm selection for Barrier (%d - Central Counter, %d - Tournament, %d - Recursive Doubling, %d - Dissemination, %d - Basic, %d - Adaptive)",
             SCOLL_ALG_BARRIER_CENTRAL_COUNTER,
             SCOLL_ALG_BARRIER_TOURNAMENT,
             SCOLL_ALG_BARRIER_RECURSIVE_DOUBLING,
             SCOLL_ALG_BARRIER_DISSEMINATION,
             SCOLL_ALG_BARRIER_BASIC,
             SCOLL_ALG_BARRIER_ADAPTIVE);
    mca_base_param_reg_int(comp, "barrier_alg",
			(const char *)help_msg, 
            false, false, 
            mca_scoll_basic_param_barrier_algorithm, &mca_scoll_basic_param_barrier_algorithm);
    
    sprintf( help_msg, 
             "Algoritm selection for Broadcast (%d - Central Counter, %d - Binomial)",
             SCOLL_ALG_BROADCAST_CENTRAL_COUNTER,
             SCOLL_ALG_BROADCAST_BINOMIAL);
    mca_base_param_reg_int(comp, "broadcast_alg",
			(const char *)help_msg, 
            false, false, 
            mca_scoll_basic_param_broadcast_algorithm, &mca_scoll_basic_param_broadcast_algorithm);
    
    sprintf( help_msg, 
             "Algoritm selection for Collect (%d - Central Counter, %d - Tournament, %d - Recursive Doubling, %d - Ring)",
             SCOLL_ALG_COLLECT_CENTRAL_COUNTER,
             SCOLL_ALG_COLLECT_TOURNAMENT,
             SCOLL_ALG_COLLECT_RECURSIVE_DOUBLING,
             SCOLL_ALG_COLLECT_RING);
    mca_base_param_reg_int(comp, "collect_alg",
			(const char *)help_msg, 
            false, false, 
            mca_scoll_basic_param_collect_algorithm, &mca_scoll_basic_param_collect_algorithm);
    
    sprintf( help_msg, 
             "Algoritm selection for Reduce (%d - Central Counter, %d - Tournament, %d - Recursive Doubling %d - Linear %d - Log)",
             SCOLL_ALG_REDUCE_CENTRAL_COUNTER,
             SCOLL_ALG_REDUCE_TOURNAMENT,
             SCOLL_ALG_REDUCE_RECURSIVE_DOUBLING,
             SCOLL_ALG_REDUCE_LEGACY_LINEAR,
             SCOLL_ALG_REDUCE_LEGACY_LOG);
    mca_base_param_reg_int(comp, "reduce_alg",
			(const char *)help_msg, 
            false, false, 
            mca_scoll_basic_param_reduce_algorithm, &mca_scoll_basic_param_reduce_algorithm);
	
    return OSHMEM_SUCCESS;
}


OBJ_CLASS_INSTANCE(mca_scoll_basic_module_t,
                   mca_scoll_base_module_t,
                   NULL, NULL);
