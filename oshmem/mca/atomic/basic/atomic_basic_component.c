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
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"
#include "atomic_basic.h"


/*
 * Public string showing the scoll basic component version number
 */
const char *mca_atomic_basic_component_version_string =
  "Open SHMEM basic atomic MCA component version " OSHMEM_VERSION;

/*
 * Global variable
 */
int mca_atomic_basic_priority_param = -1;

/*
 * Local function
 */
static int __basic_open(void);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_atomic_base_component_t mca_atomic_basic_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        MCA_ATOMIC_BASE_VERSION_2_0_0,

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

    mca_atomic_basic_init,
    mca_atomic_basic_finalize,
    mca_atomic_basic_query
};


static int __basic_open(void)
{
    /* We'll always be picked if there's only one process in the
       communicator */
    int default_value = 75;
    int param_value = default_value;
    
    mca_atomic_basic_priority_param = 
        mca_base_param_reg_int(&mca_atomic_basic_component.atomic_version, "priority", NULL, false, false, default_value, &param_value);
	
    return OSHMEM_SUCCESS;
}


OBJ_CLASS_INSTANCE(mca_atomic_basic_module_t,
                   mca_atomic_base_module_t,
                   NULL, NULL);
