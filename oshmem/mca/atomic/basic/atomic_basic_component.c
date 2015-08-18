/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
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

/*
 * Local function
 */
static int _basic_register(void);
static int _basic_open(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_atomic_base_component_t mca_atomic_basic_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    .atomic_version = {
        MCA_ATOMIC_BASE_VERSION_2_0_0,

        /* Component name and version */
        .mca_component_name = "basic",
        MCA_BASE_MAKE_VERSION(component, OSHMEM_MAJOR_VERSION, OSHMEM_MINOR_VERSION,
                              OSHMEM_RELEASE_VERSION),

        .mca_open_component = _basic_open,
        .mca_register_component_params = _basic_register,
    },
    .atomic_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Initialization / querying functions */

    .atomic_init = mca_atomic_basic_init,
    .atomic_finalize = mca_atomic_basic_finalize,
    .atomic_query = mca_atomic_basic_query,
};

static int _basic_register(void)
{
    mca_atomic_basic_component.priority = 75;
    mca_base_component_var_register (&mca_atomic_basic_component.atomic_version,
                                     "priority", "Priority of the atomic:basic "
                                     "component (default: 75)", MCA_BASE_VAR_TYPE_INT,
                                     NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                     OPAL_INFO_LVL_3,
                                     MCA_BASE_VAR_SCOPE_ALL_EQ,
                                     &mca_atomic_basic_component.priority);

    return OSHMEM_SUCCESS;
}

static int _basic_open(void)
{
    return OSHMEM_SUCCESS;
}

OBJ_CLASS_INSTANCE(mca_atomic_basic_module_t,
                   mca_atomic_base_module_t,
                   NULL,
                   NULL);
