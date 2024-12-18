/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014-2015 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "mpi.h"
#include "ompi/constants.h"
#include "coll_accelerator.h"
#include "opal/util/argv.h"

/*
 * Public string showing the coll ompi_accelerator component version number
 */
const char *mca_coll_accelerator_component_version_string =
    "Open MPI accelerator collective MCA component version " OMPI_VERSION;

/*
 * Local function
 */
static int accelerator_open(void);
static int accelerator_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_coll_accelerator_component_t mca_coll_accelerator_component = {
    {
        /* First, the mca_component_t struct containing meta information
         * about the component itself */

        .collm_version = {
            MCA_COLL_BASE_VERSION_3_0_0,

            /* Component name and version */
            .mca_component_name = "accelerator",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = accelerator_open,
            .mca_register_component_params = accelerator_register,
        },
        .collm_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Initialization / querying functions */

        .collm_init_query = mca_coll_accelerator_init_query,
        .collm_comm_query = mca_coll_accelerator_comm_query,
    },

    /* accelerator-specific component information */

    /* Priority: make it above all point to point collectives including self */
    .priority = 78,
};


static int accelerator_register(void)
{
    (void) mca_base_component_var_register(&mca_coll_accelerator_component.super.collm_version,
                                           "priority", "Priority of the accelerator coll component; only relevant if barrier_before "
                                           "or barrier_after is > 0",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_accelerator_component.priority);

    (void) mca_base_component_var_register(&mca_coll_accelerator_component.super.collm_version,
                                           "disable_accelerator_coll", "Automatically handle the accelerator buffers for the MPI collective.",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_2,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_accelerator_component.disable_accelerator_coll);

    mca_coll_accelerator_component.cts = COLL_ACCELERATOR_CTS_STR;
    (void)mca_base_component_var_register(&mca_coll_accelerator_component.super.collm_version,
                                          "cts", "Comma separated list of collectives to be enabled",
                                          MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                          OPAL_INFO_LVL_6, MCA_BASE_VAR_SCOPE_ALL, &mca_coll_accelerator_component.cts);

    return OMPI_SUCCESS;
}


/* The string parsing is based on the code available in the coll/ucc component */
static uint64_t mca_coll_accelerator_str_to_type(const char *str)
{
    if (0 == strcasecmp(str, "allreduce")) {
        return COLL_ACC_ALLREDUCE;
    } else if (0 == strcasecmp(str, "reduce_scatter_block")) {
        return COLL_ACC_REDUCE_SCATTER_BLOCK;
    } else if (0 == strcasecmp(str, "reduce_local")) {
        return COLL_ACC_REDUCE_LOCAL;
    } else if (0 == strcasecmp(str, "reduce")) {
        return COLL_ACC_REDUCE;
    } else if (0 == strcasecmp(str, "exscan")) {
        return COLL_ACC_EXSCAN;
    } else if (0 == strcasecmp(str, "scan")) {
        return COLL_ACC_SCAN;
    }
    opal_output(0, "incorrect value for cts: %s, allowed: %s",
              str, COLL_ACCELERATOR_CTS_STR);
    return COLL_ACC_LASTCOLL;
}

static void accelerator_init_default_cts(void)
{
    mca_coll_accelerator_component_t *cm = &mca_coll_accelerator_component;
    bool disable;
    char** cts;
    int n_cts, i;
    char* str;
    uint64_t *ct, c;

    disable              = (cm->cts[0] == '^') ? true : false;
    cts                  = opal_argv_split(disable ? (cm->cts + 1) : cm->cts, ',');
    n_cts                = opal_argv_count(cts);
    cm->cts_requested    = disable ? COLL_ACCELERATOR_CTS : 0;
    for (i = 0; i < n_cts; i++) {
        if (('i' == cts[i][0]) || ('I' == cts[i][0])) {
            /* non blocking collective setting */
            opal_output(0, "coll/accelerator component does not support non-blocking collectives at this time."
                        " Ignoring collective: %s\n", cts[i]);
            continue;
        } else {
            str = cts[i];
            ct  = &cm->cts_requested;
        }
        c = mca_coll_accelerator_str_to_type(str);
        if (COLL_ACC_LASTCOLL == c) {
            *ct = COLL_ACCELERATOR_CTS;
            break;
        }
        if (disable) {
            (*ct) &= ~c;
        } else {
            (*ct) |= c;
        }
    }
    opal_argv_free(cts);
}

static int accelerator_open(void)
{
    accelerator_init_default_cts();
    return OMPI_SUCCESS;
}
