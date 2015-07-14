/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "ompi_config.h"
#include "fs_lustre.h"
#include "mpi.h"

/*
 * Public string showing the fs lustre component version number
 */
const char *mca_fs_lustre_component_version_string =
  "OMPI/MPI lustre FS MCA component version " OMPI_VERSION;

static int lustre_register(void);

int mca_fs_lustre_priority = 20;
 /*setting default stripe size
   to 64KB. MCA parameter
   Can be changed at
   runtime also*/
int mca_fs_lustre_stripe_size = 0;
int mca_fs_lustre_stripe_width = 0;
/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_fs_base_component_2_0_0_t mca_fs_lustre_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    .fsm_version = {
        MCA_FS_BASE_VERSION_2_0_0,

        /* Component name and version */
        .mca_component_name = "lustre",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),
        .mca_register_component_params = lustre_register,
    },
    .fsm_data = {
        /* This component is checkpointable */
      MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .fsm_init_query = mca_fs_lustre_component_init_query,      /* get thread level */
    .fsm_file_query = mca_fs_lustre_component_file_query,      /* get priority and actions */
    .fsm_file_unquery = mca_fs_lustre_component_file_unquery,  /* undo what was done by previous function */
};

static int
lustre_register(void)
{
    mca_fs_lustre_priority = 20;
    (void) mca_base_component_var_register(&mca_fs_lustre_component.fsm_version,
                                           "priority", "Priority of the lustre fs component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_fs_lustre_priority);
    mca_fs_lustre_stripe_size = 0;
    (void) mca_base_component_var_register(&mca_fs_lustre_component.fsm_version,
                                           "stripe_size", "stripe size of a file over lustre",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_fs_lustre_stripe_size);
    mca_fs_lustre_stripe_width = 0;
    (void) mca_base_component_var_register(&mca_fs_lustre_component.fsm_version,
                                           "stripe_width", "stripe count of a file over lustre",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_fs_lustre_stripe_width);

    return OMPI_SUCCESS;
}
