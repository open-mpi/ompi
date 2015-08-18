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
 * Copyright (c) 2013      University of Houston. All rights reserved.
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
#include "sharedfp_addproc.h"
#include "mpi.h"

/*
 * Public string showing the sharedfp addproc component version number
 */
const char *mca_sharedfp_addproc_component_version_string =
  "OMPI/MPI addproc SHAREDFP MCA component version " OMPI_VERSION;

/*
 * Global variables
 */
int mca_sharedfp_addproc_priority=1;
int mca_sharedfp_addproc_verbose=0;
#if 0
char[MPI_MAX_HOSTNAME_LEN] mca_sharedfp_addproc_control_host;
#endif

static int addproc_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_sharedfp_base_component_2_0_0_t mca_sharedfp_addproc_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    .sharedfpm_version = {
        MCA_SHAREDFP_BASE_VERSION_2_0_0,

        /* Component name and version */
        .mca_component_name = "addproc",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),
        .mca_register_component_params = addproc_register,
    },
    .sharedfpm_data = {
        /* This component is checkpointable */
      MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .sharedfpm_init_query = mca_sharedfp_addproc_component_init_query,      /* get thread level */
    .sharedfpm_file_query = mca_sharedfp_addproc_component_file_query,      /* get priority and actions */
    .sharedfpm_file_unquery = mca_sharedfp_addproc_component_file_unquery,  /* undo what was done by previous function */
};


static int addproc_register(void)
{
    mca_sharedfp_addproc_priority = 1;
    (void) mca_base_component_var_register(&mca_sharedfp_addproc_component.sharedfpm_version,
                                           "priority", "Priority of the addproc sharedfp component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_sharedfp_addproc_priority);
    mca_sharedfp_addproc_verbose = 0;
    (void) mca_base_component_var_register(&mca_sharedfp_addproc_component.sharedfpm_version,
                                           "verbose", "Verbosity of the addproc sharedfp component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_sharedfp_addproc_verbose);


#if 0
    memset (mca_sharedfp_addproc_control_host, 0, MPI_MAX_HOSTNAME_LEN);
    (void) mca_base_component_var_register(&mca_sharedfp_addproc_component.sharedfpm_version,
                                           "control_host", "Name of the host where to spawn the control process(default:none)",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_sharedfp_addproc_control_host);

#endif
    return OMPI_SUCCESS;
}
