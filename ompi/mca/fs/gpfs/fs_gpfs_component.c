/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"
#include "fs_gpfs.h"

/*
 * Public string showing the fs gpfs component version number
 */
const char *mca_fs_gpfs_component_version_string =
  "OMPI/MPI gpfs FS MCA component version " OMPI_VERSION;

static int gpfs_register(void);

int mca_fs_gpfs_priority = 20;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_fs_base_component_2_0_0_t mca_fs_gpfs_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        MCA_FS_BASE_VERSION_2_0_0,

        /* Component name and version */
        "gpfs",
        OMPI_MAJOR_VERSION,
        OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION,
        NULL,
        NULL,
        NULL,
        gpfs_register
    },
    {
        /* This component is checkpointable */
      MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    mca_fs_gpfs_component_init_query,      /* get thread level */
    mca_fs_gpfs_component_file_query,      /* get priority and actions */
    mca_fs_gpfs_component_file_unquery     /* undo what was done by previous function */
};

static int
gpfs_register(void)
{
    mca_fs_gpfs_priority = 20;
    (void) mca_base_component_var_register(&mca_fs_gpfs_component.fsm_version,
                                           "priority", "Priority of the gpfs fs component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_fs_gpfs_priority);
    return OMPI_SUCCESS;
}