/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      DataDirect Networks. All rights reserved.
 * Copyright (c) 2021      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "fs_ime.h"
#include "mpi.h"

int mca_fs_ime_priority = FS_IME_BASE_PRIORITY;
int mca_fs_ime_lock_algorithm = FS_IME_LOCK_AUTO;

/*
 * Private functions
 */
static int register_component(void);


/*
 * Public string showing the fs ime component version number
 */
const char *mca_fs_ime_component_version_string =
  "OMPI/MPI IME FS MCA component version " OMPI_VERSION;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_fs_base_component_2_0_0_t mca_fs_ime_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    .fsm_version = {
        MCA_FS_BASE_VERSION_2_0_0,

        /* Component name and version */
        .mca_component_name = "ime",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),
        .mca_register_component_params = register_component,
        .mca_close_component = mca_fs_ime_native_fini,
    },
    .fsm_data = {
        /* This component is checkpointable */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .fsm_init_query = mca_fs_ime_component_init_query,      /* get thread level */
    .fsm_file_query = mca_fs_ime_component_file_query,      /* get priority and actions */
    .fsm_file_unquery = mca_fs_ime_component_file_unquery,  /* undo what was done by previous function */
};

static int register_component(void)
{
    mca_fs_ime_priority = FS_IME_BASE_PRIORITY;
    (void) mca_base_component_var_register(&mca_fs_ime_component.fsm_version,
                                           "priority", "Priority of the fs ime component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_fs_ime_priority);

    mca_fs_ime_lock_algorithm = FS_IME_LOCK_AUTO;
    (void) mca_base_component_var_register(&mca_fs_ime_component.fsm_version,
                                           "lock_algorithm", "Locking algorithm used by the fs ime component. "
                                           " 0: auto (default)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_fs_ime_lock_algorithm );

    return OMPI_SUCCESS;
}
