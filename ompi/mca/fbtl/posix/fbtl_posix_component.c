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
 * Copyright (c) 2008-2020 University of Houston. All rights reserved.
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
#include "fbtl_posix.h"
#include "mpi.h"

/*
 * Public string showing the fbtl posix component version number
 */
const char *mca_fbtl_posix_component_version_string =
  "OMPI/MPI posix FBTL MCA component version " OMPI_VERSION;

int mca_fbtl_posix_priority = 10;
bool mca_fbtl_posix_read_datasieving  = true;
bool mca_fbtl_posix_write_datasieving = true;
size_t mca_fbtl_posix_max_block_size  = 1048576;  // 1MB
size_t mca_fbtl_posix_max_gap_size    = 4096;     // Size of a block in many linux fs
size_t mca_fbtl_posix_max_tmpbuf_size = 67108864; // 64 MB
/*
 * Private functions
 */
static int register_component(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_fbtl_base_component_2_0_0_t mca_fbtl_posix_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    .fbtlm_version = {
        MCA_FBTL_BASE_VERSION_2_0_0,

        /* Component name and version */
        .mca_component_name = "posix",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),
        .mca_register_component_params = register_component,
    },
    .fbtlm_data = {
        /* This component is checkpointable */
      MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .fbtlm_init_query = mca_fbtl_posix_component_init_query,      /* get thread level */
    .fbtlm_file_query = mca_fbtl_posix_component_file_query,      /* get priority and actions */
    .fbtlm_file_unquery = mca_fbtl_posix_component_file_unquery,  /* undo what was done by previous function */
};

static int register_component(void)
{
    mca_fbtl_posix_priority = 10;
    (void) mca_base_component_var_register(&mca_fbtl_posix_component.fbtlm_version,
                                           "priority", "Priority of the fbtl posix component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_fbtl_posix_priority);

    mca_fbtl_posix_max_block_size = 1048576;
    (void) mca_base_component_var_register(&mca_fbtl_posix_component.fbtlm_version,
                                           "max_block_size", "Maximum average size in bytes of a data block in an iovec for data sieving. "
                                           "An average block size larger than this parameter will disable data sieving. Default: 1048576 bytes.",
                                           MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_fbtl_posix_max_block_size );

    mca_fbtl_posix_max_gap_size = 4096;
    (void) mca_base_component_var_register(&mca_fbtl_posix_component.fbtlm_version,
                                           "max_gap_size", "Maximum average gap size between two blocks in an iovec for data sieving. "
                                           "An average gap size larger than this parameter will disable data sieving. Default: 4096 bytes. " ,
                                           MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_fbtl_posix_max_gap_size );

    mca_fbtl_posix_max_tmpbuf_size = 67108864;
    (void) mca_base_component_var_register(&mca_fbtl_posix_component.fbtlm_version,
                                           "max_tmpbuf_size", "Maximum size of the temporary buffer used for data sieving in bytes. "
                                           "Default: 67108864 (64MB). " ,
                                           MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_fbtl_posix_max_tmpbuf_size );
    
    mca_fbtl_posix_read_datasieving  = true;
    (void) mca_base_component_var_register(&mca_fbtl_posix_component.fbtlm_version,
                                           "read_datasieving", "Parameter indicating whether to perform data sieving for read operations. "
                                           "Default: true.",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_fbtl_posix_read_datasieving );

    mca_fbtl_posix_write_datasieving  = true;
    (void) mca_base_component_var_register(&mca_fbtl_posix_component.fbtlm_version,
                                           "write_datasieving", "Parameter indicating whether to perform data sieving for write operations. "
                                           "Default: true.",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_fbtl_posix_write_datasieving );

    
    return OMPI_SUCCESS;
}
