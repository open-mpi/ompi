/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
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

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/shmem/shmem.h"
#include "shmem_mmap.h"

/**
 * public string showing the shmem ompi_mmap component version number
 */
const char *opal_shmem_mmap_component_version_string =
    "OPAL mmap shmem MCA component version " OPAL_VERSION;

/**
 * local functions
 */
static int mmap_open(void);
static int mmap_query(mca_base_module_t **module, int *priority);
static int mmap_runtime_query(mca_base_module_t **module,
                              int *priority,
                              const char *hint);

/**
 * instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_shmem_mmap_component_t mca_shmem_mmap_component = {
    /* ////////////////////////////////////////////////////////////////////// */
    /* super */
    /* ////////////////////////////////////////////////////////////////////// */
    {
        /**
         * common MCA component data
         */
        {
            OPAL_SHMEM_BASE_VERSION_2_0_0,

            /* component name and version */
            "mmap",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,

            /* component open */
            mmap_open,
            /* component close */
            NULL,
            /* component query */
            mmap_query
        },
        /* MCA v2.0.0 component meta data */
        {
            /* the component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        mmap_runtime_query,
    },
    /* ////////////////////////////////////////////////////////////////////// */
    /* mmap component-specific information */
    /* see: shmem_mmap.h for more information */
    /* ////////////////////////////////////////////////////////////////////// */
    /* (default) priority - set high to make mmap the default */
    50
};

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_runtime_query(mca_base_module_t **module,
                   int *priority,
                   const char *hint)
{
    /* no run-time query needed for mmap, so this is easy */
    *priority = mca_shmem_mmap_component.priority;
    *module = (mca_base_module_t *)&opal_shmem_mmap_module.super;
    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_open(void)
{
    mca_base_param_reg_int(
        &mca_shmem_mmap_component.super.base_version,
        "priority", "Priority of the mmap shmem component", false, false,
        mca_shmem_mmap_component.priority, &mca_shmem_mmap_component.priority
    );

    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_query(mca_base_module_t **module, int *priority)
{
    *priority = mca_shmem_mmap_component.priority;
    *module = (mca_base_module_t *)&opal_shmem_mmap_module.super;
    return OPAL_SUCCESS;
}

