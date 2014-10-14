/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/constants.h"

#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"

#include "sshmem_mmap.h"

/**
 * public string showing the shmem ompi_mmap component version number
 */
const char *mca_sshmem_mmap_component_version_string =
    "OSHMEM mmap sshmem MCA component version " OSHMEM_VERSION;

int mca_sshmem_mmap_relocate_backing_file = 0;
char *mca_sshmem_mmap_backing_file_base_dir = NULL;
bool mca_sshmem_mmap_nfs_warning = true;

/**
 * local functions
 */
static int mmap_register(void);
static int mmap_open(void);
static int mmap_close(void);
static int mmap_query(mca_base_module_t **module, int *priority);
static int mmap_runtime_query(mca_base_module_t **module,
                             int *priority,
                             const char *hint);

/**
 * instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_sshmem_mmap_component_t mca_sshmem_mmap_component = {
    /* ////////////////////////////////////////////////////////////////////// */
    /* super */
    /* ////////////////////////////////////////////////////////////////////// */
    {
        /**
         * common MCA component data
         */
        {
            MCA_SSHMEM_BASE_VERSION_2_0_0,

            /* component name and version */
            "mmap",
            OSHMEM_MAJOR_VERSION,
            OSHMEM_MINOR_VERSION,
            OSHMEM_RELEASE_VERSION,

            /* component open */
            mmap_open,
            /* component close */
            mmap_close,
            /* component query */
            mmap_query,
            /* component register */
            mmap_register
        },
        /* MCA v2.0.0 component meta data */
        {
            /* the component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        mmap_runtime_query,
    },
};

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_runtime_query(mca_base_module_t **module,
                   int *priority,
                   const char *hint)
{
    /* no run-time query needed for mmap, so this is easy */
    *priority = mca_sshmem_mmap_component.priority;
    *module = (mca_base_module_t *)&mca_sshmem_mmap_module.super;
    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_register(void)
{
    /* ////////////////////////////////////////////////////////////////////// */
    /* (default) priority - set high to make mmap the default */
    mca_sshmem_mmap_component.priority = 20;
    mca_base_component_var_register (&mca_sshmem_mmap_component.super.base_version,
                                     "priority", "Priority for sshmem mmap "
                                     "component (default: 20)", MCA_BASE_VAR_TYPE_INT,
                                     NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                     OPAL_INFO_LVL_3,
                                     MCA_BASE_VAR_SCOPE_ALL_EQ,
                                     &mca_sshmem_mmap_component.priority);

    mca_sshmem_mmap_component.is_anonymous = 1;
    mca_base_component_var_register (&mca_sshmem_mmap_component.super.base_version,
                                    "anonymous", "Select whether anonymous sshmem is used for mmap "
                                    "component (default: 1)", MCA_BASE_VAR_TYPE_INT,
                                    NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_4,
                                    MCA_BASE_VAR_SCOPE_ALL_EQ,
                                    &mca_sshmem_mmap_component.is_anonymous);

   mca_sshmem_mmap_component.is_start_addr_fixed = 1;
   mca_base_component_var_register (&mca_sshmem_mmap_component.super.base_version,
                                    "fixed", "Select whether fixed start address is used for shmem "
                                    "(default: 1)", MCA_BASE_VAR_TYPE_INT,
                                    NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_4,
                                    MCA_BASE_VAR_SCOPE_ALL_EQ,
                                    &mca_sshmem_mmap_component.is_start_addr_fixed);
    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_open(void)
{
    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_query(mca_base_module_t **module, int *priority)
{
    *priority = mca_sshmem_mmap_component.priority;
    *module = (mca_base_module_t *)&mca_sshmem_mmap_module.super;
    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_close(void)
{
    return OSHMEM_SUCCESS;
}

