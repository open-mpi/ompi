/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_var.h"

#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "oshmem/mca/sshmem/base/static-components.h"

/**
 * globals
 */

/**
 * if 32 bit we set sshmem_base_start_adress to 0
 * to let OS allocate segment automatically
 */
#if UINTPTR_MAX == 0xFFFFFFFF
void *mca_sshmem_base_start_address = (void*)0;
#else
void* mca_sshmem_base_start_address = (void*)0xFF000000;
#endif

char * mca_sshmem_base_backing_file_dir = NULL;

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * Register some sshmem-wide MCA params
 */
static int
mca_sshmem_base_register (mca_base_register_flag_t flags)
{
    int index;

    index = mca_base_var_register("oshmem",
                                 "sshmem",
                                 "base",
                                 "start_address",
                                 "Specify base address for shared memory region",
                                 MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG,
                                 NULL,
                                 0,
                                 MCA_BASE_VAR_FLAG_SETTABLE,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_sshmem_base_start_address);
    (void) mca_base_var_register_synonym(index, "oshmem", "memheap", "base",
                                         "start_address",
                                         MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    mca_sshmem_base_backing_file_dir = "/dev/shm";
    index = mca_base_var_register("oshmem",
                                 "sshmem",
                                 "base",
                                 "backing_file_dir",
                                 "Specifies where backing files will be created when "
                                 "mmap is used and shmem_mmap_anonymous set to 0.",
                                 MCA_BASE_VAR_TYPE_STRING,
                                 NULL,
                                 0,
                                 MCA_BASE_VAR_FLAG_SETTABLE,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_sshmem_base_backing_file_dir);
    return OSHMEM_SUCCESS;
}

static int mca_sshmem_base_open(mca_base_open_flag_t flags)
{
    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_framework_components_open(&oshmem_sshmem_base_framework, flags)) {
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}

/* Use the default open function */
MCA_BASE_FRAMEWORK_DECLARE(oshmem, sshmem,
                           "OSHMEM SSHMEM",
                           mca_sshmem_base_register,
                           mca_sshmem_base_open,
                           mca_sshmem_base_close,
                           mca_sshmem_base_static_components,
                           MCA_BASE_FRAMEWORK_FLAG_DEFAULT);
