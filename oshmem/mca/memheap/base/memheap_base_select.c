/*
 * Copyright (c) 2013-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "oshmem/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "oshmem/info/info.h"
#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "oshmem/include/shmemx.h"
#include "oshmem/mca/sshmem/base/base.h"


mca_memheap_base_config_t mca_memheap_base_config = {
    .device_nic_mem_seg_size = 0
};

mca_memheap_base_module_t mca_memheap = {0};

/**
 * Function for weeding out memheap components that shouldn't be executed.
 * Implementation inspired by btl/base.
 *
 * Call the init function on all available components to find out if
 * they want to run. Select all components that don't fail.  Failing
 * components will be closed and unloaded.  The selected modules will
 * be pointed to by mca_memheap_base_module_t.
 */

static memheap_context_t* _memheap_create(void);

/**
 * Choose to init one component with the highest priority.
 * If the include list if it is not empty choose a component that appear in the list.
 * O/W choose the highest priority component not in the exclude list.
 * Include and exclude lists may be given in the shmem launcher command line.
 */
int mca_memheap_base_select()
{
    int best_priority;
    memheap_context_t *context;
    mca_memheap_base_component_t *best_component = NULL;
    mca_memheap_base_module_t *best_module = NULL;

    if( OPAL_SUCCESS != mca_base_select("memheap", oshmem_memheap_base_framework.framework_output,
                                        &oshmem_memheap_base_framework.framework_components,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component,
                                        &best_priority) ) {
        return OSHMEM_ERROR;
    }

    context = _memheap_create();
    if (NULL == context) {
        return OSHMEM_ERROR;
    }

    if (OSHMEM_SUCCESS != best_component->memheap_init(context)) {
        opal_show_help("help-oshmem-memheap.txt",
                       "find-available:none-found",
                       true,
                       "memheap");
        return OSHMEM_ERROR;
    }

    /* Calculate memheap size in case it was not set during component initialization */
    best_module->memheap_size = context->user_size;
    setenv(SHMEM_HEAP_TYPE,
           best_component->memheap_version.mca_component_name, 1);

    mca_memheap = *best_module;

    MEMHEAP_VERBOSE(10,
                    "SELECTED %s component %s",
                    best_component->memheap_version.mca_type_name, 
                    best_component->memheap_version.mca_component_name);

    return OSHMEM_SUCCESS;
}

static size_t _memheap_size(void)
{
    return (size_t) memheap_align(oshmem_shmem_info_env.symmetric_heap_size);
}

static memheap_context_t* _memheap_create(void)
{
    int rc = OSHMEM_SUCCESS;
    static memheap_context_t context;
    size_t user_size, size;

    user_size = _memheap_size();
    if (user_size < MEMHEAP_BASE_MIN_SIZE) {
        MEMHEAP_ERROR("Requested memheap size is less than minimal meamheap size (%llu < %llu)",
                      (unsigned long long)user_size, MEMHEAP_BASE_MIN_SIZE);
        return NULL ;
    }
    /* Inititialize symmetric area */
    if (OSHMEM_SUCCESS == rc) {
        rc = mca_memheap_base_alloc_init(&mca_memheap_base_map,
                                         user_size + MEMHEAP_BASE_PRIVATE_SIZE, 0);
    }

    /* Initialize atomic symmetric area */
    size = mca_memheap_base_config.device_nic_mem_seg_size;
    if ((OSHMEM_SUCCESS == rc) && (size > 0)) {
        rc = mca_memheap_base_alloc_init(&mca_memheap_base_map, size,
                                         SHMEM_HINT_DEVICE_NIC_MEM);
        if (rc == OSHMEM_ERR_NOT_IMPLEMENTED) {
            /* do not treat NOT_IMPLEMENTED as error */
            rc = OSHMEM_SUCCESS;
        }
    }

    /* Inititialize static/global variables area */
    if (OSHMEM_SUCCESS == rc) {
        rc = mca_memheap_base_static_init(&mca_memheap_base_map);
    }

    /* Memory Registration */
    if (OSHMEM_SUCCESS == rc) {
        rc = mca_memheap_base_reg(&mca_memheap_base_map);
    }

    /* Init OOB channel */
    if (OSHMEM_SUCCESS == rc) {
        rc = memheap_oob_init(&mca_memheap_base_map);
    }

    if (OSHMEM_SUCCESS == rc) {
        context.user_size = user_size;
        context.private_size = MEMHEAP_BASE_PRIVATE_SIZE;
        context.user_base_addr =
                (void*) ((unsigned char*) mca_memheap_base_map.mem_segs[HEAP_SEG_INDEX].super.va_base
                        + 0);
        context.private_base_addr =
                (void*) ((unsigned char*) mca_memheap_base_map.mem_segs[HEAP_SEG_INDEX].super.va_base
                        + context.user_size);
    }

    return ((OSHMEM_SUCCESS == rc) ? &context : NULL );
}
