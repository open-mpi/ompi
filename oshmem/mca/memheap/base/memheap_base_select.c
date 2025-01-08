/*
 * Copyright (c) 2013-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#include "oshmem/include/shmemx.h"
#include "oshmem/mca/sshmem/base/base.h"
#include "ompi/util/timings.h"

#include <sys/mman.h>

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

    OPAL_TIMING_ENV_INIT(timing);

    if( OPAL_SUCCESS != mca_base_select("memheap", oshmem_memheap_base_framework.framework_output,
                                        &oshmem_memheap_base_framework.framework_components,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component,
                                        &best_priority) ) {
        return OSHMEM_ERROR;
    }

    OPAL_TIMING_ENV_NEXT(timing, "env");

    context = _memheap_create();
    if (NULL == context) {
        return OSHMEM_ERROR;
    }

    OPAL_TIMING_ENV_NEXT(timing, "_memheap_create()");

    if (OSHMEM_SUCCESS != best_component->memheap_init(context)) {
        opal_show_help("help-oshmem-memheap.txt",
                       "find-available:none-found",
                       true,
                       "memheap");
        return OSHMEM_ERROR;
    }

    OPAL_TIMING_ENV_NEXT(timing, "best_component->memheap_init()");

    /* Calculate memheap size in case it was not set during component initialization */
    best_module->memheap_size = context->user_size;
    setenv(SHMEM_HEAP_TYPE,
           best_component->memheap_version.mca_component_name, 1);

    mca_memheap = *best_module;

    MEMHEAP_VERBOSE(10,
                    "SELECTED %s component %s",
                    best_component->memheap_version.mca_type_name, 
                    best_component->memheap_version.mca_component_name);

    OPAL_TIMING_ENV_NEXT(timing, "DONE");
    return OSHMEM_SUCCESS;
}

static size_t _memheap_size(void)
{
    return (size_t) memheap_align(oshmem_shmem_info_env.symmetric_heap_size);
}

static void *memheap_mmap_get(void *hint, size_t size)
{
    void *addr;

    addr = mmap(hint, size,
                PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (addr == MAP_FAILED) {
        return NULL;
    }

    return addr;
}

static int memheap_exchange_base_address(size_t size, void **address)
{
    int nprocs = oshmem_num_procs();
    int need_sync = (*address == NULL);
    void *base = NULL;
    void *ptr = NULL;
    int rc, i;
    void **bases;

    bases = calloc(nprocs, sizeof(*bases));
    if (NULL == bases) {
        return OSHMEM_ERROR;
    }

    if (oshmem_my_proc_id() == 0) {
        ptr = memheap_mmap_get(NULL, size);
        base = ptr;
    }

    rc = oshmem_shmem_bcast(&base, sizeof(base), 0);
    if (OSHMEM_SUCCESS != rc) {
        MEMHEAP_ERROR("Failed to exchange allocated vma for base segment "
                      "(error %d)", rc);
        goto out;
    }

    if (oshmem_my_proc_id() != 0) {
        ptr = memheap_mmap_get(base, size);
    }

    MEMHEAP_VERBOSE(100, "#%d: exchange base address: base %p: %s",
                    oshmem_my_proc_id(), base,
                    (base == ptr)? "ok" : "unavailable");

    *address = base;
    if (need_sync) {
        /* They all succeed or fail to allow fallback */
        rc = oshmem_shmem_allgather(&ptr, bases, sizeof(ptr));
        if (OSHMEM_SUCCESS != rc) {
            MEMHEAP_ERROR("Failed to exchange selected vma for base segment "
                          "(error %d)", rc);
            goto out;
        }

        for (i = 0; i < nprocs; i++) {
            if ((NULL == bases[i]) || (bases[i] != base)) {
                *address = NULL;
                break;
            }
        }
    } else if (ptr != base) {
        /* Any failure terminates the rank and others start teardown */
        rc = OSHMEM_ERROR;
    }

out:
    if (((OSHMEM_SUCCESS != rc) || (*address == NULL)) && (ptr != NULL)) {
        (void)munmap(ptr, size);
    }

    free(bases);
    return rc;
}


/*
 * The returned mca_sshmem_base_start_address value is reserved by using
 * mmap() for the expected size.
 */
static int memheap_base_segment_setup(size_t size)
{
    int rc;

    if ((mca_sshmem_base_start_address == (void *)UINTPTR_MAX) ||
        (mca_sshmem_base_start_address == NULL)) {
        if (UINTPTR_MAX == 0xFFFFFFFF) {
            /**
             * if 32 bit we set sshmem_base_start_adress to 0
             * to let OS allocate segment automatically
             */
            mca_sshmem_base_start_address = NULL;
            return OSHMEM_SUCCESS;
        }

        rc = memheap_exchange_base_address(size, &mca_sshmem_base_start_address);
        if (OSHMEM_SUCCESS != rc) {
            MEMHEAP_ERROR("Failed to setup base segment address (error %d)", rc);
            return rc;
        }

        if (NULL != mca_sshmem_base_start_address) {
            goto done; /* Region is reserved */
        }

#if defined(__aarch64__)
        mca_sshmem_base_start_address = (void*)0xAB0000000000;
#else
        mca_sshmem_base_start_address = (void*)0xFF000000;
#endif
    }

    if (mca_sshmem_base_start_address != memheap_mmap_get(
                                      mca_sshmem_base_start_address, size)) {
        MEMHEAP_ERROR("Failed to create segment address %p/%zu",
                      mca_sshmem_base_start_address, size);
        return OSHMEM_ERROR;
    }

done:
    if (oshmem_my_proc_id() == 0) {
        MEMHEAP_VERBOSE(10, "Using symmetric segment address %p/%zu",
                        mca_sshmem_base_start_address, size);
    }

    return OSHMEM_SUCCESS;
}

static memheap_context_t* _memheap_create(void)
{
    int rc = OSHMEM_SUCCESS;
    static memheap_context_t context;
    size_t user_size, size;

    OPAL_TIMING_ENV_INIT(timing);

    user_size = _memheap_size();
    if (user_size < MEMHEAP_BASE_MIN_SIZE) {
        MEMHEAP_ERROR("Requested memheap size is less than minimal meamheap size (%llu < %llu)",
                      (unsigned long long)user_size, MEMHEAP_BASE_MIN_SIZE);
        return NULL ;
    }

    OPAL_TIMING_ENV_NEXT(timing, "_memheap_size()");

    /* Locate and reserve symmetric area */
    rc = memheap_base_segment_setup(user_size + MEMHEAP_BASE_PRIVATE_SIZE);
    if (OSHMEM_SUCCESS != rc) {
        MEMHEAP_ERROR("Failed to negotiate base segment addres");
        return NULL;
    }

    /* Initialize symmetric area */
    rc = mca_memheap_base_alloc_init(&mca_memheap_base_map,
                                     user_size + MEMHEAP_BASE_PRIVATE_SIZE, 0,
                                     "regular_mem");

    OPAL_TIMING_ENV_NEXT(timing, "mca_memheap_base_alloc_init()");

    /* Initialize atomic symmetric area */
    size = mca_memheap_base_config.device_nic_mem_seg_size;
    if ((OSHMEM_SUCCESS == rc) && (size > 0)) {
        rc = mca_memheap_base_alloc_init(&mca_memheap_base_map, size,
                                         SHMEM_HINT_DEVICE_NIC_MEM,
                                         "device_mem");
        if (rc == OSHMEM_ERR_NOT_IMPLEMENTED) {
            /* do not treat NOT_IMPLEMENTED as error */
            rc = OSHMEM_SUCCESS;
        }
    }

    OPAL_TIMING_ENV_NEXT(timing, "mca_memheap_base_alloc_init(DEVICE_MEM)");


    /* Inititialize static/global variables area */
    if (OSHMEM_SUCCESS == rc) {
        rc = mca_memheap_base_static_init(&mca_memheap_base_map);
    }

    OPAL_TIMING_ENV_NEXT(timing, "mca_memheap_base_static_init()");

    /* Memory Registration */
    if (OSHMEM_SUCCESS == rc) {
        rc = mca_memheap_base_reg(&mca_memheap_base_map);
    }

    OPAL_TIMING_ENV_NEXT(timing, "mca_memheap_base_reg()");

    /* Init OOB channel */
    if (OSHMEM_SUCCESS == rc) {
        rc = memheap_oob_init(&mca_memheap_base_map);
    }
    OPAL_TIMING_ENV_NEXT(timing, "memheap_oob_init()");

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
    OPAL_TIMING_ENV_NEXT(timing, "DONE");

    return ((OSHMEM_SUCCESS == rc) ? &context : NULL );
}
