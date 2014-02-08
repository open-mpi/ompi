/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include <stdio.h>

#include "oshmem_config.h"

#include "oshmem/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "oshmem/mca/scoll/base/static-components.h"

/*
 * Global variables; most of which are loaded by back-ends of MCA
 * variables
 */
long* mca_scoll_sync_array = NULL;

/*
 * Ensure all function pointers are NULL'ed out to start with
 */
static void scoll_base_module_construct(mca_scoll_base_module_t *m)
{
    /* Collective function pointers */
    m->scoll_barrier = NULL;
    m->scoll_broadcast = NULL;
    m->scoll_collect = NULL;
    m->scoll_reduce = NULL;
    m->scoll_module_enable = NULL;
}

OBJ_CLASS_INSTANCE(mca_scoll_base_module_t, opal_object_t,
                   scoll_base_module_construct, NULL);

int mca_scoll_enable(void)
{
    int ret = OSHMEM_SUCCESS;

    if (!mca_scoll_sync_array) {
        void* ptr = (void*) mca_scoll_sync_array;
        int i = 0;

        MCA_MEMHEAP_CALL(private_alloc((_SHMEM_BARRIER_SYNC_SIZE * sizeof(*mca_scoll_sync_array)), &ptr));
        mca_scoll_sync_array = ptr;

        for (i = 0; i < _SHMEM_BARRIER_SYNC_SIZE; i++) {
            mca_scoll_sync_array[i] = _SHMEM_SYNC_VALUE;
        }
    }

    /* Note: it is done to support FCA only and we need to consider possibility to
     * find a way w/o this ugly hack
     */
    if (OSHMEM_SUCCESS != (ret = mca_scoll_base_select(oshmem_group_all))) {
        return ret;
    }
    if (OSHMEM_SUCCESS != (ret = mca_scoll_base_select(oshmem_group_self))) {
        return ret;
    }

    return OSHMEM_SUCCESS;
}

static int mca_scoll_base_register(mca_base_register_flag_t flags)
{
    return OSHMEM_SUCCESS;
}

static int mca_scoll_base_close(void)
{
    /* This call should be done before memheap close */
    if (mca_scoll_sync_array) {
        void* ptr = (void*) mca_scoll_sync_array;

        MCA_MEMHEAP_CALL(private_free(ptr));
        mca_scoll_sync_array = NULL;
    }

    return mca_base_framework_components_close(&oshmem_scoll_base_framework, NULL);
}

static int mca_scoll_base_open(mca_base_open_flag_t flags)
{
    /* Open up all available components */
    if (OPAL_SUCCESS !=
            mca_base_framework_components_open(&oshmem_scoll_base_framework, flags)) {
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

MCA_BASE_FRAMEWORK_DECLARE(oshmem, scoll,
                           "OSHMEM SCOLL",
                           mca_scoll_base_register,
                           mca_scoll_base_open,
                           mca_scoll_base_close,
                           mca_scoll_base_static_components,
                           MCA_BASE_FRAMEWORK_FLAG_DEFAULT);
