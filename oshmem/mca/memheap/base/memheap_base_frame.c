/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include <stdio.h>

#include "oshmem/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"

/*
 * The following file was created by configure. It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "oshmem/mca/memheap/base/static-components.h"


int mca_memheap_base_output = -1;
int mca_memheap_base_key_exchange = 1;
opal_list_t mca_memheap_base_components_opened = {{0}};
int mca_memheap_base_already_opened = 0;
mca_memheap_map_t mca_memheap_base_map = {{{{0}}}};

static int mca_memheap_base_register(mca_base_register_flag_t flags)
{
    (void) mca_base_var_register("oshmem",
                                 "memheap",
                                 "base",
                                 "key_exchange",
                                 "0|1 - disabled, enabled(default) force memory keys exchange",
                                 MCA_BASE_VAR_TYPE_INT,
                                 NULL,
                                 0,
                                 MCA_BASE_VAR_FLAG_SETTABLE,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_memheap_base_key_exchange);


    return OSHMEM_SUCCESS;
}

static int mca_memheap_base_close(void)
{
    if (mca_memheap_base_already_opened <= 0) {
        return OSHMEM_ERROR;
    }
    mca_memheap_base_already_opened--;
    if (mca_memheap_base_already_opened > 0) {
        return OSHMEM_SUCCESS;
    }

    memheap_oob_destruct();

    mca_memheap_base_dereg(&mca_memheap_base_map);

    mca_memheap_base_alloc_exit(&mca_memheap_base_map);
    mca_memheap_base_static_exit(&mca_memheap_base_map);

    /* Close all remaining available components */
    return mca_base_framework_components_close(&oshmem_memheap_base_framework, NULL);
}

static int mca_memheap_base_open(mca_base_open_flag_t flags)
{
    mca_memheap_base_already_opened = mca_memheap_base_already_opened + 1;
    if (mca_memheap_base_already_opened > 1) {
        return OSHMEM_SUCCESS;
    }

    memset(&mca_memheap_base_map, 0, sizeof(mca_memheap_base_map));
    mca_memheap_base_map.n_segments = 0;
    mca_memheap_base_map.num_transports = 0;

    oshmem_framework_open_output(&oshmem_memheap_base_framework);

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_framework_components_open(&oshmem_memheap_base_framework, flags)) {
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}

MCA_BASE_FRAMEWORK_DECLARE(oshmem, memheap,
                           "OSHMEM MEMHEAP",
                           mca_memheap_base_register,
                           mca_memheap_base_open,
                           mca_memheap_base_close,
                           mca_memheap_base_static_components,
                           MCA_BASE_FRAMEWORK_FLAG_DEFAULT);
