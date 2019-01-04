/*
 * Copyright (c) 2013-2016 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem_config.h"

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"

#include "oshmem/runtime/runtime.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_addr_accessible = pshmem_addr_accessible
#include "oshmem/shmem/c/profile/defines.h"
#endif

int shmem_addr_accessible(const void *addr, int pe)
{
    void* rva;
    sshmem_mkey_t *mkey;
    int i;

    RUNTIME_CHECK_INIT();

    for (i = 0; i < mca_memheap_base_num_transports(); i++) {
        /* TODO: iterate on all ctxs, try to get cached mkey */
        mkey = mca_memheap_base_get_cached_mkey(oshmem_ctx_default, pe, (void *)addr, i, &rva);
        if (mkey) {
            return 1;
        }
    }

    return 0;
}
