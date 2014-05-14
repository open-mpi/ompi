/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
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

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_addr_accessible = pshmem_addr_accessible
#include "oshmem/shmem/c/profile/defines.h"
#endif

int shmem_addr_accessible(void *addr, int pe)
{
    void* rva;
    sshmem_mkey_t *mkey;

    RUNTIME_CHECK_INIT();

    mkey = MCA_MEMHEAP_CALL(get_cached_mkey(pe, addr,
                    oshmem_get_transport_id(pe), &rva));

    return mkey ? 1 : 0;
}
