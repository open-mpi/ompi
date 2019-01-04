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

#include <stdlib.h>

#include "orte/util/show_help.h"

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"
#include "oshmem/shmem/shmem_api_logger.h"

#include "oshmem/runtime/runtime.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"


#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ptr = pshmem_ptr
#include "oshmem/shmem/c/profile/defines.h"
#endif

void *shmem_ptr(const void *dst_addr, int pe)
{
    ompi_proc_t *proc;
    sshmem_mkey_t *mkey;
    int i;
    void *rva;

    RUNTIME_CHECK_INIT();
    RUNTIME_CHECK_PE(pe);
    RUNTIME_CHECK_ADDR(dst_addr);

    /* process can access its own memory */
    if (pe == oshmem_my_proc_id()) {
        return (void *)dst_addr;
    }

    /* The memory must be on the local node */
    proc = oshmem_proc_group_find(oshmem_group_all, pe);
    if (!OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)) {
        return NULL;
    }

    for (i = 0; i < mca_memheap_base_num_transports(); i++) {
        /* TODO: iterate on all ctxs, try to get cached mkeys */
        mkey = mca_memheap_base_get_cached_mkey(oshmem_ctx_default, pe, (void *)dst_addr, i, &rva);
        if (!mkey) {
            continue;
        }

        if (mca_memheap_base_mkey_is_shm(mkey)) {
            return rva;
        }

        rva = MCA_SPML_CALL(rmkey_ptr(dst_addr, mkey, pe));
        if (rva != NULL) {
            return rva;
        }
    }

    return NULL;
}
