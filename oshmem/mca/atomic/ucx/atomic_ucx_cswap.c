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
#include <stdio.h>
#include <stdlib.h>

#include "oshmem/constants.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"
#include "oshmem/runtime/runtime.h"

#include "atomic_ucx.h"

/* size argument should be constant to hint compiler
 * to calculate size relative branches in compile time */
static inline
int mca_atomic_ucx_cswap_inner(void *target,
                               void *prev,
                               uint64_t cond,
                               uint64_t value,
                               size_t size,
                               int pe)
{
    int status;
    ucs_status_ptr_t status_ptr;
    spml_ucx_mkey_t *ucx_mkey;
    uint64_t rva;
    uint64_t val;

    val        = value;
    ucx_mkey   = mca_spml_ucx_get_mkey(pe, target, (void *)&rva);
    status_ptr = ucp_atomic_fetch_nb(mca_spml_self->ucp_peers[pe].ucp_conn,
                                     UCP_ATOMIC_FETCH_OP_CSWAP, cond, &val, size,
                                     rva, ucx_mkey->rkey,
                                     opal_common_ucx_empty_complete_cb);
    status = opal_common_ucx_wait_request_opal_status(status_ptr, mca_spml_self->ucp_worker);
    if (OPAL_SUCCESS == status) {
        assert(NULL != prev);
        if (sizeof(uint32_t) == size) {
            *(uint32_t*)prev = val;
        } else {
            *(uint64_t*)prev = val;
        }
    }
    return status;
}

int mca_atomic_ucx_cswap(void *target,
                         void *prev,
                         uint64_t cond,
                         uint64_t value,
                         size_t size,
                         int pe)
{
    if (sizeof(uint64_t) == size) {
        return mca_atomic_ucx_cswap_inner(target, prev, cond, value, sizeof(uint64_t), pe);
    } else if (sizeof(uint32_t) == size) {
        return mca_atomic_ucx_cswap_inner(target, prev, cond, value, sizeof(uint32_t), pe);
    } else {
        ATOMIC_ERROR("[#%d] Type size must be 4 or 8 bytes.", my_pe);
        return OSHMEM_ERROR;
    }
}
