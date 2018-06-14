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
                               const void *cond,
                               const void *value,
                               size_t size,
                               int pe)
{
    ucs_status_t status;
    ucs_status_ptr_t status_ptr;
    spml_ucx_mkey_t *ucx_mkey;
    uint64_t rva;
    uint64_t val;
    uint64_t cmp;

    val = (4 == size) ? *(uint32_t*)value : *(uint64_t*)value;
    cmp = (4 == size) ? *(uint32_t*)cond : *(uint64_t*)cond;
    ucx_mkey = mca_spml_ucx_get_mkey(pe, target, (void *)&rva); 
    status_ptr = ucp_atomic_fetch_nb(mca_spml_self->ucp_peers[pe].ucp_conn,
                                     UCP_ATOMIC_FETCH_OP_CSWAP, cmp, &val, size,
                                     rva, ucx_mkey->rkey, mca_atomic_ucx_complete_cb);
    status = mca_atomic_ucx_wait_request(status_ptr);
    if (UCS_OK == status) {
        assert(NULL != prev);
        memcpy(prev, &val, size);
        if (4 == size) {
            *(uint32_t*)prev = val;
        } else {
            *(uint64_t*)prev = val;
        }
    }
    return ucx_status_to_oshmem(status);
}

int mca_atomic_ucx_cswap(void *target,
                         void *prev,
                         const void *cond,
                         const void *value,
                         size_t size,
                         int pe)
{
    if (8 == size) {
        return mca_atomic_ucx_cswap_inner(target, prev, cond, value, 8, pe);
    } else if (4 == size) {
        return mca_atomic_ucx_cswap_inner(target, prev, cond, value, 4, pe);
    } else {
        ATOMIC_ERROR("[#%d] Type size must be 4 or 8 bytes.", my_pe);
        return OSHMEM_ERROR;
    }
}

int mca_atomic_ucx_swap(void *target,
                        void *prev,
                        const void *value,
                        size_t size,
                        int pe,
                        struct oshmem_op_t *op)
{
    ucs_status_t status;
    ucs_status_ptr_t status_ptr;
    spml_ucx_mkey_t *ucx_mkey;
    uint64_t rva;
    uint64_t val;

    MCA_ATOMIC_UCX_GTE_VAL(val, value, size, pe);
    ucx_mkey = mca_spml_ucx_get_mkey(pe, target, (void *)&rva);
    status_ptr = ucp_atomic_fetch_nb(mca_spml_self->ucp_peers[pe].ucp_conn,
                                     UCP_ATOMIC_FETCH_OP_SWAP, val, prev, size,
                                     rva, ucx_mkey->rkey, mca_atomic_ucx_complete_cb);
    status = mca_atomic_ucx_wait_request(status_ptr);
    return ucx_status_to_oshmem(status);
}

