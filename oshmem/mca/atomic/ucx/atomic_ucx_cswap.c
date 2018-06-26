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

/* nlong argument should be constant to hint compiler
 * to calculate nlong relative branches in compile time */
static inline
int mca_atomic_ucx_cswap_inner(void *target,
                               void *prev,
                               const void *cond,
                               const void *value,
                               size_t nlong,
                               int pe)
{
    ucs_status_t status;
    ucs_status_ptr_t status_ptr;
    spml_ucx_mkey_t *ucx_mkey;
    uint64_t rva;
    uint64_t val;
    uint64_t cmp;

    val = (4 == nlong) ? *(uint32_t*)value : *(uint64_t*)value;
    ucx_mkey = mca_spml_ucx_get_mkey(pe, target, (void *)&rva); 
    if (NULL == cond) {
        status_ptr = ucp_atomic_fetch_nb(mca_spml_self->ucp_peers[pe].ucp_conn,
                                         UCP_ATOMIC_FETCH_OP_SWAP, val, prev, nlong,
                                         rva, ucx_mkey->rkey,
                                         opal_common_ucx_empty_complete_cb);
        status = opal_common_ucx_wait_request(status_ptr, mca_spml_self->ucp_worker);
    }
    else {
        cmp = (4 == nlong) ? *(uint32_t*)cond : *(uint64_t*)cond;
        status_ptr = ucp_atomic_fetch_nb(mca_spml_self->ucp_peers[pe].ucp_conn,
                                         UCP_ATOMIC_FETCH_OP_CSWAP, cmp, &val, nlong,
                                         rva, ucx_mkey->rkey,
                                         opal_common_ucx_empty_complete_cb);
        status = opal_common_ucx_wait_request(status_ptr, mca_spml_self->ucp_worker);
        if (UCS_OK == status) {
            assert(NULL != prev);
            memcpy(prev, &val, nlong);
            if (4 == nlong) {
                *(uint32_t*)prev = val;
            } else {
                *(uint64_t*)prev = val;
            }
        }
    }
    return ucx_status_to_oshmem(status);
}

int mca_atomic_ucx_cswap(void *target,
                         void *prev,
                         const void *cond,
                         const void *value,
                         size_t nlong,
                         int pe)
{
    if (8 == nlong) {
        return mca_atomic_ucx_cswap_inner(target, prev, cond, value, 8, pe);
    } else if (4 == nlong) {
        return mca_atomic_ucx_cswap_inner(target, prev, cond, value, 4, pe);
    } else {
        ATOMIC_ERROR("[#%d] Type size must be 4 or 8 bytes.", my_pe);
        return OSHMEM_ERROR;
    }
}
