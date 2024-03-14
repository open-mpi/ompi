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

int mca_atomic_ucx_cswap(shmem_ctx_t ctx,
                         void *target,
                         uint64_t *prev,
                         uint64_t cond,
                         uint64_t value,
                         size_t size,
                         int pe)
{
    ucs_status_ptr_t status_ptr;
    spml_ucx_mkey_t *ucx_mkey;
    uint64_t rva;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
#if HAVE_DECL_UCP_ATOMIC_OP_NBX
    ucp_request_param_t param = {
        .op_attr_mask = UCP_OP_ATTR_FIELD_DATATYPE |
                        UCP_OP_ATTR_FIELD_REPLY_BUFFER,
        .datatype     = ucp_dt_make_contig(size),
        .reply_buffer = prev
    };
#endif
    assert(NULL != prev);

    if (size == 8) {
        *prev = value;
    } else if (size == 4) {
        *(uint32_t*)prev = value;
    } else {
        ATOMIC_ERROR("[#%d] Type size must be 4 or 8 bytes.", my_pe);
        return OSHMEM_ERROR;
    }

    ucx_mkey   = mca_spml_ucx_ctx_mkey_by_va(ctx, pe, target, (void *)&rva, mca_spml_self);
    assert(NULL != ucx_mkey);
#if HAVE_DECL_UCP_ATOMIC_OP_NBX
    status_ptr = ucp_atomic_op_nbx(ucx_ctx->ucp_peers[pe].ucp_conn,
                                   UCP_ATOMIC_OP_CSWAP, &cond, 1, rva,
                                   ucx_mkey->rkey, &param);
#else
    status_ptr = ucp_atomic_fetch_nb(ucx_ctx->ucp_peers[pe].ucp_conn,
                                     UCP_ATOMIC_FETCH_OP_CSWAP, cond, prev, size,
                                     rva, ucx_mkey->rkey,
                                     opal_common_ucx_empty_complete_cb);
#endif

    if (OPAL_LIKELY(!UCS_PTR_IS_ERR(status_ptr))) {
        mca_spml_ucx_remote_op_posted(ucx_ctx, pe);
    }

    return opal_common_ucx_wait_request(status_ptr, ucx_ctx->ucp_worker[0],
#if HAVE_DECL_UCP_ATOMIC_OP_NBX
                                        "ucp_atomic_op_nbx");
#else
                                        "ucp_atomic_fetch_nb");
#endif
}

