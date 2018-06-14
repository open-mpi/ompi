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

#include "atomic_ucx.h"

int mca_atomic_ucx_fadd(void *target,
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

    assert(NULL != prev);

    MCA_ATOMIC_UCX_GTE_VAL(val, value, size, pe);

    ucx_mkey = mca_spml_ucx_get_mkey(pe, target, (void *)&rva);
    status_ptr = ucp_atomic_fetch_nb(mca_spml_self->ucp_peers[pe].ucp_conn,
                                     UCP_ATOMIC_FETCH_OP_FADD, val, prev, size,
                                     rva, ucx_mkey->rkey, mca_atomic_ucx_complete_cb);
    status = mca_atomic_ucx_wait_request(status_ptr);

    return ucx_status_to_oshmem(status);
}

int mca_atomic_ucx_add(void *target,
                       const void *value,
                       size_t size,
                       int pe,
                       struct oshmem_op_t *op)
{
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey;
    uint64_t rva;
    uint64_t val;

    MCA_ATOMIC_UCX_GTE_VAL(val, value, size, pe);

    ucx_mkey = mca_spml_ucx_get_mkey(pe, target, (void *)&rva);
    status = ucp_atomic_post(mca_spml_self->ucp_peers[pe].ucp_conn,
                             UCP_ATOMIC_POST_OP_ADD, val, size, rva,
                             ucx_mkey->rkey);
    return ucx_status_to_oshmem(status);
}
