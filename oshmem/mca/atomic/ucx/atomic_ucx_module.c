/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include <stdio.h>

#include "oshmem/constants.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/proc/proc.h"
#include "atomic_ucx.h"

/*
 * Initial query function that is invoked during initialization, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_atomic_ucx_init(bool enable_progress_threads, bool enable_threads)
{
    return OSHMEM_SUCCESS;
}

int mca_atomic_ucx_finalize(void)
{
    return OSHMEM_SUCCESS;
}

static inline
int mca_atomic_ucx_op(void *target,
                      const void *value,
                      size_t size,
                      int pe,
                      ucp_atomic_post_op_t op)
{
    ucs_status_t status;
    ucs_status_ptr_t status_ptr;
    spml_ucx_mkey_t *ucx_mkey;
    uint64_t rva;
    uint64_t val;

    if (8 == size) {
        val = *(uint64_t*)value;
    } else if (4 == size) {
        val = *(uint32_t*)value;
    } else {
        ATOMIC_ERROR("[#%d] Type size must be 4 or 8 bytes.", my_pe);
        return OSHMEM_ERROR;
    }

    ucx_mkey = mca_spml_ucx_get_mkey(pe, target, (void *)&rva);
    status = ucp_atomic_post(mca_spml_self->ucp_peers[pe].ucp_conn,
                             op, val, size, rva,
                             ucx_mkey->rkey);
    return ucx_status_to_oshmem(status);
}

static inline
int mca_atomic_ucx_fop(void *target,
                       void *prev,
                       const void *value,
                       size_t size,
                       int pe,
                       ucp_atomic_fetch_op_t op)
{
    ucs_status_t status;
    ucs_status_ptr_t status_ptr;
    spml_ucx_mkey_t *ucx_mkey;
    uint64_t rva;
    uint64_t val;

    if (8 == size) {
        val = *(uint64_t*)value;
    } else if (4 == size) {
        val = *(uint32_t*)value;
    } else {
        ATOMIC_ERROR("[#%d] Type size must be 4 or 8 bytes.", my_pe);
        return OSHMEM_ERROR;
    }

    ucx_mkey = mca_spml_ucx_get_mkey(pe, target, (void *)&rva);
    status_ptr = ucp_atomic_fetch_nb(mca_spml_self->ucp_peers[pe].ucp_conn,
                                     op, val, prev, size,
                                     rva, ucx_mkey->rkey,
                                     opal_common_ucx_empty_complete_cb);
    status = opal_common_ucx_wait_request(status_ptr, mca_spml_self->ucp_worker);

    return ucx_status_to_oshmem(status);
}

static int mca_atomic_ucx_add(void *target,
                              const void *value,
                              size_t size,
                              int pe)
{
    return mca_atomic_ucx_op(target, value, size, pe, UCP_ATOMIC_POST_OP_ADD);
}

static int mca_atomic_ucx_fadd(void *target,
                               void *prev,
                               const void *value,
                               size_t size,
                               int pe)
{
    return mca_atomic_ucx_fop(target, prev, value, size, pe, UCP_ATOMIC_FETCH_OP_FADD);
}

static int mca_atomic_ucx_swap(void *target,
                               void *prev,
                               const void *value,
                               size_t size,
                               int pe)
{
    return mca_atomic_ucx_fop(target, prev, value, size, pe, UCP_ATOMIC_FETCH_OP_SWAP);
}


mca_atomic_base_module_t *
mca_atomic_ucx_query(int *priority)
{
    mca_atomic_ucx_module_t *module;

    *priority = mca_atomic_ucx_component.priority;

    module = OBJ_NEW(mca_atomic_ucx_module_t);
    if (module) {
        module->super.atomic_add   = mca_atomic_ucx_add;
        module->super.atomic_fadd  = mca_atomic_ucx_fadd;
        module->super.atomic_swap  = mca_atomic_ucx_swap;
        module->super.atomic_cswap = mca_atomic_ucx_cswap;
        return &(module->super);
    }

    return NULL ;
}
