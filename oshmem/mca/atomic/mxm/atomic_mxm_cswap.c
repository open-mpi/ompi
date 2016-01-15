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
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/runtime/runtime.h"

#include "atomic_mxm.h"

int mca_atomic_mxm_cswap(void *target,
                         void *prev,
                         const void *cond,
                         const void *value,
                         size_t nlong,
                         int pe)
{
    unsigned my_pe;
    uint8_t nlong_order;
    void *remote_addr;
    int ptl_id;
    mxm_send_req_t sreq;
    mxm_error_t mxm_err;
    sshmem_mkey_t *r_mkey;

    my_pe = oshmem_my_proc_id();
    ptl_id = -1;
    mxm_err = MXM_OK;

    if (!prev || !target || !value) {
        ATOMIC_ERROR("[#%d] Whether target, value or prev are not defined",
                     my_pe);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERR_BAD_PARAM;
    }
    if ((pe < 0) || (pe >= oshmem_num_procs())) {
        ATOMIC_ERROR("[#%d] PE=%d not valid", my_pe, pe);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERR_BAD_PARAM;
    }

    switch (nlong) {
    case 1:
        nlong_order = 0;
        break;
    case 2:
        nlong_order = 1;
        break;
    case 4:
        nlong_order = 2;
        break;
    case 8:
        nlong_order = 3;
        break;
    default:
        ATOMIC_ERROR("[#%d] Type size must be 1/2/4 or 8 bytes.", my_pe);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERR_BAD_PARAM;
    }

    ptl_id = oshmem_proc_group_all(pe)->transport_ids[0];
    if (MXM_PTL_SHM == ptl_id) {
        ptl_id = MXM_PTL_RDMA;
    }
    r_mkey = mca_memheap_base_get_cached_mkey(pe, target, ptl_id, &remote_addr);
    if (!r_mkey) {
        ATOMIC_ERROR("[#%d] %p is not address of symmetric variable",
                     my_pe, target);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERR_BAD_PARAM;
    }

    /* mxm request init */
    sreq.base.state = MXM_REQ_NEW;
    sreq.base.mq = mca_spml_self->mxm_mq;
    sreq.base.conn = mca_spml_self->mxm_peers[pe]->mxm_hw_rdma_conn;
    sreq.base.completed_cb = NULL;
    sreq.base.data_type = MXM_REQ_DATA_BUFFER;

    /* set data */
    sreq.base.data.buffer.ptr = (void *) value;
    sreq.base.data.buffer.length = nlong;
    sreq.base.data.buffer.memh = MXM_INVALID_MEM_HANDLE;

    sreq.op.atomic.remote_vaddr = (uintptr_t) remote_addr;
#if MXM_API < MXM_VERSION(2,0)
    sreq.base.flags = 0;
    sreq.op.atomic.remote_memh  = MXM_INVALID_MEM_HANDLE;
#else
    sreq.flags = 0;
    sreq.op.atomic.remote_mkey = to_mxm_mkey(r_mkey);
#endif
    sreq.op.atomic.order = nlong_order;

    if (NULL == cond) {
        sreq.opcode = MXM_REQ_OP_ATOMIC_SWAP;
    } else {
#if MXM_API < MXM_VERSION(2,0)
        memcpy(&sreq.op.atomic.value8, cond, nlong);
#else
        memcpy(&sreq.op.atomic.value, cond, nlong);
#endif
        sreq.opcode = MXM_REQ_OP_ATOMIC_CSWAP;
    }

    if (MXM_OK != (mxm_err = mxm_req_send(&sreq))) {
        ATOMIC_ERROR("[#%d] mxm_req_send failed, mxm_error = %d",
                     my_pe, mxm_err);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

    mxm_req_wait(&sreq.base);
    if (MXM_OK != sreq.base.error) {
        ATOMIC_ERROR("[#%d] mxm_req_wait got non MXM_OK error: %d",
                     my_pe, sreq.base.error);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    memcpy(prev, value, nlong);

    return OSHMEM_SUCCESS;
}

