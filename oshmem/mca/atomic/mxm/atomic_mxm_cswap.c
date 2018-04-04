/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
    mxm_send_req_t sreq;

    mca_atomic_mxm_req_init(&sreq, pe, target, nlong);
    memcpy(prev, value, nlong);

    sreq.base.data.buffer.ptr = prev;
    if (NULL == cond) {
        sreq.opcode = MXM_REQ_OP_ATOMIC_SWAP;
    } else {
        memcpy(&sreq.op.atomic.value, cond, nlong);
        sreq.opcode = MXM_REQ_OP_ATOMIC_CSWAP;
    }

    mca_atomic_mxm_post(&sreq);

    return OSHMEM_SUCCESS;
}

