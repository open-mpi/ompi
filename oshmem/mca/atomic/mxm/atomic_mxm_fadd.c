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
#include "oshmem/op/op.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/runtime/runtime.h"

#include "atomic_mxm.h"

int mca_atomic_mxm_fadd(void *target,
                        void *prev,
                        const void *value,
                        size_t nlong,
                        int pe,
                        struct oshmem_op_t *op)
{
    mxm_send_req_t sreq;
    static char dummy_buf[8];

    mca_atomic_mxm_req_init(&sreq, pe, target, nlong);

    memcpy(&sreq.op.atomic.value, value, nlong);
    sreq.opcode = MXM_REQ_OP_ATOMIC_FADD;
    if (NULL == prev) {
        sreq.base.data.buffer.ptr = dummy_buf;
    } else {
        sreq.base.data.buffer.ptr = prev;
    }

    mca_atomic_mxm_post(&sreq);

    return OSHMEM_SUCCESS;
}
