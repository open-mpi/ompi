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
#include "oshmem/op/op.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/runtime/runtime.h"

#include "atomic_ucx.h"

int mca_atomic_ucx_fadd(void *target,
                        void *prev,
                        const void *value,
                        size_t nlong,
                        int pe,
                        struct oshmem_op_t *op)
{
    /* TODO: actual code */
    return OSHMEM_SUCCESS;
}
