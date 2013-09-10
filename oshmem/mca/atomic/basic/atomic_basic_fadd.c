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
#include "atomic_basic.h"

int mca_atomic_basic_fadd(void *target,
                          void *prev,
                          const void *value,
                          size_t nlong,
                          int pe,
                          struct oshmem_op_t *op)
{
    int rc = OSHMEM_SUCCESS;

    if (!target || !value) {
        rc = OSHMEM_ERROR;
    }

    if (rc == OSHMEM_SUCCESS) {
        long long temp_value = 0;

        atomic_basic_lock(pe);

        rc = MCA_SPML_CALL(get(target, nlong, (void*)&temp_value, pe));

        if (prev)
            memcpy(prev, (void*) &temp_value, nlong);

        op->o_func.c_fn((void*) value,
                        (void*) &temp_value,
                        nlong / op->dt_size);

        if (rc == OSHMEM_SUCCESS) {
            rc = MCA_SPML_CALL(put(target, nlong, (void*)&temp_value, pe));
            shmem_quiet();
        }

        atomic_basic_unlock(pe);
    }

    return rc;
}
