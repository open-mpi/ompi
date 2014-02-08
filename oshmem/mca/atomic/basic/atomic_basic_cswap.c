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
#include "atomic_basic.h"

int mca_atomic_basic_cswap(void *target,
                           void *prev,
                           const void *cond,
                           const void *value,
                           size_t nlong,
                           int pe)
{
    int rc = OSHMEM_SUCCESS;

    if (!prev) {
        rc = OSHMEM_ERROR;
    }

    if (rc == OSHMEM_SUCCESS) {
        atomic_basic_lock(pe);

        rc = MCA_SPML_CALL(get(target, nlong, prev, pe));

        if ((rc == OSHMEM_SUCCESS) && (!cond || !memcmp(prev, cond, nlong))) {
            rc = MCA_SPML_CALL(put(target, nlong, (void*)value, pe));
            shmem_quiet();
        }

        atomic_basic_unlock(pe);
    }

    return rc;
}
