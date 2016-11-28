/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
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
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"
#include "scoll_basic.h"

static int _algorithm_simple(struct oshmem_group_t *group,
                             void *target,
                             const void *source,
                             ptrdiff_t dst, ptrdiff_t sst,
                             size_t nelems,
                             size_t element_size,
                             long *pSync);

int mca_scoll_basic_alltoall(struct oshmem_group_t *group,
                             void *target,
                             const void *source,
                             ptrdiff_t dst, ptrdiff_t sst,
                             size_t nelems,
                             size_t element_size,
                             long *pSync,
                             int alg)
{
    int rc = OSHMEM_SUCCESS;

    /* Arguments validation */
    if (!group) {
        SCOLL_ERROR("Active set (group) of PE is not defined");
        rc = OSHMEM_ERR_BAD_PARAM;
    }

    /* Check if this PE is part of the group */
    if ((rc == OSHMEM_SUCCESS) && oshmem_proc_group_is_member(group)) {
        int i = 0;

        if (pSync) {
            rc = _algorithm_simple(group,
                                   target,
                                   source,
                                   dst,
                                   sst,
                                   nelems,
                                   element_size,
                                   pSync);
        } else {
            SCOLL_ERROR("Incorrect argument pSync");
            rc = OSHMEM_ERR_BAD_PARAM;
        }

        /* Restore initial values */
        SCOLL_VERBOSE(12,
                      "PE#%d Restore special synchronization array",
                      group->my_pe);
        for (i = 0; pSync && (i < _SHMEM_ALLTOALL_SYNC_SIZE); i++) {
            pSync[i] = _SHMEM_SYNC_VALUE;
        }
    }

    return rc;
}

static int _algorithm_simple(struct oshmem_group_t *group,
                             void *target,
                             const void *source,
                             ptrdiff_t tst, ptrdiff_t sst,
                             size_t nelems,
                             size_t element_size,
                             long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    int pe_cur;
    int i;
    int j;
    int k;

    SCOLL_VERBOSE(14,
                  "[#%d] send data to all PE in the group",
                  group->my_pe);
    j = oshmem_proc_group_find_id(group, group->my_pe);
    for (i = 0; i < group->proc_count; i++) {
        /* index permutation for better distribution of traffic */
        k = (((j)+(i))%(group->proc_count));
        pe_cur = oshmem_proc_pe(group->proc_array[k]);
        rc = MCA_SPML_CALL(put(
                (void *)((char *)target + j * tst * nelems * element_size),
                nelems * element_size,
                (void *)((char *)source + i * sst * nelems * element_size),
                pe_cur));
        if (OSHMEM_SUCCESS != rc) {
            break;
        }
    }
    /* fence (which currently acts as quiet) is needed
     * because scoll level barrier does not guarantee put completion
     */
    MCA_SPML_CALL(fence());

    /* Wait for operation completion */
    if (rc == OSHMEM_SUCCESS) {
        SCOLL_VERBOSE(14, "[#%d] Wait for operation completion", group->my_pe);
        rc = BARRIER_FUNC(group,
                (pSync + 1),
                SCOLL_DEFAULT_ALG);
    }

    return rc;
}

