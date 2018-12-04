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

static int a2a_alg_simple(struct oshmem_group_t *group,
                          void *target,
                          const void *source,
                          size_t nelems,
                          size_t element_size);

static int a2as_alg_simple(struct oshmem_group_t *group,
                           void *target,
                           const void *source,
                           ptrdiff_t dst, ptrdiff_t sst,
                           size_t nelems,
                           size_t element_size);


int mca_scoll_basic_alltoall(struct oshmem_group_t *group,
                             void *target,
                             const void *source,
                             ptrdiff_t dst, ptrdiff_t sst,
                             size_t nelems,
                             size_t element_size,
                             long *pSync,
                             int alg)
{
    int rc;
    int i;

    /* Arguments validation */
    if (!group) {
        SCOLL_ERROR("Active set (group) of PE is not defined");
        return OSHMEM_ERR_BAD_PARAM;
    }

    /* Check if this PE is part of the group */
    if (!oshmem_proc_group_is_member(group)) {
        return OSHMEM_SUCCESS;
    }

    if (!pSync) {
        SCOLL_ERROR("Incorrect argument pSync");
        return OSHMEM_ERR_BAD_PARAM;
    }

    /* Do nothing on zero-length request */
    if (OPAL_UNLIKELY(!nelems)) {
        return OPAL_SUCCESS;
    }

    if ((sst == 1) && (dst == 1)) {
        rc = a2a_alg_simple(group, target, source, nelems, element_size);
    } else {
        rc = a2as_alg_simple(group, target, source, dst, sst, nelems,
                             element_size);
    }

    if (rc != OSHMEM_SUCCESS) {
       return rc;
    }

    /* quiet is needed because scoll level barrier does not
     * guarantee put completion
     */
    MCA_SPML_CALL(quiet(oshmem_ctx_default));

    /* Wait for operation completion */
    SCOLL_VERBOSE(14, "[#%d] Wait for operation completion", group->my_pe);
    rc = BARRIER_FUNC(group, pSync, SCOLL_DEFAULT_ALG);

    /* Restore initial values */
    SCOLL_VERBOSE(12, "PE#%d Restore special synchronization array",
                  group->my_pe);

    for (i = 0; pSync && (i < _SHMEM_ALLTOALL_SYNC_SIZE); i++) {
        pSync[i] = _SHMEM_SYNC_VALUE;
    }

    return rc;
}


static inline void *
get_stride_elem(const void *base, ptrdiff_t sst, size_t nelems, size_t elem_size,
                int block_idx, int elem_idx)
{
    /*
     * j th block starts at: nelems * element_size * sst * j
     * offset of the l th element in the block is: element_size * sst * l
     */
    return (char *)base + elem_size * sst * (nelems * block_idx + elem_idx);
}

static inline int
get_dst_pe(struct oshmem_group_t *group, int src_blk_idx, int dst_blk_idx, int *dst_pe_idx)
{
    /* index permutation for better distribution of traffic */
    (*dst_pe_idx) = (dst_blk_idx + src_blk_idx) % group->proc_count;

    /* convert to the global pe */
    return oshmem_proc_pe(group->proc_array[*dst_pe_idx]);
}

static int a2as_alg_simple(struct oshmem_group_t *group,
                           void *target,
                           const void *source,
                           ptrdiff_t tst, ptrdiff_t sst,
                           size_t nelems,
                           size_t element_size)
{
    int rc;
    int dst_pe;
    int src_blk_idx;
    int dst_blk_idx;
    int dst_pe_idx;
    size_t elem_idx;

    SCOLL_VERBOSE(14,
                  "[#%d] send data to all PE in the group",
                  group->my_pe);

    dst_blk_idx = oshmem_proc_group_find_id(group, group->my_pe);

    for (src_blk_idx = 0; src_blk_idx < group->proc_count; src_blk_idx++) {

        dst_pe = get_dst_pe(group, src_blk_idx, dst_blk_idx, &dst_pe_idx);
        for (elem_idx = 0; elem_idx < nelems; elem_idx++) {
            rc = MCA_SPML_CALL(put(oshmem_ctx_default, 
                        get_stride_elem(target, tst, nelems, element_size,
                                        dst_blk_idx, elem_idx),
                        element_size,
                        get_stride_elem(source, sst, nelems, element_size,
                                        dst_pe_idx, elem_idx),
                        dst_pe));
            if (OSHMEM_SUCCESS != rc) {
                return rc;
            }
        }
    }
    return OSHMEM_SUCCESS;
}

static int a2a_alg_simple(struct oshmem_group_t *group,
                           void *target,
                           const void *source,
                           size_t nelems,
                           size_t element_size)
{
    int rc;
    int dst_pe;
    int src_blk_idx;
    int dst_blk_idx;
    int dst_pe_idx;
    void *dst_blk;

    SCOLL_VERBOSE(14,
                  "[#%d] send data to all PE in the group",
                  group->my_pe);

    dst_blk_idx = oshmem_proc_group_find_id(group, group->my_pe);

    /* block start at stride 1 first elem */
    dst_blk = get_stride_elem(target, 1, nelems, element_size, dst_blk_idx, 0);

    for (src_blk_idx = 0; src_blk_idx < group->proc_count; src_blk_idx++) {

        dst_pe = get_dst_pe(group, src_blk_idx, dst_blk_idx, &dst_pe_idx);
        rc = MCA_SPML_CALL(put(oshmem_ctx_default, dst_blk,
                                nelems * element_size,
                                get_stride_elem(source, 1, nelems,
                                                element_size, dst_pe_idx, 0),
                                dst_pe));
        if (OSHMEM_SUCCESS != rc) {
            return rc;
        }
    }
    return OSHMEM_SUCCESS;
}
