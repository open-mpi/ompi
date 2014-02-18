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

#include "orte/mca/grpcomm/grpcomm.h"

#include "opal/util/bit_ops.h"

#include "oshmem/constants.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"
#include "scoll_basic.h"

static int _algorithm_central_counter(struct oshmem_group_t *group,
                                       int PE_root,
                                       void *target,
                                       const void *source,
                                       size_t nlong,
                                       long *pSync);
static int _algorithm_binomial_tree(struct oshmem_group_t *group,
                                     int PE_root,
                                     void *target,
                                     const void *source,
                                     size_t nlong,
                                     long *pSync);

int mca_scoll_basic_broadcast(struct oshmem_group_t *group,
                              int PE_root,
                              void *target,
                              const void *source,
                              size_t nlong,
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
            alg = (alg == SCOLL_DEFAULT_ALG ?
                    mca_scoll_basic_param_broadcast_algorithm : alg);
            switch (alg) {
            case SCOLL_ALG_BROADCAST_CENTRAL_COUNTER:
                {
                    rc = _algorithm_central_counter(group,
                                                     PE_root,
                                                     target,
                                                     source,
                                                     nlong,
                                                     pSync);
                    break;
                }
            case SCOLL_ALG_BROADCAST_BINOMIAL:
                {
                    rc = _algorithm_binomial_tree(group,
                                                   PE_root,
                                                   target,
                                                   source,
                                                   nlong,
                                                   pSync);
                    break;
                }
            default:
                {
                    rc = _algorithm_binomial_tree(group,
                                                   PE_root,
                                                   target,
                                                   source,
                                                   nlong,
                                                   pSync);
                }
            }
        } else {
            SCOLL_ERROR("Incorrect argument pSync");
            rc = OSHMEM_ERR_BAD_PARAM;
        }

        /* Restore initial values */
        SCOLL_VERBOSE(12,
                      "[#%d] Restore special synchronization array",
                      group->my_pe);
        for (i = 0; pSync && (i < _SHMEM_BCAST_SYNC_SIZE); i++) {
            pSync[i] = _SHMEM_SYNC_VALUE;
        }
    }

    return rc;
}

/*
 This algorithm is quite simple and straightforward. But because of it�s obvious simplicity and
 the naive prove for correctness it is implemented quite often. The root send data to all.
 Outlay:
 NP-1 competing network transfers are needed to implement the counter
 The memory usage is constant (1 byte) per node.
 */
static int _algorithm_central_counter(struct oshmem_group_t *group,
                                       int PE_root,
                                       void *target,
                                       const void *source,
                                       size_t nlong,
                                       long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    int i = 0;

    SCOLL_VERBOSE(12,
                  "[#%d] Broadcast algorithm: Central Counter",
                  group->my_pe);
    SCOLL_VERBOSE(15,
                  "[#%d] pSync[0] = %ld root = #%d",
                  group->my_pe, pSync[0], PE_root);

    /* Check if this PE is the root */
    if (PE_root == group->my_pe) {
        int pe_cur = 0;

        SCOLL_VERBOSE(14,
                      "[#%d] send data to all PE in the group",
                      group->my_pe);
        for (i = 0; (i < group->proc_count) && (rc == OSHMEM_SUCCESS); i++) {
            pe_cur = oshmem_proc_pe(group->proc_array[i]);
            if (pe_cur != PE_root) {
                SCOLL_VERBOSE(15,
                              "[#%d] send data to #%d",
                              group->my_pe, pe_cur);
                rc = MCA_SPML_CALL(put(target, nlong, (void *)source, pe_cur));
            }
        }
    }

    /* Wait for operation completion to set needed size */
    if (rc == OSHMEM_SUCCESS) {
        SCOLL_VERBOSE(14, "[#%d] Wait for operation completion", group->my_pe);
        rc = BARRIER_FUNC(group,
                (pSync + 1),
                SCOLL_DEFAULT_ALG);
    }

    return rc;
}

/*
 The Binomial Spanning Tree algorithm.
 Outlay:
 The game scales with log2(NP) and uses 1 byte of memory.
 */
static int _algorithm_binomial_tree(struct oshmem_group_t *group,
                                     int PE_root,
                                     void *target,
                                     const void *source,
                                     size_t nlong,
                                     long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    long value = SHMEM_SYNC_INIT;
    int root_id = oshmem_proc_group_find_id(group, PE_root);
    int my_id = oshmem_proc_group_find_id(group, group->my_pe);
    int peer_id = 0;
    int peer_pe = 0;
    int vrank;
    int dim = opal_cube_dim(group->proc_count);
    int hibit;
    int mask;
    int i = 0;

    SCOLL_VERBOSE(12, "[#%d] Broadcast algorithm: Tree", group->my_pe);
    SCOLL_VERBOSE(15,
                  "[#%d] pSync[0] = %ld root = #%d",
                  group->my_pe, pSync[0], PE_root);

    vrank = (my_id + group->proc_count - root_id) % group->proc_count;
    hibit = opal_hibit(vrank, dim);

    SCOLL_VERBOSE(15,
                  "[#%d] dim = %d vrank = %d hibit = %d",
                  group->my_pe, dim, vrank, hibit);

    dim--;

    pSync[0] = SHMEM_SYNC_READY;
    /* Receive data from parent in the tree. */
    if (vrank > 0) {
        value = SHMEM_SYNC_READY;

        SCOLL_VERBOSE(14, "[#%d] wait", group->my_pe);
        rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_NE, (void*)&value, SHMEM_LONG));
        while ((value = pSync[0]) < 0) {
            SCOLL_VERBOSE(14,
                          "[#%d] Broadcast size is a negative value (%li)\n",
                          group->my_pe, pSync[0]);
            MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_NE, (void*)&value, SHMEM_LONG));
        }
        if (OSHMEM_SUCCESS != rc) {
            return rc;
        }
        nlong = (size_t) pSync[0];
    }

    /* Send data to the children. */
    for (i = hibit + 1, mask = 1 << i; i <= dim; ++i, mask <<= 1) {
        peer_id = vrank | mask;

        if (peer_id < group->proc_count) {
            /* Wait for the child to be ready to receive (pSync must have the initial value) */
            peer_id = (peer_id + root_id) % group->proc_count;
            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

            SCOLL_VERBOSE(14,
                          "[#%d] check remote pe is ready to receive #%d",
                          group->my_pe, peer_pe);
            do {
                rc = MCA_SPML_CALL(get((void*)pSync, sizeof(long), (void*)pSync, peer_pe));
            } while ((OSHMEM_SUCCESS == rc) && (pSync[0] != SHMEM_SYNC_READY));

            SCOLL_VERBOSE(14, "[#%d] send data to #%d", group->my_pe, peer_pe);
            rc = MCA_SPML_CALL(put(target, nlong, (my_id == root_id ? (void *)source : target), peer_pe));

            MCA_SPML_CALL(fence());

            SCOLL_VERBOSE(14, "[#%d] signals to #%d", group->my_pe, peer_pe);
            value = nlong;
            rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
            if (OSHMEM_SUCCESS != rc) {
                break;
            }
        }
    }

    return rc;
}
