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

#include "opal/util/bit_ops.h"

#include "oshmem/constants.h"
#include "oshmem/op/op.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"
#include "scoll_basic.h"

static int _algorithm_central_counter(struct oshmem_group_t *group,
                                       struct oshmem_op_t *op,
                                       void *target,
                                       const void *source,
                                       size_t nlong,
                                       long *pSync,
                                       void *pWrk);
static int _algorithm_tournament(struct oshmem_group_t *group,
                                  struct oshmem_op_t *op,
                                  void *target,
                                  const void *source,
                                  size_t nlong,
                                  long *pSync,
                                  void *pWrk);
static int _algorithm_recursive_doubling(struct oshmem_group_t *group,
                                          struct oshmem_op_t *op,
                                          void *target,
                                          const void *source,
                                          size_t nlong,
                                          long *pSync,
                                          void *pWrk);
static int _algorithm_linear(struct oshmem_group_t *group,
                              struct oshmem_op_t *op,
                              void *target,
                              const void *source,
                              size_t nlong,
                              long *pSync,
                              void *pWrk);
static int _algorithm_log(struct oshmem_group_t *group,
                           struct oshmem_op_t *op,
                           void *target,
                           const void *source,
                           size_t nlong,
                           long *pSync,
                           void *pWrk);

int mca_scoll_basic_reduce(struct oshmem_group_t *group,
                           struct oshmem_op_t *op,
                           void *target,
                           const void *source,
                           size_t nlong,
                           long *pSync,
                           void *pWrk,
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
                    mca_scoll_basic_param_reduce_algorithm : alg);
            switch (alg) {
            case SCOLL_ALG_REDUCE_CENTRAL_COUNTER:
                {
                    rc = _algorithm_central_counter(group,
                                                     op,
                                                     target,
                                                     source,
                                                     nlong,
                                                     pSync,
                                                     pWrk);
                    break;
                }
            case SCOLL_ALG_REDUCE_TOURNAMENT:
                {
                    rc = _algorithm_tournament(group,
                                                op,
                                                target,
                                                source,
                                                nlong,
                                                pSync,
                                                pWrk);
                    break;
                }
            case SCOLL_ALG_REDUCE_RECURSIVE_DOUBLING:
                {
                    rc = _algorithm_recursive_doubling(group,
                                                        op,
                                                        target,
                                                        source,
                                                        nlong,
                                                        pSync,
                                                        pWrk);
                    break;
                }
            case SCOLL_ALG_REDUCE_LEGACY_LINEAR:
                {
                    rc = _algorithm_linear(group,
                                            op,
                                            target,
                                            source,
                                            nlong,
                                            pSync,
                                            pWrk);
                    break;
                }
            case SCOLL_ALG_REDUCE_LEGACY_LOG:
                {
                    rc = _algorithm_log(group,
                                         op,
                                         target,
                                         source,
                                         nlong,
                                         pSync,
                                         pWrk);
                    break;
                }
            default:
                {
                    rc = _algorithm_central_counter(group,
                                                     op,
                                                     target,
                                                     source,
                                                     nlong,
                                                     pSync,
                                                     pWrk);
                }
            }
        } else {
            SCOLL_ERROR("Incorrect argument pSync");
            rc = OSHMEM_ERR_BAD_PARAM;
        }

        /* Restore initial values */
        SCOLL_VERBOSE(12,
                      "PE#%d Restore special synchronization array",
                      group->my_pe);
        for (i = 0; pSync && (i < _SHMEM_REDUCE_SYNC_SIZE); i++) {
            pSync[i] = _SHMEM_SYNC_VALUE;
        }
    }

    return rc;
}

/*
 This algorithm is quite simple and straightforward for PEs with identical data size.
 One node gathers data from peers and send final result to them.
 Outlay:
 NP-1 competing network transfers are needed.
 */
static int _algorithm_central_counter(struct oshmem_group_t *group,
                                       struct oshmem_op_t *op,
                                       void *target,
                                       const void *source,
                                       size_t nlong,
                                       long *pSync,
                                       void *pWrk)
{
    int rc = OSHMEM_SUCCESS;
    int i = 0;
    int PE_root = oshmem_proc_pe(group->proc_array[0]);

    SCOLL_VERBOSE(12, "[#%d] Reduce algorithm: Central Counter", group->my_pe);

    if (PE_root == group->my_pe) {
        int pe_cur = 0;
        void *target_cur = NULL;

        target_cur = malloc(nlong);
        if (target_cur) {
            memcpy(target, (void *) source, nlong);

            SCOLL_VERBOSE(14,
                          "[#%d] Gather data from all PEs in the group",
                          group->my_pe);
            for (i = 0; (i < group->proc_count) && (rc == OSHMEM_SUCCESS);
                    i++) {
                /* Get PE ID of a peer from the group */
                pe_cur = oshmem_proc_pe(group->proc_array[i]);

                if (pe_cur == group->my_pe)
                    continue;

                SCOLL_VERBOSE(14,
                              "[#%d] Gather data (%d bytes) from #%d",
                              group->my_pe, (int)nlong, pe_cur);

                /* Clean up temporary buffer */
                memset(target_cur, 0, nlong);

                /* Get data from the current peer */
                rc = MCA_SPML_CALL(get((void *)source, nlong, target_cur, pe_cur));

                /* Do reduction operation */
                if (rc == OSHMEM_SUCCESS) {
                    op->o_func.c_fn(target_cur, target, nlong / op->dt_size);
                }
            }

            free(target_cur);
        } else {
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        }
    }

    /* Send result to all PE in group */
    if (rc == OSHMEM_SUCCESS) {
        SCOLL_VERBOSE(14,
                      "[#%d] Broadcast from the root #%d",
                      group->my_pe, PE_root);
        rc = BCAST_FUNC(group,
                PE_root,
                target,
                target,
                nlong,
                (pSync + 1),
                SCOLL_DEFAULT_ALG);
    }

    return rc;
}

static int _algorithm_tournament(struct oshmem_group_t *group,
                                  struct oshmem_op_t *op,
                                  void *target,
                                  const void *source,
                                  size_t nlong,
                                  long *pSync,
                                  void *pWrk)
{
    int rc = OSHMEM_SUCCESS;
    int round = 0;
    int exit_flag = group->proc_count - 1;
    long value = SHMEM_SYNC_INIT;
    int my_id = oshmem_proc_group_find_id(group, group->my_pe);
    int peer_id = 0;
    int peer_pe = 0;
    void *target_cur = NULL;
    int PE_root = oshmem_proc_pe(group->proc_array[0]);

    SCOLL_VERBOSE(12, "[#%d] Reduce algorithm: Tournament", group->my_pe);
    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    /* Set current state as WAIT */
    pSync[0] = SHMEM_SYNC_WAIT;

    target_cur = malloc(nlong);
    if (target_cur) {
        memcpy(target_cur, (void *) source, nlong);
    } else {
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    while (exit_flag && (rc == OSHMEM_SUCCESS)) {
        /* Define a peer for competition */
        peer_id = my_id ^ (1 << round);

        /* Update exit condition and round counter */
        exit_flag >>= 1;
        round++;

        /* Do not have peer for tournament */
        if (peer_id >= group->proc_count)
            continue;

        if (my_id < peer_id) {
            pSync[0] = peer_id;
            value = my_id;

            SCOLL_VERBOSE(14, "[#%d] round = %d wait", group->my_pe, round);
            rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_EQ, (void*)&value, SHMEM_LONG));

            /* Do reduction operation */
            if (rc == OSHMEM_SUCCESS) {
                op->o_func.c_fn(target, target_cur, nlong / op->dt_size);
            }
        } else {
            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

#if 1 /* It is ugly implementation of compare and swap operation 
         Usage of this hack does not give performance improvement but
         it is expected that shmem_long_cswap() will make it faster.
       */
            do {
                MCA_SPML_CALL(get((void*)pSync, sizeof(value), (void*)&value, peer_pe));
            } while (value != my_id);

            SCOLL_VERBOSE(14,
                          "[#%d] round = %d send data to #%d",
                          group->my_pe, round, peer_pe);
            rc = MCA_SPML_CALL(put(target, nlong, target_cur, peer_pe));

            MCA_SPML_CALL(fence());

            SCOLL_VERBOSE(14,
                          "[#%d] round = %d signals to #%d",
                          group->my_pe, round, peer_pe);
            value = peer_id;
            rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
#endif
            SCOLL_VERBOSE(14, "[#%d] round = %d wait", group->my_pe, round);
            value = SHMEM_SYNC_RUN;
            rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_EQ, (void*)&value, SHMEM_LONG));

            break;
        }
    }

    /* Send result to all PE in group */
    if ((my_id == 0) && (rc == OSHMEM_SUCCESS)) {
        SCOLL_VERBOSE(14, "[#%d] signals to all", group->my_pe);

        memcpy(target, target_cur, nlong);

        value = SHMEM_SYNC_RUN;
        for (peer_id = 1;
                (peer_id < group->proc_count) && (rc == OSHMEM_SUCCESS);
                peer_id++) {
            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);
            rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
        }
    }

    /* Send result to all PE in group */
    if (rc == OSHMEM_SUCCESS) {
        SCOLL_VERBOSE(14,
                      "[#%d] Broadcast from the root #%d",
                      group->my_pe, PE_root);
        rc = BCAST_FUNC(group,
                PE_root,
                target,
                target,
                nlong,
                (pSync + 1),
                SCOLL_DEFAULT_ALG);
    }

    free(target_cur);

    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    return rc;
}

static int _algorithm_recursive_doubling(struct oshmem_group_t *group,
                                          struct oshmem_op_t *op,
                                          void *target,
                                          const void *source,
                                          size_t nlong,
                                          long *pSync,
                                          void *pWrk)
{
    int rc = OSHMEM_SUCCESS;
    int round = 0;
    int floor2_proc = 0;
    int exit_flag = 0;
    long value = SHMEM_SYNC_INIT;
    void *target_cur = NULL;
    int my_id = oshmem_proc_group_find_id(group, group->my_pe);
    int peer_id = 0;
    int peer_pe = 0;
    int i = 0;

    floor2_proc = 1;
    i = group->proc_count;
    i >>= 1;
    while (i) {
        i >>= 1;
        floor2_proc <<= 1;
    }

    target_cur = malloc(nlong);
    if (target_cur) {
        memcpy(target_cur, (void *) source, nlong);
    } else {
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    SCOLL_VERBOSE(12,
                  "[#%d] Reduce algorithm: Recursive Doubling",
                  group->my_pe);
    SCOLL_VERBOSE(15,
                  "[#%d] pSync[0] = %ld floor2_proc = %d",
                  group->my_pe, pSync[0], floor2_proc);

    if (my_id >= floor2_proc) {
        /* I am in extra group, my partner is node (my_id-y) in basic group */
        peer_id = my_id - floor2_proc;
        peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

        /* Special procedure is needed in case target and source are the same */
        if (source == target) {
            SCOLL_VERBOSE(14,
                          "[#%d] wait for peer #%d is ready",
                          group->my_pe, peer_pe);
            value = SHMEM_SYNC_WAIT;
            rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_EQ, (void*)&value, SHMEM_LONG));
        }

        SCOLL_VERBOSE(14,
                      "[#%d] is extra send data to #%d",
                      group->my_pe, peer_pe);
        rc = MCA_SPML_CALL(put(target, nlong, target_cur, peer_pe));

        MCA_SPML_CALL(fence());

        SCOLL_VERBOSE(14,
                      "[#%d] is extra and signal to #%d",
                      group->my_pe, peer_pe);
        value = SHMEM_SYNC_RUN;
        rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));

        SCOLL_VERBOSE(14, "[#%d] wait", group->my_pe);
        value = SHMEM_SYNC_RUN;
        rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_EQ, (void*)&value, SHMEM_LONG));
    } else {
        /* Wait for a peer from extra group */
        if ((group->proc_count - floor2_proc) > my_id) {
            /* I am in basic group, my partner is node (my_id+y) in extra group */
            peer_id = my_id + floor2_proc;
            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

            /* Special procedure is needed in case target and source are the same */
            if (source == target) {
                SCOLL_VERBOSE(14,
                              "[#%d] signal to #%d that I am ready",
                              group->my_pe, peer_pe);
                value = SHMEM_SYNC_WAIT;
                rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
            }

            SCOLL_VERBOSE(14,
                          "[#%d] wait a signal from #%d",
                          group->my_pe, peer_pe);
            value = SHMEM_SYNC_RUN;
            rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_EQ, (void*)&value, SHMEM_LONG));

            /* Do reduction operation */
            if (rc == OSHMEM_SUCCESS) {
                op->o_func.c_fn(target, target_cur, nlong / op->dt_size);
            }
        }

        /* Pairwise exchange  */
        exit_flag = floor2_proc - 1;
        pSync[0] = round;
        while (exit_flag && (rc == OSHMEM_SUCCESS)) {
            /* Define a peer for competition */
            peer_id = my_id ^ (1 << round);

            /* Update exit condition and round counter */
            exit_flag >>= 1;
            round++;

            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

#if 1 /* It is ugly implementation of compare and swap operation 
         Usage of this hack does not give performance improvement but
         it is expected that shmem_long_cswap() will make it faster.
       */
            do {
                MCA_SPML_CALL(get((void*)pSync, sizeof(value), (void*)&value, peer_pe));
            } while (value != (round - 1));

            SCOLL_VERBOSE(14,
                          "[#%d] round = %d send data to #%d",
                          group->my_pe, round, peer_pe);
            rc = MCA_SPML_CALL(put(target, nlong, target_cur, peer_pe));

            MCA_SPML_CALL(fence());

            SCOLL_VERBOSE(14,
                          "[#%d] round = %d signals to #%d",
                          group->my_pe, round, peer_pe);
            value = SHMEM_SYNC_RUN;
            rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
#endif

            SCOLL_VERBOSE(14, "[#%d] round = %d wait", group->my_pe, round);
            value = SHMEM_SYNC_RUN;
            rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_EQ, (void*)&value, SHMEM_LONG));

            /* Do reduction operation */
            if (rc == OSHMEM_SUCCESS) {
                op->o_func.c_fn(target, target_cur, nlong / op->dt_size);
            }

            pSync[0] = round;
        }

        memcpy(target, target_cur, nlong);

        /* Notify a peer from extra group */
        if ((group->proc_count - floor2_proc) > my_id) {
            /* I am in basic group, my partner is node (my_id+y) in extra group */
            peer_id = my_id + floor2_proc;
            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

            SCOLL_VERBOSE(14,
                          "[#%d] is extra send data to #%d",
                          group->my_pe, peer_pe);
            rc = MCA_SPML_CALL(put(target, nlong, target_cur, peer_pe));

            MCA_SPML_CALL(fence());

            SCOLL_VERBOSE(14, "[#%d] signals to #%d", group->my_pe, peer_pe);
            value = SHMEM_SYNC_RUN;
            rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
        }
    }

    free(target_cur);

    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    return rc;
}

static int _algorithm_linear(struct oshmem_group_t *group,
                              struct oshmem_op_t *op,
                              void *target,
                              const void *source,
                              size_t nlong,
                              long *pSync,
                              void *pWrk)
{
    int rc = OSHMEM_SUCCESS;
    int i, rank, size;
    char *free_buffer = NULL;
    char *pml_buffer = NULL;
    char *inbuf;
    int peer_id = 0;
    int peer_pe = 0;

    /* Initialize */
    rank = group->my_pe;
    size = group->proc_count;
    int root_id = size - 1;
    int root_pe = oshmem_proc_pe(group->proc_array[root_id]);

    SCOLL_VERBOSE(12, "[#%d] Reduce algorithm: Basic", group->my_pe);

    /* If not root, send data to the root. */

    if (rank != root_pe) {
        rc = MCA_SPML_CALL(send((void*)source, nlong, root_pe, MCA_SPML_BASE_PUT_STANDARD));
    } else {

        /* for reducing buffer allocation lengths.... */

        if (size > 1) {
            free_buffer = (char*) malloc(nlong);
            if (NULL == free_buffer) {
                return OSHMEM_ERR_OUT_OF_RESOURCE;
            }
            pml_buffer = free_buffer;
        }

        /* Initialize the receive buffer. */

        if (root_id == (size - 1)) {
            memcpy(target, (void *) source, nlong);
        } else {
            peer_id = size - 1;
            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);
            rc = MCA_SPML_CALL(recv(target, nlong, peer_pe));
        }
        if (OSHMEM_SUCCESS != rc) {
            if (NULL != free_buffer) {
                free(free_buffer);
            }
            return rc;
        }

        /* Loop receiving and calling reduction function (C or Fortran). */

        for (i = size - 2; i >= 0; --i) {
            if (root_id == i) {
                inbuf = (char*) source;
            } else {
                peer_id = i;
                peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);
                rc = MCA_SPML_CALL(recv(pml_buffer, nlong, peer_pe));
                if (OSHMEM_SUCCESS != rc) {
                    if (NULL != free_buffer) {
                        free(free_buffer);
                    }
                    return rc;
                }

                inbuf = pml_buffer;
            }

            /* Perform the reduction */
            op->o_func.c_fn(inbuf, target, nlong / op->dt_size);
        }

        if (NULL != free_buffer) {
            free(free_buffer);
        }
    }

    /* Send result to all PE in group */
    if (rc == OSHMEM_SUCCESS) {
        SCOLL_VERBOSE(14,
                      "[#%d] Broadcast from the root #%d",
                      group->my_pe, root_pe);
        rc = BCAST_FUNC(group,
                root_pe,
                target,
                target,
                nlong,
                (pSync + 1),
                SCOLL_DEFAULT_ALG);
    }

    /* All done */
    return rc;
}

static int _algorithm_log(struct oshmem_group_t *group,
                           struct oshmem_op_t *op,
                           void *target,
                           const void *source,
                           size_t nlong,
                           long *pSync,
                           void *pWrk)
{
    int rc = OSHMEM_SUCCESS;
    int i, size, rank, vrank;
    int mask;
    void *sbuf = (void*) source;
    void *rbuf = target;
    char *free_buffer = NULL;
    char *free_rbuf = NULL;
    char *pml_buffer = NULL;
    char *snd_buffer = NULL;
    char *rcv_buffer = (char*) rbuf;
    int my_id = oshmem_proc_group_find_id(group, group->my_pe);
    int peer_id = 0;
    int peer_pe = 0;
    int root_id = 0;
    int root_pe = oshmem_proc_pe(group->proc_array[root_id]);
    int dim = 0;

    /* Initialize */
    rank = group->my_pe;
    size = group->proc_count;
    dim = opal_cube_dim(group->proc_count);
    vrank = (my_id + size - root_id) % size;

    SCOLL_VERBOSE(12, "[#%d] Reduce algorithm: Log", rank);

    /* Allocate the incoming and resulting message buffers.  See lengthy
     * rationale above. */

    free_buffer = (char*) malloc(nlong);
    if (NULL == free_buffer) {
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    pml_buffer = free_buffer;
    rcv_buffer = pml_buffer;

    /* Allocate sendbuf in case the MPI_IN_PLACE option has been used. See lengthy
     * rationale above. */

    snd_buffer = (char*) sbuf;

    if (my_id != root_id && 0 == (vrank & 1)) {
        /* root is the only one required to provide a valid rbuf.
         * Assume rbuf is invalid for all other ranks, so fix it up
         * here to be valid on all non-leaf ranks */
        free_rbuf = (char*) malloc(nlong);
        if (NULL == free_rbuf) {
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
            goto cleanup_and_return;
        }
        rbuf = free_rbuf;
    }

    /* Loop over cube dimensions. High processes send to low ones in the
     * dimension. */

    for (i = 0, mask = 1; i < dim; ++i, mask <<= 1) {

        /* A high-proc sends to low-proc and stops. */
        if (vrank & mask) {
            peer_id = vrank & ~mask;
            peer_id = (peer_id + root_id) % size;
            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

            rc = MCA_SPML_CALL(send((void*)snd_buffer, nlong, peer_pe, MCA_SPML_BASE_PUT_STANDARD));
            if (OSHMEM_SUCCESS != rc) {
                goto cleanup_and_return;
            }
            snd_buffer = (char*) rbuf;
            break;
        }

        /* A low-proc receives, reduces, and moves to a higher
         * dimension. */

        else {
            peer_id = vrank | mask;
            if (peer_id >= size) {
                continue;
            }
            peer_id = (peer_id + root_id) % size;
            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

            /* Most of the time (all except the first one for commutative
             * operations) we receive in the user provided buffer
             * (rbuf). But the exception is here to allow us to dont have
             * to copy from the sbuf to a temporary location. If the
             * operation is commutative we dont care in which order we
             * apply the operation, so for the first time we can receive
             * the data in the pml_buffer and then apply to operation
             * between this buffer and the user provided data. */

            rc = MCA_SPML_CALL(recv(rcv_buffer, nlong, peer_pe));
            if (OSHMEM_SUCCESS != rc) {
                goto cleanup_and_return;
            }
            /* Perform the operation. The target is always the user
             * provided buffer We do the operation only if we receive it
             * not in the user buffer */
            if (snd_buffer != sbuf) {
                /* the target buffer is the locally allocated one */
                op->o_func.c_fn(rcv_buffer, pml_buffer, nlong / op->dt_size);
            } else {
                /* If we're commutative, we don't care about the order of
                 * operations and we can just reduce the operations now.
                 * If we are not commutative, we have to copy the send
                 * buffer into a temp buffer (pml_buffer) and then reduce
                 * what we just received against it. */
                {
                    op->o_func.c_fn(sbuf, pml_buffer, nlong / op->dt_size);
                }
                /* now we have to send the buffer containing the computed data */
                snd_buffer = pml_buffer;
                /* starting from now we always receive in the user
                 * provided buffer */
                rcv_buffer = (char*) rbuf;
            }
        }
    }

    /* Get the result to the root if needed. */
    rc = OSHMEM_SUCCESS;
    if (0 == vrank) {
        if (root_id == my_id) {
            memcpy(rbuf, snd_buffer, nlong);
        } else {
            rc = MCA_SPML_CALL(send((void*)snd_buffer, nlong, root_pe, MCA_SPML_BASE_PUT_STANDARD));
        }
    } else if (my_id == root_id) {
        rc = MCA_SPML_CALL(recv(rcv_buffer, nlong, root_pe));
        if (rcv_buffer != rbuf) {
            op->o_func.c_fn(rcv_buffer, rbuf, nlong / op->dt_size);
        }
    }

    cleanup_and_return: if (NULL != free_buffer) {
        free(free_buffer);
    }
    if (NULL != free_rbuf) {
        free(free_rbuf);
    }

    /* Send result to all PE in group */
    if (rc == OSHMEM_SUCCESS) {
        SCOLL_VERBOSE(14,
                      "[#%d] Broadcast from the root #%d",
                      rank, root_pe);
        rc = BCAST_FUNC(group,
                root_pe,
                target,
                target,
                nlong,
                (pSync + 1),
                SCOLL_DEFAULT_ALG);
    }

    /* All done */
    return rc;
}
