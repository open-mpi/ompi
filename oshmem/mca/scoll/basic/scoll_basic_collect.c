/*
 * Copyright (c) 2013-2015 Mellanox Technologies, Inc.
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

#include "oshmem/constants.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"
#include "scoll_basic.h"

static int _algorithm_central_collector(struct oshmem_group_t *group,
                                         void *target,
                                         const void *source,
                                         size_t nlong,
                                         long *pSync);
static int _algorithm_f_central_counter(struct oshmem_group_t *group,
                                         void *target,
                                         const void *source,
                                         size_t nlong,
                                         long *pSync);
static int _algorithm_f_tournament(struct oshmem_group_t *group,
                                    void *target,
                                    const void *source,
                                    size_t nlong,
                                    long *pSync);
static int _algorithm_f_recursive_doubling(struct oshmem_group_t *group,
                                            void *target,
                                            const void *source,
                                            size_t nlong,
                                            long *pSync);
static int _algorithm_f_ring(struct oshmem_group_t *group,
                              void *target,
                              const void *source,
                              size_t nlong,
                              long *pSync);

int mca_scoll_basic_collect(struct oshmem_group_t *group,
                            void *target,
                            const void *source,
                            size_t nlong,
                            long *pSync,
                            bool nlong_type,
                            int alg)
{
    int rc = OSHMEM_SUCCESS;

    /* Arguments validation */
    if (!group || !pSync) {
        SCOLL_ERROR("Active set (group) of PE is not defined");
        rc = OSHMEM_ERR_BAD_PARAM;
    }

    /* Check if this PE is part of the group */
    if ((rc == OSHMEM_SUCCESS) && oshmem_proc_group_is_member(group)) {
        int i = 0;

        if (nlong_type) {
            alg = (alg == SCOLL_DEFAULT_ALG ?
                    mca_scoll_basic_param_collect_algorithm : alg);
            switch (alg) {
            case SCOLL_ALG_COLLECT_CENTRAL_COUNTER:
                {
                    rc = _algorithm_f_central_counter(group,
                                                       target,
                                                       source,
                                                       nlong,
                                                       pSync);
                    break;
                }
            case SCOLL_ALG_COLLECT_TOURNAMENT:
                {
                    rc = _algorithm_f_tournament(group,
                                                  target,
                                                  source,
                                                  nlong,
                                                  pSync);
                    break;
                }
            case SCOLL_ALG_COLLECT_RECURSIVE_DOUBLING:
                {
                    rc = _algorithm_f_recursive_doubling(group,
                                                          target,
                                                          source,
                                                          nlong,
                                                          pSync);
                    break;
                }
            case SCOLL_ALG_COLLECT_RING:
                {
                    rc = _algorithm_f_ring(group,
                                            target,
                                            source,
                                            nlong,
                                            pSync);
                    break;
                }
            default:
                {
                    rc = _algorithm_f_central_counter(group,
                                                       target,
                                                       source,
                                                       nlong,
                                                       pSync);
                }
            }
        } else {
            rc = _algorithm_central_collector(group,
                                               target,
                                               source,
                                               nlong,
                                               pSync);
        }

        /* Restore initial values */
        SCOLL_VERBOSE(12,
                      "[#%d] Restore special synchronization array",
                      group->my_pe);
        for (i = 0; i < _SHMEM_COLLECT_SYNC_SIZE; i++) {
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
static int _algorithm_f_central_counter(struct oshmem_group_t *group,
                                         void *target,
                                         const void *source,
                                         size_t nlong,
                                         long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    int i = 0;
    int PE_root = oshmem_proc_pe(group->proc_array[0]);

    SCOLL_VERBOSE(12,
                  "[#%d] Collect algorithm: Central Counter (identical size)",
                  group->my_pe);
    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    if (PE_root == group->my_pe) {
        int pe_cur = 0;

        memcpy((void*) ((unsigned char*) target + 0 * nlong),
               (void *) source,
               nlong);

        SCOLL_VERBOSE(14,
                      "[#%d] Gather data from all PEs in the group",
                      group->my_pe);
        for (i = 0; (i < group->proc_count) && (rc == OSHMEM_SUCCESS); i++) {
            /* Get PE ID of a peer from the group */
            pe_cur = oshmem_proc_pe(group->proc_array[i]);

            if (pe_cur == group->my_pe)
                continue;

            SCOLL_VERBOSE(14,
                          "[#%d] Gather data (%d bytes) from #%d",
                          group->my_pe, (int)nlong, pe_cur);

            /* Get data from the current peer */
            rc = MCA_SPML_CALL(get((void *)source, nlong, (void*)((unsigned char*)target + i * nlong), pe_cur));
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
                    group->proc_count * nlong,
                    (pSync + 1),
                    SCOLL_DEFAULT_ALG);
    }

    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    return rc;
}

static int _algorithm_f_tournament(struct oshmem_group_t *group,
                                    void *target,
                                    const void *source,
                                    size_t nlong,
                                    long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    int round = 0;
    int exit_flag = group->proc_count - 1;
    long value = SHMEM_SYNC_INIT;
    int my_id = oshmem_proc_group_find_id(group, group->my_pe);
    int peer_id = 0;
    int peer_pe = 0;
    int PE_root = oshmem_proc_pe(group->proc_array[0]);

    SCOLL_VERBOSE(12,
                  "[#%d] Collect algorithm: Tournament (identical size)",
                  group->my_pe);
    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    /* Set current state as WAIT */
    pSync[0] = SHMEM_SYNC_WAIT;

    /* Copy data to itself */
    memcpy((void*) ((unsigned char*) target + my_id * nlong),
           (void *) source,
           nlong);

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
            rc = MCA_SPML_CALL(put((void*)((unsigned char*)target + my_id * nlong), (1 << (round - 1)) * nlong, (void*)((unsigned char*)target + my_id * nlong), peer_pe));

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
                group->proc_count * nlong,
                (pSync + 1),
                SCOLL_DEFAULT_ALG);
    }

    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    return rc;
}

static int _algorithm_f_ring(struct oshmem_group_t *group,
                              void *target,
                              const void *source,
                              size_t nlong,
                              long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    int i = 0;
    long value = SHMEM_SYNC_INIT;
    int my_id = oshmem_proc_group_find_id(group, group->my_pe);
    int data_index = 0;
    int peer_id = 0;
    int peer_pe = 0;

    SCOLL_VERBOSE(12,
                  "[#%d] Collect algorithm: Ring (identical size)",
                  group->my_pe);
    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    peer_id = (my_id + 1) % group->proc_count;
    peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);
    memcpy((void*) ((unsigned char*) target + my_id * nlong),
           (void *) source,
           nlong);
    data_index = my_id;

    for (i = 0; (i < (group->proc_count - 1)) && (rc == OSHMEM_SUCCESS); i++) {
        SCOLL_VERBOSE(14,
                      "[#%d] round = %d send data to #%d by index = %d",
                      group->my_pe, i, peer_pe, data_index);
        rc = MCA_SPML_CALL(put((void*)((unsigned char*)target + data_index * nlong), nlong, (void*)((unsigned char*)target + data_index * nlong), peer_pe));

        MCA_SPML_CALL(fence());

        SCOLL_VERBOSE(14,
                      "[#%d] round = %d signals to #%d",
                      group->my_pe, i, peer_pe);
        value = i;
        rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));

        data_index = (data_index ? (data_index - 1) : (group->proc_count - 1));

        SCOLL_VERBOSE(14,
                      "[#%d] round = %d wait for data by index = %d",
                      group->my_pe, i, data_index);
        if (i == 0) {
            value = _SHMEM_SYNC_VALUE;
            rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_NE, (void*)&value, SHMEM_LONG));
        } else {
            value = i;
            rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_GE, (void*)&value, SHMEM_LONG));
        }
    }

    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    return rc;
}

static int _algorithm_f_recursive_doubling(struct oshmem_group_t *group,
                                            void *target,
                                            const void *source,
                                            size_t nlong,
                                            long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    int round = 0;
    int floor2_proc = 0;
    int exit_flag = 0;
    long value = SHMEM_SYNC_INIT;
    int my_id = oshmem_proc_group_find_id(group, group->my_pe);
    int data_index = 0;
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

    SCOLL_VERBOSE(12,
                  "[#%d] Collect algorithm: Recursive Doubling (identical size)",
                  group->my_pe);
    SCOLL_VERBOSE(15,
                  "[#%d] pSync[0] = %ld floor2_proc = %d",
                  group->my_pe, pSync[0], floor2_proc);

    memcpy((void*) ((unsigned char*) target + my_id * nlong),
           (void *) source,
           nlong);
    data_index = my_id;

    if (my_id >= floor2_proc) {
        int pe_cur = 0;

        /* I am in extra group, my partner is node (my_id-y) in basic group */
        peer_id = my_id - floor2_proc;
        peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

        for (i = 0; (i < group->proc_count) && (rc == OSHMEM_SUCCESS); i++) {
            if (i == my_id)
                continue;

            pe_cur = oshmem_proc_pe(group->proc_array[i]);

            SCOLL_VERBOSE(14,
                          "[#%d] is extra send data to #%d",
                          group->my_pe, pe_cur);
            rc = MCA_SPML_CALL(put((void*)((unsigned char*)target + data_index * nlong), nlong, (void *)source, pe_cur));
        }

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

            SCOLL_VERBOSE(14,
                          "[#%d] wait a signal from #%d",
                          group->my_pe, peer_pe);
            value = SHMEM_SYNC_RUN;
            rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_EQ, (void*)&value, SHMEM_LONG));
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
                          "[#%d] round = %d send data to #%d by index = %d",
                          group->my_pe, round, peer_pe, data_index);
            rc = MCA_SPML_CALL(put((void*)((unsigned char*)target + data_index * nlong), (1 << (round - 1)) * nlong, (void*)((unsigned char*)target + data_index * nlong), peer_pe));

            MCA_SPML_CALL(fence());

            data_index = (my_id / (1 << round)) * (1 << round);

            SCOLL_VERBOSE(14,
                          "[#%d] round = %d signals to #%d",
                          group->my_pe, round, peer_pe);
            value = SHMEM_SYNC_RUN;
            rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
#endif

            SCOLL_VERBOSE(14, "[#%d] round = %d wait", group->my_pe, round);
            value = SHMEM_SYNC_RUN;
            rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_EQ, (void*)&value, SHMEM_LONG));

            pSync[0] = round;
        }

        /* Notify a peer from extra group */
        if ((group->proc_count - floor2_proc) > my_id) {
            /* I am in basic group, my partner is node (my_id+y) in extra group */
            peer_id = my_id + floor2_proc;
            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

            SCOLL_VERBOSE(14,
                          "[#%d] is extra send data to #%d",
                          group->my_pe, peer_pe);
            rc = MCA_SPML_CALL(put(target, group->proc_count * nlong, target, peer_pe));

            MCA_SPML_CALL(fence());

            SCOLL_VERBOSE(14, "[#%d] signals to #%d", group->my_pe, peer_pe);
            value = SHMEM_SYNC_RUN;
            rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
        }
    }

    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    return rc;
}

/*
 This algorithm is quite simple and straightforward. It allows to have different data size on PEs.
 One node gathers data from peers and send final result to them.
 Outlay:
 NP-1 competing network transfers are needed.
 */
static int _algorithm_central_collector(struct oshmem_group_t *group,
                                         void *target,
                                         const void *source,
                                         size_t nlong,
                                         long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    size_t offset = 0;
    int i = 0;
    int PE_root = oshmem_proc_pe(group->proc_array[0]);

    SCOLL_VERBOSE(12,
                  "[#%d] Collect algorithm: Central Counter (vary size)",
                  group->my_pe);

    /* Set own data size */
    pSync[0] = (nlong ? (long)nlong : SHMEM_SYNC_READY);

    if (PE_root == group->my_pe) {
        long value = 0;
        int pe_cur = 0;
        long wait_pe_count = 0;
        long* wait_pe_array = NULL;

        wait_pe_count = group->proc_count;
        wait_pe_array = malloc(sizeof(*wait_pe_array) * wait_pe_count);
        if (wait_pe_array) {
            memset((void*) wait_pe_array,
                   0,
                   sizeof(*wait_pe_array) * wait_pe_count);
            wait_pe_array[0] = nlong;
            wait_pe_count--;

            while (wait_pe_count) {
                SCOLL_VERBOSE(14,
                              "[#%d] Gather data size info from all PEs in the group",
                              group->my_pe);
                for (i = 1; (i < group->proc_count) && (rc == OSHMEM_SUCCESS);
                        i++) {
                    if (wait_pe_array[i] == 0) {
                        pe_cur = oshmem_proc_pe(group->proc_array[i]);
                        value = 0;
                        rc = MCA_SPML_CALL(get((void*)pSync, sizeof(value), (void*)&value, pe_cur));
                        if ((rc == OSHMEM_SUCCESS)
                                && (value != _SHMEM_SYNC_VALUE)) {
                            wait_pe_array[i] = value;
                            wait_pe_count--;
                            SCOLL_VERBOSE(14,
                                          "Got source data size as %d from #%d (wait list counter: %d)",
                                          (int)value, pe_cur, (int)wait_pe_count);
                        }
                    }
                }
            }

            memcpy((void*) ((unsigned char*) target + 0 * nlong),
                   (void *) source,
                   nlong);
            offset += nlong;

            for (i = 1; (i < group->proc_count) && (rc == OSHMEM_SUCCESS);
                    i++) {

                /* Skip zero size data */
                if (wait_pe_array[i] == SHMEM_SYNC_READY) {
                    continue;
                }

                /* Get PE ID of a peer from the group */
                pe_cur = oshmem_proc_pe(group->proc_array[i]);

                /* Get data from the current peer */
                rc = MCA_SPML_CALL(get((void *)source, (size_t)wait_pe_array[i], (void*)((unsigned char*)target + offset), pe_cur));

                SCOLL_VERBOSE(14,
                              "Got %d bytes of data from #%d (offset: %d)",
                              (int)wait_pe_array[i], pe_cur, (int)offset);

                offset += (size_t)wait_pe_array[i];
            }

            free(wait_pe_array);
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
                offset,
                (pSync + 1),
                SCOLL_DEFAULT_ALG);
    }

    return rc;
}
