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
#include "oshmem/proc/proc.h"
#include "scoll_basic.h"

static int _algorithm_central_counter(struct oshmem_group_t *group,
                                       long *pSync);
static int _algorithm_tournament(struct oshmem_group_t *group, long *pSync);
static int _algorithm_recursive_doubling(struct oshmem_group_t *group,
                                          long *pSync);
static int _algorithm_dissemination(struct oshmem_group_t *group, long *pSync);
static int _algorithm_basic(struct oshmem_group_t *group, long *pSync);
static int _algorithm_adaptive(struct oshmem_group_t *group, long *pSync);

int mca_scoll_basic_barrier(struct oshmem_group_t *group, long *pSync, int alg)
{
    int rc = OSHMEM_SUCCESS;

    /* Arguments validation */
    if (!group || !pSync) {
        SCOLL_ERROR("Active set (group) of PE is not defined");
        rc = OSHMEM_ERR_BAD_PARAM;
    }

    if ((rc == OSHMEM_SUCCESS) && oshmem_proc_group_is_member(group)) {
        if (pSync) {
            alg = (alg == SCOLL_DEFAULT_ALG ?
                    mca_scoll_basic_param_barrier_algorithm : alg);
            switch (alg) {
            case SCOLL_ALG_BARRIER_CENTRAL_COUNTER:
                {
                    rc = _algorithm_central_counter(group, pSync);
                    break;
                }
            case SCOLL_ALG_BARRIER_TOURNAMENT:
                {
                    rc = _algorithm_tournament(group, pSync);
                    break;
                }
            case SCOLL_ALG_BARRIER_RECURSIVE_DOUBLING:
                {
                    rc = _algorithm_recursive_doubling(group, pSync);
                    break;
                }
            case SCOLL_ALG_BARRIER_DISSEMINATION:
                {
                    rc = _algorithm_dissemination(group, pSync);
                    break;
                }
            case SCOLL_ALG_BARRIER_BASIC:
                {
                    rc = _algorithm_basic(group, pSync);
                    break;
                }
            case SCOLL_ALG_BARRIER_ADAPTIVE:
                {
                    rc = _algorithm_adaptive(group, pSync);
                    break;
                }
            default:
                {
                    rc = _algorithm_recursive_doubling(group, pSync);
                }
            }
        } else {
            SCOLL_ERROR("Incorrect argument pSync");
            rc = OSHMEM_ERR_BAD_PARAM;
        }
    }

    return rc;
}

/*
 This algorithm is quite simple and straightforward. But because of it�s obvious simplicity and
 the naive prove for correctness it is implemented quite often. One node asks peers if they are
 achieve barrier state. When all processors are ready it signals to go ahead.
 Outlay:
 NP-1 competing network transfers are needed to implement the counter
 The memory usage is constant (1 byte) per node.
 */
static int _algorithm_central_counter(struct oshmem_group_t *group,
                                       long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    long value = SHMEM_SYNC_INIT;
    int root_id = 0;
    int PE_root = oshmem_proc_pe(group->proc_array[root_id]);
    int i = 0;

    SCOLL_VERBOSE(12, "[#%d] Barrier algorithm: Central Counter", group->my_pe);
    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    /* Set current state as WAIT */
    pSync[0] = SHMEM_SYNC_WAIT;

    /* Root processes synchronization */
    if (PE_root == group->my_pe) {
        int pe_cur = 0;
        long wait_pe_count = 0;
        int* wait_pe_array = NULL;

        wait_pe_array = malloc(sizeof(*wait_pe_array) * group->proc_count);
        if (wait_pe_array) {
            SCOLL_VERBOSE(14, "[#%d] PE is the root", group->my_pe);

            wait_pe_count = group->proc_count;
            for (i = 0; i < group->proc_count; i++) {
                wait_pe_array[i] = oshmem_proc_pe(group->proc_array[i]);
            }
            wait_pe_array[root_id] = OSHMEM_PE_INVALID;
            wait_pe_count--;

            while (wait_pe_count) {
                for (i = 0; (i < group->proc_count) && (rc == OSHMEM_SUCCESS);
                        i++) {
                    pe_cur = wait_pe_array[i];
                    if (pe_cur != OSHMEM_PE_INVALID) {
                        rc = MCA_SPML_CALL(get((void*)pSync, sizeof(value), (void*)&value, pe_cur));
                        if ((rc == OSHMEM_SUCCESS)
                                && (value == SHMEM_SYNC_WAIT)) {
                            wait_pe_array[i] = OSHMEM_PE_INVALID;
                            wait_pe_count--;
                            SCOLL_VERBOSE(14,
                                          "[#%d] PE#%d is ready (wait list counter: %d)",
                                          group->my_pe, pe_cur, (int)wait_pe_count);
                        }
                    }
                }
            }

            SCOLL_VERBOSE(14, "[#%d] PE signals to all", group->my_pe);
            value = SHMEM_SYNC_RUN;
            for (i = 0; (i < group->proc_count) && (rc == OSHMEM_SUCCESS);
                    i++) {
                pe_cur = oshmem_proc_pe(group->proc_array[i]);
                if (pe_cur != PE_root) {
                    rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, pe_cur));
                }
            }

            free(wait_pe_array);
        } else {
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        }

        /* Possibly this is unnecessary...
         But imagine the scenario when you have 2 sequential barriers and the root PE is the fastest one.
         The root could leave the first barrier and in the second barrier it could get SHMEM_SYNC_WAIT value on
         remote node before the remote node receives its SHMEM_SYNC_RUN value in the first barrier
         */
        /* TODO: actually it must be quiet */
        MCA_SPML_CALL(fence());
    }
    /* Wait for RUN signal */
    else {
        SCOLL_VERBOSE(14,
                      "[#%d] PE waits for a signal from root",
                      group->my_pe);

        value = SHMEM_SYNC_RUN;
        rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_EQ, (void*)&value, SHMEM_LONG));
    }

    /* Restore initial values */
    SCOLL_VERBOSE(12,
                  "[#%d] Restore special synchronization array",
                  group->my_pe);
    for (i = 0; i < _SHMEM_BARRIER_SYNC_SIZE; i++) {
        pSync[i] = _SHMEM_SYNC_VALUE;
    }

    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    return rc;
}

/*
 The Tournament Barrier, proposed by Hengsen, Finkel and Manser is mostly suitable for shared memory
 multiprocessors because it benefits from several caching mechanisms.
 The algorithm is similar to a tournament game. In each round two
 nodes play against each other. The winner is known in advance and waits until the looser arrives. The
 winners play against each other in the next round. The overall winner (the champion) notifies all others
 about the end of the barrier.
 Outlay:
 The game scales with log2(NP) and uses 1 byte of memory.
 */
static int _algorithm_tournament(struct oshmem_group_t *group, long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    int round = 0;
    int exit_flag = group->proc_count - 1;
    long value = SHMEM_SYNC_INIT;
    int my_id = oshmem_proc_group_find_id(group, group->my_pe);
    int peer_id = 0;
    int peer_pe = 0;
    int i = 0;

    SCOLL_VERBOSE(12, "[#%d] Barrier algorithm: Tournament", group->my_pe);
    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    /* Set current state as WAIT */
    pSync[0] = SHMEM_SYNC_WAIT;

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
                          "[#%d] round = %d signals to #%d",
                          group->my_pe, round, peer_pe);
            value = peer_id;
            rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
#else
            SCOLL_VERBOSE(14, "[#%d] round = %d signals to #%d", group->my_pe, round, peer_pe);
            do
            {
                rc = MCA_ATOMIC_CALL(cswap((void*)pSync, (void*)&value, (const void*)&my_id, (const void*)&peer_id, sizeof(value), peer_pe));
            }while (value != my_id);
#endif
            SCOLL_VERBOSE(14, "[#%d] round = %d wait", group->my_pe, round);
            value = SHMEM_SYNC_RUN;
            rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_EQ, (void*)&value, SHMEM_LONG));

            break;
        }
    }

    /* Restore initial values */
    SCOLL_VERBOSE(12,
                  "[#%d] Restore special synchronization array",
                  group->my_pe);
    for (i = 0; i < _SHMEM_BARRIER_SYNC_SIZE; i++) {
        pSync[i] = _SHMEM_SYNC_VALUE;
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

    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    return rc;
}

/*
 Pairwise Exchange With Recursive Doubling.
 Rinka Gupta, Vinod Tipparaju, Jare Nieplocha, and Dhabaleswar Panda. Efficient Barrier
 using Remote Memory Operations on VIA-Based Clusters. In 2002 IEEE International
 Conference on Cluster Computing (CLUSTER 2002), page 83. IEEE Computer Society, 2002.
 Outlay:
 The algorithm uses a maximum of log2(NP) + 2 network writes and P bytes memory per node.
 */
static int _algorithm_recursive_doubling(struct oshmem_group_t *group,
                                          long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    int round = 0;
    int floor2_proc = 0;
    int exit_flag = 0;
    long value = SHMEM_SYNC_INIT;
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

    SCOLL_VERBOSE(12,
                  "[#%d] Barrier algorithm: Recursive Doubling",
                  group->my_pe);
    SCOLL_VERBOSE(15,
                  "[#%d] pSync[0] = %ld floor2_proc = %d",
                  group->my_pe, pSync[0], floor2_proc);

    if (my_id >= floor2_proc) {
        /* I am in extra group, my partner is node (my_id-y) in basic group */
        peer_id = my_id - floor2_proc;
        peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

        SCOLL_VERBOSE(14,
                      "[#%d] is extra and signal to #%d",
                      group->my_pe, peer_pe);
        value = SHMEM_SYNC_WAIT;
        rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));

        SCOLL_VERBOSE(14, "[#%d] wait", group->my_pe);
        value = SHMEM_SYNC_RUN;
        rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_EQ, (void*)&value, SHMEM_LONG));

        /* Restore initial values */
        SCOLL_VERBOSE(12,
                      "[#%d] Restore special synchronization array",
                      group->my_pe);
        for (i = 0; i < _SHMEM_BARRIER_SYNC_SIZE; i++) {
            pSync[i] = _SHMEM_SYNC_VALUE;
        }
    } else {
        /* Wait for a peer from extra group */
        if ((group->proc_count - floor2_proc) > my_id) {
            /* I am in basic group, my partner is node (my_id+y) in extra group */
            peer_id = my_id + floor2_proc;
            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

            SCOLL_VERBOSE(14,
                          "[#%d] wait a signal from #%d",
                          group->my_pe, peer_pe);
            value = SHMEM_SYNC_WAIT;
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
                          "[#%d] round = %d signals to #%d",
                          group->my_pe, round, peer_pe);
            value = round;
            rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
#else
            SCOLL_VERBOSE(14, "[#%d] round = %d signals to #%d", group->my_pe, round, peer_pe);
            {
                long cond = round - 1;
                do
                {
                    rc = MCA_ATOMIC_CALL(cswap((void*)pSync, (void*)&value, (const void*)&cond, (const void*)&round, sizeof(value), peer_pe));
                }while (value != (round-1));
            }
#endif

            SCOLL_VERBOSE(14, "[#%d] round = %d wait", group->my_pe, round);
            value = round;
            rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_GE, (void*)&value, SHMEM_LONG));
        }

        /* Restore initial values */
        SCOLL_VERBOSE(12,
                      "[#%d] Restore special synchronization array",
                      group->my_pe);
        for (i = 0; i < _SHMEM_BARRIER_SYNC_SIZE; i++) {
            pSync[i] = _SHMEM_SYNC_VALUE;
        }

        /* Notify a peer from extra group */
        if ((group->proc_count - floor2_proc) > my_id) {
            /* I am in basic group, my partner is node (my_id+y) in extra group */
            peer_id = my_id + floor2_proc;
            peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

            SCOLL_VERBOSE(14, "[#%d] signals to #%d", group->my_pe, peer_pe);
            value = SHMEM_SYNC_RUN;
            rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
        }
    }

    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    return rc;
}

/*
 The Dissemination Barrier, introduced by Hengsen, Finkel and Manser in 1998.
 The algorithm is mostly an improvement of the Butterfly Barrier for non power of two processor counts.
 It uses the same pairwise synchronization but with other partners.
 Outlay:
 The game scales with log2(NP) and uses 1 byte of memory.
 */
static int _algorithm_dissemination(struct oshmem_group_t *group, long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    int round = 0;
    int log2_proc = 0;
    long value = SHMEM_SYNC_INIT;
    int my_id = oshmem_proc_group_find_id(group, group->my_pe);
    int peer_id = 0;
    int peer_pe = 0;
    int i = 0;

    log2_proc = scoll_log2((unsigned long) group->proc_count);

    SCOLL_VERBOSE(12, "[#%d] Barrier algorithm: Dissemination", group->my_pe);
    SCOLL_VERBOSE(15,
                  "[#%d] pSync[0] = %ld floor2_proc = %d",
                  group->my_pe, pSync[0], log2_proc);

    pSync[0] = round;
    for (round = 0; (round <= log2_proc) && (rc == OSHMEM_SUCCESS); round++) {
        /* Define a peer to send signal */
        peer_id = (my_id + (1 << round)) % group->proc_count;

        peer_pe = oshmem_proc_pe(group->proc_array[peer_id]);

#if 1 /* It is ugly implementation of compare and swap operation 
         Usage of this hack does not give performance improvement but
         it is expected that shmem_long_cswap() will make it faster.
       */
        do {
            MCA_SPML_CALL(get((void*)pSync, sizeof(value), (void*)&value, peer_pe));
        } while (value != round);

        SCOLL_VERBOSE(14,
                      "[#%d] round = %d signals to #%d",
                      group->my_pe, round, peer_pe);
        value = round + 1;
        rc = MCA_SPML_CALL(put((void*)pSync, sizeof(value), (void*)&value, peer_pe));
#endif

        SCOLL_VERBOSE(14, "[#%d] round = %d wait", group->my_pe, round);
        value = round + 1;
        rc = MCA_SPML_CALL(wait((void*)pSync, SHMEM_CMP_GE, (void*)&value, SHMEM_LONG));
    }

    /* Restore initial values */
    SCOLL_VERBOSE(12,
                  "[#%d] Restore special synchronization array",
                  group->my_pe);
    for (i = 0; i < _SHMEM_BARRIER_SYNC_SIZE; i++) {
        pSync[i] = _SHMEM_SYNC_VALUE;
    }

    SCOLL_VERBOSE(15, "[#%d] pSync[0] = %ld", group->my_pe, pSync[0]);

    return rc;
}

static int _algorithm_basic(struct oshmem_group_t *group, long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    int root_id = 0;
    int PE_root = oshmem_proc_pe(group->proc_array[root_id]);
    int i = 0;

    SCOLL_VERBOSE(12, "[#%d] Barrier algorithm: Basic", group->my_pe);

    if (PE_root != group->my_pe) {
        rc = MCA_SPML_CALL(send(NULL, 0, PE_root, MCA_SPML_BASE_PUT_STANDARD));
        if (OSHMEM_SUCCESS != rc) {
            return rc;
        }

        rc = MCA_SPML_CALL(recv(NULL, 0, PE_root));
        if (OSHMEM_SUCCESS != rc) {
            return rc;
        }
    }

    /* The root collects and broadcasts the messages. */

    else {
        int pe_cur = 0;

        for (i = 0; (i < group->proc_count) && (rc == OSHMEM_SUCCESS); i++) {
            pe_cur = oshmem_proc_pe(group->proc_array[i]);
            if (pe_cur != PE_root) {
                rc = MCA_SPML_CALL(recv(NULL, 0, SHMEM_ANY_SOURCE));
            }
            if (OSHMEM_SUCCESS != rc) {
                return rc;
            }
        }

        for (i = 0; (i < group->proc_count) && (rc == OSHMEM_SUCCESS); i++) {
            pe_cur = oshmem_proc_pe(group->proc_array[i]);
            if (pe_cur != PE_root) {
                rc = MCA_SPML_CALL(send(NULL, 0, pe_cur, MCA_SPML_BASE_PUT_STANDARD));
            }
            if (OSHMEM_SUCCESS != rc) {
                return rc;
            }
        }
    }

    return rc;
}

static int _algorithm_adaptive(struct oshmem_group_t *group, long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    bool local_peers_only = true;

    SCOLL_VERBOSE(12, "[#%d] Barrier algorithm: Adaptive", group->my_pe);

    /* check if we have only local peers */
    {
        int i = 0;

        for (i = 0; i < group->proc_count; i++) {
            if (i == group->id)
                continue;

            if (!OPAL_PROC_ON_LOCAL_NODE(group->proc_array[i]->super.proc_flags)) {
                local_peers_only = false;
                break;
            }
        }
    }

    /* Select algorithm we use:
     * use send/recv way for group in the same node and for np < 32
     * otherwise use put/get way
     */
    if (local_peers_only || (group->proc_count < 32)) {
        rc = _algorithm_basic(group, pSync);
    } else {
        rc = _algorithm_recursive_doubling(group, pSync);
    }

    return rc;
}
