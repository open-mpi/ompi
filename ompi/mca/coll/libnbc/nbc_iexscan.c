/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "opal/align.h"
#include "ompi/op/op.h"

#include "nbc_internal.h"

static inline int exscan_sched_linear(
    int rank, int comm_size, const void *sendbuf, void *recvbuf, int count,
    MPI_Datatype datatype,  MPI_Op op, char inplace, NBC_Schedule *schedule,
    void *tmpbuf);
static inline int exscan_sched_recursivedoubling(
    int rank, int comm_size, const void *sendbuf, void *recvbuf,
    int count, MPI_Datatype datatype,  MPI_Op op, char inplace,
    NBC_Schedule *schedule, void *tmpbuf1, void *tmpbuf2);

#ifdef NBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int NBC_Scan_args_compare(NBC_Scan_args *a, NBC_Scan_args *b, void *param) {
    if ((a->sendbuf == b->sendbuf) &&
        (a->recvbuf == b->recvbuf) &&
        (a->count == b->count) &&
        (a->datatype == b->datatype) &&
        (a->op == b->op) ) {
        return 0;
    }

    if( a->sendbuf < b->sendbuf ) {
        return -1;
    }

    return 1;
}
#endif

static int nbc_exscan_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                           struct ompi_communicator_t *comm, ompi_request_t ** request,
                           mca_coll_base_module_t *module, bool persistent) {
    int rank, p, res;
    NBC_Schedule *schedule;
    char inplace;
    void *tmpbuf = NULL, *tmpbuf1 = NULL, *tmpbuf2 = NULL;
    enum { NBC_EXSCAN_LINEAR, NBC_EXSCAN_RDBL } alg;
    ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;
    ptrdiff_t span, gap;

    NBC_IN_PLACE(sendbuf, recvbuf, inplace);

    rank = ompi_comm_rank(comm);
    p = ompi_comm_size(comm);

    if (p < 2) {
        return nbc_get_noop_request(persistent, request);
    }

    span = opal_datatype_span(&datatype->super, count, &gap);
    if (libnbc_iexscan_algorithm == 2) {
        alg = NBC_EXSCAN_RDBL;
        ptrdiff_t span_align = OPAL_ALIGN(span, datatype->super.align, ptrdiff_t);
        tmpbuf = malloc(span_align + span);
        if (NULL == tmpbuf) { return OMPI_ERR_OUT_OF_RESOURCE; }
        tmpbuf1 = (void *)(-gap);
        tmpbuf2 = (char *)(span_align) - gap;
    } else {
        alg = NBC_EXSCAN_LINEAR;
        if (rank > 0) {
            tmpbuf = malloc(span);
            if (NULL == tmpbuf) { return OMPI_ERR_OUT_OF_RESOURCE; }
        }
    }

#ifdef NBC_CACHE_SCHEDULE
    NBC_Scan_args *args, *found, search;
    /* search schedule in communicator specific tree */
    search.sendbuf = sendbuf;
    search.recvbuf = recvbuf;
    search.count = count;
    search.datatype = datatype;
    search.op = op;
    found = (NBC_Scan_args *) hb_tree_search ((hb_tree *) libnbc_module->NBC_Dict[NBC_EXSCAN], &search);
    if (NULL == found) {
#endif
    schedule = OBJ_NEW(NBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
        free(tmpbuf);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (alg == NBC_EXSCAN_LINEAR) {
        res = exscan_sched_linear(rank, p, sendbuf, recvbuf, count, datatype,
                                  op, inplace, schedule, tmpbuf);
    } else {
        res = exscan_sched_recursivedoubling(rank, p, sendbuf, recvbuf, count,
                                             datatype, op, inplace, schedule, tmpbuf1, tmpbuf2);
    }
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        free(tmpbuf);
        return res;
    }

    res = NBC_Sched_commit(schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
       OBJ_RELEASE(schedule);
       free(tmpbuf);
       return res;
    }

#ifdef NBC_CACHE_SCHEDULE
        /* save schedule to tree */
        args = (NBC_Scan_args *) malloc (sizeof (args));
        if (NULL != args) {
            args->sendbuf = sendbuf;
            args->recvbuf = recvbuf;
            args->count = count;
            args->datatype = datatype;
            args->op = op;
            args->schedule = schedule;
            res = hb_tree_insert ((hb_tree *) libnbc_module->NBC_Dict[NBC_EXSCAN], args, args, 0);
            if (0 == res) {
                OBJ_RETAIN(schedule);

                /* increase number of elements for A2A */
                if (++libnbc_module->NBC_Dict_size[NBC_EXSCAN] > NBC_SCHED_DICT_UPPER) {
                    NBC_SchedCache_dictwipe ((hb_tree *) libnbc_module->NBC_Dict[NBC_EXSCAN],
                                             &libnbc_module->NBC_Dict_size[NBC_EXSCAN]);
                }
            } else {
                NBC_Error("error in dict_insert() (%i)", res);
                free (args);
            }
        }
    } else {
        /* found schedule */
        schedule = found->schedule;
        OBJ_RETAIN(schedule);
    }
#endif

    res = NBC_Schedule_request(schedule, comm, libnbc_module, persistent, request, tmpbuf);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        free(tmpbuf);
        return res;
    }

    return OMPI_SUCCESS;
}

int ompi_coll_libnbc_iexscan(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                             struct ompi_communicator_t *comm, ompi_request_t ** request,
                             mca_coll_base_module_t *module) {
    int res = nbc_exscan_init(sendbuf, recvbuf, count, datatype, op,
                              comm, request, module, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }
  
    res = NBC_Start(*(ompi_coll_libnbc_request_t **)request);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (*(ompi_coll_libnbc_request_t **)request);
        *request = &ompi_request_null.request;
        return res;
    }

    return OMPI_SUCCESS;
}

int ompi_coll_libnbc_exscan_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                 struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                 mca_coll_base_module_t *module) {
    int res = nbc_exscan_init(sendbuf, recvbuf, count, datatype, op,
                              comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}

/*
 * exscan_sched_linear:
 *
 * Function:  Linear algorithm for exclusive scan.
 * Accepts:   Same as MPI_Iexscan
 * Returns:   MPI_SUCCESS or error code
 *
 * Working principle:
 * 1. Each process (but process 0) receives from left neighbor
 * 2. Performs op
 * 3. All but rank p - 1 do sends to it's right neighbor and exits
 *
 * Schedule length: O(1)
 */
static inline int exscan_sched_linear(
    int rank, int comm_size, const void *sendbuf, void *recvbuf, int count,
    MPI_Datatype datatype,  MPI_Op op, char inplace, NBC_Schedule *schedule,
    void *tmpbuf)
{
    int res = OMPI_SUCCESS;
    ptrdiff_t gap;
    opal_datatype_span(&datatype->super, count, &gap);

    if (rank > 0) {
        if (inplace) {
            res = NBC_Sched_copy(recvbuf, false, count, datatype,
                                 (char *)tmpbuf - gap, false, count, datatype, schedule, false);
        } else {
            res = NBC_Sched_copy((void *)sendbuf, false, count, datatype,
                                 (char *)tmpbuf - gap, false, count, datatype, schedule, false);
        }
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

        res = NBC_Sched_recv(recvbuf, false, count, datatype, rank - 1, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

        if (rank < comm_size - 1) {
            /* We have to wait until we have the data */
            res = NBC_Sched_barrier(schedule);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            res = NBC_Sched_op(recvbuf, false, (void *)(-gap), true, count,
                               datatype, op, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            /* Send reduced data onward */
            res = NBC_Sched_send ((void *)(-gap), true, count, datatype, rank + 1, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
        }
    } else if (comm_size > 1) {
        /* Process 0 */
        if (inplace) {
            res = NBC_Sched_send(recvbuf, false, count, datatype, 1, schedule, false);
        } else {
            res = NBC_Sched_send(sendbuf, false, count, datatype, 1, schedule, false);
        }
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
    }

cleanup_and_return:
    return res;
}

/*
 * exscan_sched_recursivedoubling:
 *
 * Function:  Recursive doubling algorithm for exclusive scan.
 * Accepts:   Same as MPI_Iexscan
 * Returns:   MPI_SUCCESS or error code
 *
 * Description:  Implements recursive doubling algorithm for MPI_Iexscan.
 *               The algorithm preserves order of operations so it can
 *               be used both by commutative and non-commutative operations.
 *
 * Example for 5 processes and commutative operation MPI_SUM:
 * Process:  0                 1             2              3               4
 * recvbuf:  -                 -             -              -               -
 *   psend: [0]               [1]           [2]            [3]             [4]
 *
 *  Step 1:
 * recvbuf:  -                [0]            -             [2]              -
 *   psend: [1+0]             [0+1]         [3+2]          [2+3]           [4]
 *
 *  Step 2:
 * recvbuf:  -                [0]            [1+0]          [(0+1)+2]       -
 *   psend: [(3+2)+(1+0)]     [(2+3)+(0+1)]  [(1+0)+(3+2)]  [(1+0)+(2+3)]  [4]
 *
 *  Step 3:
 * recvbuf:  -                [0]            [1+0]          [(0+1)+2]      [(3+2)+(1+0)]
 *   psend: [4+((3+2)+(1+0))]                                              [((3+2)+(1+0))+4]
 *
 * Time complexity (worst case): \ceil(\log_2(p))(2\alpha + 2m\beta + 2m\gamma)
 * Memory requirements (per process): 2 * count * typesize = O(count)
 * Limitations: intra-communicators only
 * Schedule length: O(log(p))
 */
static inline int exscan_sched_recursivedoubling(
    int rank, int comm_size, const void *sendbuf, void *recvbuf, int count,
    MPI_Datatype datatype, MPI_Op op, char inplace,
    NBC_Schedule *schedule, void *tmpbuf1, void *tmpbuf2)
{
    int res = OMPI_SUCCESS;
    char *psend = (char *)tmpbuf1;
    char *precv = (char *)tmpbuf2;

    if (!inplace) {
        res = NBC_Sched_copy((char *)sendbuf, false, count, datatype,
                             psend, true, count, datatype, schedule, true);
    } else {
        res = NBC_Sched_copy((char *)recvbuf, false, count, datatype,
                             psend, true, count, datatype, schedule, true);
    }
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

    int is_commute = ompi_op_is_commute(op);
    int is_first_block = 1;

    for (int mask = 1; mask < comm_size; mask <<= 1) {
        int remote = rank ^ mask;
        if (remote < comm_size) {
            res = NBC_Sched_send(psend, true, count, datatype, remote, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
            res = NBC_Sched_recv(precv, true, count, datatype, remote, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            if (rank > remote) {
                /* Assertion: rank > 0 and rbuf is valid */
                if (is_first_block) {
                    res = NBC_Sched_copy(precv, true, count, datatype,
                                         recvbuf, false, count, datatype, schedule, false);
                    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
                    is_first_block = 0;
                } else {
                    /* Accumulate prefix reduction: recvbuf = precv <op> recvbuf */
                    res = NBC_Sched_op(precv, true, recvbuf, false, count,
                                       datatype, op, schedule, false);
                    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
                }
                /* Partial result: psend = precv <op> psend */
                res = NBC_Sched_op(precv, true, psend, true, count,
                                   datatype, op, schedule, true);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
            } else {
                if (is_commute) {
                    /* psend = precv <op> psend */
                    res = NBC_Sched_op(precv, true, psend, true, count,
                                       datatype, op, schedule, true);
                    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
                } else {
                    /* precv = psend <op> precv */
                    res = NBC_Sched_op(psend, true, precv, true, count,
                                       datatype, op, schedule, true);
                    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
                    char *tmp = psend;
                    psend = precv;
                    precv = tmp;
                }
            }
        }
    }

cleanup_and_return:
    return res;
}
