/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

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

/* linear iexscan
 * working principle:
 * 1. each node (but node 0) receives from left neigbor
 * 2. performs op
 * 3. all but rank p-1 do sends to it's right neigbor and exits
 *
 */
int ompi_coll_libnbc_iexscan(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                             struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_1_0_t *module) {
    int rank, p, res;
    MPI_Aint ext;
    NBC_Schedule *schedule;
#ifdef NBC_CACHE_SCHEDULE
    NBC_Scan_args *args, *found, search;
#endif
    char inplace;
    NBC_Handle *handle;
    ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

    NBC_IN_PLACE(sendbuf, recvbuf, inplace);

    rank = ompi_comm_rank (comm);
    p = ompi_comm_size (comm);

    res = ompi_datatype_type_extent(datatype, &ext);
    if (MPI_SUCCESS != res) {
        NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
        return res;
    }

    res = NBC_Init_handle(comm, &handle, libnbc_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    if (inplace && rank < p - 1) {
        /* need more buffer space for the inplace case */
        handle->tmpbuf = malloc(ext * count * 2);
    } else {
        handle->tmpbuf = malloc(ext * count);
    }

    if (handle->tmpbuf == NULL) {
        NBC_Return_handle (handle);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

#ifdef NBC_CACHE_SCHEDULE
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
            NBC_Return_handle (handle);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* make sure the schedule is released with the handle on error */
        handle->schedule = schedule;

        if (rank != 0) {
            if (inplace && rank < p - 1) {
                /* if sendbuf == recvbuf do not clobber the send buffer until it has been combined
                 * with the incoming data. */
                res = NBC_Sched_recv ((void *) (ext * count), true, count, datatype, rank-1, schedule, false);
            } else {
                res = NBC_Sched_recv (recvbuf, false, count, datatype, rank-1, schedule, false);
            }

            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                NBC_Return_handle (handle);
                return res;
            }

            if (rank < p - 1) {
                /* we have to wait until we have the data */
                res = NBC_Sched_barrier(schedule);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                    NBC_Return_handle (handle);
                    return res;
                }

                /* perform the reduce in my temporary buffer */
                /* this cannot be done until handle->tmpbuf is unused :-( so barrier after */
                if (inplace) {
                    res = NBC_Sched_op (0, true, sendbuf, false, (void *)(ext * count), true, count,
                                        datatype, op, schedule, true);
                } else {
                    res = NBC_Sched_op (0, true, sendbuf, false, recvbuf, false, count, datatype, op,
                                        schedule, true);
                }

                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                    NBC_Return_handle (handle);
                    return res;
                }

                /* send reduced data onward */
                res = NBC_Sched_send (0, true, count, datatype, rank + 1, schedule, false);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                    NBC_Return_handle (handle);
                    return res;
                }

                if (inplace) {
                    /* copy the received data into the receive buffer */
                    res = NBC_Sched_copy ((void *)(ext * count), true, count, datatype, recvbuf,
                                          false, count, datatype, schedule, false);
                    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                        NBC_Return_handle (handle);
                        return res;
                    }
                }
            }
        } else if (p > 1) {
            res = NBC_Sched_send (sendbuf, false, count, datatype, 1, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                NBC_Return_handle (handle);
                return res;
            }
        }

        res = NBC_Sched_commit(schedule);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
            NBC_Return_handle (handle);
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

    res = NBC_Start (handle, schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return res;
    }

    *request = (ompi_request_t *) handle;

    /* tmpbuf is freed with the handle */
    return OMPI_SUCCESS;
}
