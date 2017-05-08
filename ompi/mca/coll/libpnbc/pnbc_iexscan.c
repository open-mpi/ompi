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
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "pnbc_internal.h"

#ifdef PNBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int PNBC_Scan_args_compare(PNBC_Scan_args *a, PNBC_Scan_args *b, void *param) {
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
int ompi_coll_libpnbc_iexscan(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                             struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_2_0_t *module) {
    int rank, p, res;
    ptrdiff_t gap, span;
    PNBC_Schedule *schedule;
#ifdef PNBC_CACHE_SCHEDULE
    PNBC_Scan_args *args, *found, search;
#endif
    char inplace;
    PNBC_Handle *handle;
    ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

    PNBC_IN_PLACE(sendbuf, recvbuf, inplace);

    rank = ompi_comm_rank (comm);
    p = ompi_comm_size (comm);

    res = PNBC_Init_handle(comm, &handle, libpnbc_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    span = opal_datatype_span(&datatype->super, count, &gap);
    if (0 < rank) {
        handle->tmpbuf = malloc(span);
        if (handle->tmpbuf == NULL) {
            PNBC_Return_handle (handle);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        if (inplace) {
            res = PNBC_Copy(recvbuf, count, datatype, (char *)handle->tmpbuf-gap, count, datatype, comm);
        } else {
            res = PNBC_Copy(sendbuf, count, datatype, (char *)handle->tmpbuf-gap, count, datatype, comm);
        }
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
            PNBC_Return_handle (handle);
            return res;
        }
    }

#ifdef PNBC_CACHE_SCHEDULE
    /* search schedule in communicator specific tree */
    search.sendbuf = sendbuf;
    search.recvbuf = recvbuf;
    search.count = count;
    search.datatype = datatype;
    search.op = op;
    found = (PNBC_Scan_args *) hb_tree_search ((hb_tree *) libpnbc_module->PNBC_Dict[PNBC_EXSCAN], &search);
    if (NULL == found) {
#endif
        schedule = OBJ_NEW(PNBC_Schedule);
        if (OPAL_UNLIKELY(NULL == schedule)) {
            PNBC_Return_handle (handle);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* make sure the schedule is released with the handle on error */
        handle->schedule = schedule;

        if (rank != 0) {
            res = PNBC_Sched_recv (recvbuf, false, count, datatype, rank-1, schedule, false);

            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                PNBC_Return_handle (handle);
                return res;
            }

            if (rank < p - 1) {
                /* we have to wait until we have the data */
                res = PNBC_Sched_barrier(schedule);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                    PNBC_Return_handle (handle);
                    return res;
                }

                res = PNBC_Sched_op (recvbuf, false, (void *)(-gap), true, count,
                                     datatype, op, schedule, true);

                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                    PNBC_Return_handle (handle);
                    return res;
                }

                /* send reduced data onward */
                res = PNBC_Sched_send ((void *)(-gap), true, count, datatype, rank + 1, schedule, false);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                    PNBC_Return_handle (handle);
                    return res;
                }
            }
        } else if (p > 1) {
            if (inplace) {
              res = PNBC_Sched_send (recvbuf, false, count, datatype, 1, schedule, false);
            } else {
              res = PNBC_Sched_send (sendbuf, false, count, datatype, 1, schedule, false);
            }
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                PNBC_Return_handle (handle);
                return res;
            }
        }

        res = PNBC_Sched_commit(schedule);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
            PNBC_Return_handle (handle);
            return res;
        }

#ifdef PNBC_CACHE_SCHEDULE
        /* save schedule to tree */
        args = (PNBC_Scan_args *) malloc (sizeof (args));
        if (NULL != args) {
            args->sendbuf = sendbuf;
            args->recvbuf = recvbuf;
            args->count = count;
            args->datatype = datatype;
            args->op = op;
            args->schedule = schedule;
            res = hb_tree_insert ((hb_tree *) libpnbc_module->PNBC_Dict[PNBC_EXSCAN], args, args, 0);
            if (0 == res) {
                OBJ_RETAIN(schedule);

                /* increase number of elements for A2A */
                if (++libpnbc_module->PNBC_Dict_size[PNBC_EXSCAN] > PNBC_SCHED_DICT_UPPER) {
                    PNBC_SchedCache_dictwipe ((hb_tree *) libpnbc_module->PNBC_Dict[PNBC_EXSCAN],
                                             &libpnbc_module->PNBC_Dict_size[PNBC_EXSCAN]);
                }
            } else {
                PNBC_Error("error in dict_insert() (%i)", res);
                free (args);
            }
        }
    } else {
        /* found schedule */
        schedule = found->schedule;
        OBJ_RETAIN(schedule);
    }
#endif

    res = PNBC_Start (handle, schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        PNBC_Return_handle (handle);
        return res;
    }

    *request = (ompi_request_t *) handle;

    /* tmpbuf is freed with the handle */
    return OMPI_SUCCESS;
}
