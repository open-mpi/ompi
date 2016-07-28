/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

#ifdef NBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int NBC_Scan_args_compare(NBC_Scan_args *a, NBC_Scan_args *b, void *param) {

    if( (a->sendbuf == b->sendbuf) &&
        (a->recvbuf == b->recvbuf) &&
        (a->count == b->count) &&
        (a->datatype == b->datatype) &&
        (a->op == b->op) ) {
        return  0;
    }
    if( a->sendbuf < b->sendbuf ) {
        return -1;
    }
    return +1;
}
#endif

/* linear iexscan
 * working principle:
 * 1. each node (but node 0) receives from left neigbor
 * 2. performs op
 * 3. all but rank p-1 do sends to it's right neigbor and exits
 *
 */
int ompi_coll_libnbc_iexscan(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                             struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_0_0_t *module) {
    int rank, p, res;
    ptrdiff_t gap, span;
    NBC_Schedule *schedule;
#ifdef NBC_CACHE_SCHEDULE
    NBC_Scan_args *args, *found, search;
#endif
    char inplace;
    NBC_Handle *handle;
    ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
    ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

    NBC_IN_PLACE(sendbuf, recvbuf, inplace);

    rank = ompi_comm_rank (comm);
    p = ompi_comm_size (comm);

    res = NBC_Init_handle(comm, coll_req, libnbc_module);
    if (res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }

    handle = (*coll_req);

    span = opal_datatype_span(&datatype->super, count, &gap);
    if (0 < rank) {
        handle->tmpbuf = malloc(span);
        if (handle->tmpbuf == NULL) { printf("Error in malloc()\n"); return NBC_OOR; }
        if (inplace) {
            res = NBC_Copy(recvbuf, count, datatype, (char *)handle->tmpbuf-gap, count, datatype, comm);
        } else {
            res = NBC_Copy(sendbuf, count, datatype, (char *)handle->tmpbuf-gap, count, datatype, comm);
        }
    }

#ifdef NBC_CACHE_SCHEDULE
    fprintf (stderr, "NBC_CACHE_SCHEDULE\n");
    /* search schedule in communicator specific tree */
    search.sendbuf=sendbuf;
    search.recvbuf=recvbuf;
    search.count=count;
    search.datatype=datatype;
    search.op=op;
    found = (NBC_Scan_args*)hb_tree_search((hb_tree*)handle->comminfo->NBC_Dict[NBC_EXSCAN], &search);
    if (found == NULL) {
#endif
        schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
        if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

        res = NBC_Sched_create(schedule);
        if (res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

        if (rank != 0) {
            res = NBC_Sched_recv (recvbuf, false, count, datatype, rank-1, schedule);

            if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

            if (rank < p - 1) {
                /* we have to wait until we have the data */
                res = NBC_Sched_barrier(schedule);
                if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

                res = NBC_Sched_op (recvbuf, false, (void *)(-gap), true, count, datatype, op, schedule);
                if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_op() (%i)\n", res); return res; }

                res = NBC_Sched_barrier(schedule);
                if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

                /* send reduced data onward */
                res = NBC_Sched_send ((void *)(-gap), true, count, datatype, rank + 1, schedule);
                if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
            }
        } else if (p > 1) {
            if (inplace) {
              res = NBC_Sched_send (recvbuf, false, count, datatype, 1, schedule);
            } else {
              res = NBC_Sched_send (sendbuf, false, count, datatype, 1, schedule);
            }
            if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
        }

        res = NBC_Sched_commit(schedule);
        if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

#ifdef NBC_CACHE_SCHEDULE
        /* save schedule to tree */
        args = (NBC_Scan_args*)malloc(sizeof(NBC_Alltoall_args));
        args->sendbuf=sendbuf;
        args->recvbuf=recvbuf;
        args->count=count;
        args->datatype=datatype;
        args->op=op;
        args->schedule=schedule;
        res = hb_tree_insert ((hb_tree*)handle->comminfo->NBC_Dict[NBC_EXSCAN], args, args, 0);
        if(res != 0) printf("error in dict_insert() (%i)\n", res);
        /* increase number of elements for A2A */
        if(++handle->comminfo->NBC_Dict_size[NBC_EXSCAN] > NBC_SCHED_DICT_UPPER) {
            NBC_SchedCache_dictwipe((hb_tree*)handle->comminfo->NBC_Dict[NBC_EXSCAN], &handle->comminfo->NBC_Dict_size[NBC_EXSCAN]);
        }
    } else {
        /* found schedule */
        schedule=found->schedule;
    }
#endif

    res = NBC_Start(handle, schedule);
    if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Start() (%i)\n", res); return res; }

    /* tmpbuf is freed with the handle */
    return NBC_OK;
}
