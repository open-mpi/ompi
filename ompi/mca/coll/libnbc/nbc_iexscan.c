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
    MPI_Aint ext;
    NBC_Schedule *schedule;
#ifdef NBC_CACHE_SCHEDULE
    NBC_Scan_args *args, *found, search;
#endif
    char inplace;
    NBC_Handle *handle;
    ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
    ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

    NBC_IN_PLACE(sendbuf, recvbuf, inplace);

    res = NBC_Init_handle(comm, coll_req, libnbc_module);
    if (res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }

    handle = (*coll_req);

    res = MPI_Comm_rank(comm, &rank);
    if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
    res = MPI_Comm_size(comm, &p);
    if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_size() (%i)\n", res); return res; }
    res = MPI_Type_extent(datatype, &ext);
    if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }

    if (inplace && rank < p - 1)
        /* need more buffer space for the inplace case */
        handle->tmpbuf = malloc(ext * count * 2);
    else
        handle->tmpbuf = malloc(ext * count);

    if (handle->tmpbuf == NULL) { printf("Error in malloc()\n"); return NBC_OOR; }

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
            if (inplace && rank < p - 1)
                /* if sendbuf == recvbuf do not clobber the send buffer until it has been combined
                 * with the incoming data. */
                res = NBC_Sched_recv((void *)(ext * count), true, count, datatype, rank-1, schedule);
            else
                res = NBC_Sched_recv(recvbuf, false, count, datatype, rank-1, schedule);

            if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

            if (rank < p - 1) {
                /* we have to wait until we have the data */
                res = NBC_Sched_barrier(schedule);
                if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

                /* perform the reduce in my temporary buffer */
                if (inplace)
                    res = NBC_Sched_op(0, true, sendbuf, false, (void *)(ext * count), true, count, datatype, op, schedule);
                else
                    res = NBC_Sched_op(0, true, sendbuf, false, recvbuf, false, count, datatype, op, schedule);
                if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_op() (%i)\n", res); return res; }

                /* this cannot be done until handle->tmpbuf is unused :-( */
                res = NBC_Sched_barrier(schedule);
                if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

                /* send reduced data onward */
                res = NBC_Sched_send(0, true, count, datatype, rank + 1, schedule);
                if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_send() (%i)\n", res); return res; }

                if (inplace)
                    /* copy the received data into the receive buffer */
                    NBC_Sched_copy ((void *)(ext * count), true, count, datatype, recvbuf, false, count, datatype, schedule);
            }
        } else if (p > 1) {
            res = NBC_Sched_send(sendbuf, false, count, datatype, 1, schedule);
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
