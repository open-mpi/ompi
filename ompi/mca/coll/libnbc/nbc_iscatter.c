/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

#ifdef NBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int NBC_Scatter_args_compare(NBC_Scatter_args *a, NBC_Scatter_args *b, void *param) {
    if( (a->sendbuf == b->sendbuf) &&
        (a->sendcount == b->sendcount) &&
        (a->sendtype == b->sendtype) &&
        (a->recvbuf == b->recvbuf) &&
        (a->recvcount == b->recvcount) &&
        (a->recvtype == b->recvtype) &&
        (a->root == b->root) ) {
        return  0;
    }
    if( a->sendbuf < b->sendbuf ) {
        return -1;
    }
    return +1;
}
#endif

/* simple linear MPI_Iscatter */
int ompi_coll_libnbc_iscatter(void* sendbuf, int sendcount, MPI_Datatype sendtype,
                              void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                              struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_0_0_t *module) {
  int rank, p, res, i;
  MPI_Aint sndext = 0;
  NBC_Schedule *schedule;
  char *sbuf, inplace;
#ifdef NBC_CACHE_SCHEDULE
  NBC_Scatter_args *args, *found, search;
#endif
  NBC_Handle *handle;
  ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  res = NBC_Init_handle(comm, coll_req, libnbc_module);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  handle = (*coll_req);
  res = MPI_Comm_rank(comm, &rank);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Comm_size(comm, &p);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_size() (%i)\n", res); return res; }
  if (rank == root) {
    res = MPI_Type_extent(sendtype, &sndext);
    if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }
  }

  handle->tmpbuf=NULL;

  if((rank == root) && (!inplace)) {
    sbuf = ((char *)sendbuf) + (rank*sendcount*sndext);
    /* if I am the root - just copy the message (not for MPI_IN_PLACE) */
    res = NBC_Copy(sbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm);
    if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
  }

#ifdef NBC_CACHE_SCHEDULE
  /* search schedule in communicator specific tree */
  search.sendbuf=sendbuf;
  search.sendcount=sendcount;
  search.sendtype=sendtype;
  search.recvbuf=recvbuf;
  search.recvcount=recvcount;
  search.recvtype=recvtype;
  search.root=root;
  found = (NBC_Scatter_args*)hb_tree_search((hb_tree*)handle->comminfo->NBC_Dict[NBC_SCATTER], &search);
  if(found == NULL) {
#endif
    schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
    if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

    res = NBC_Sched_create(schedule);
    if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

    /* receive from root */
    if(rank != root) {
      /* recv msg from root */
      res = NBC_Sched_recv(recvbuf, false, recvcount, recvtype, root, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    } else {
      for(i=0;i<p;i++) {
        sbuf = ((char *)sendbuf) + (i*sendcount*sndext);
        if(i != root) {
          /* root sends the right buffer to the right receiver */
          res = NBC_Sched_send(sbuf, false, sendcount, sendtype, i, schedule);
          if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
        }
      }
    }

    res = NBC_Sched_commit(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }
#ifdef NBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = (NBC_Scatter_args*)malloc(sizeof(NBC_Scatter_args));
    args->sendbuf=sendbuf;
    args->sendcount=sendcount;
    args->sendtype=sendtype;
    args->recvbuf=recvbuf;
    args->recvcount=recvcount;
    args->recvtype=recvtype;
    args->root=root;
    args->schedule=schedule;
          res = hb_tree_insert ((hb_tree*)handle->comminfo->NBC_Dict[NBC_SCATTER], args, args, 0);
    if(res != 0) printf("error in dict_insert() (%i)\n", res);
    /* increase number of elements for A2A */
    if(++handle->comminfo->NBC_Dict_size[NBC_SCATTER] > NBC_SCHED_DICT_UPPER) {
      NBC_SchedCache_dictwipe((hb_tree*)handle->comminfo->NBC_Dict[NBC_SCATTER], &handle->comminfo->NBC_Dict_size[NBC_SCATTER]);
    }
  } else {
    /* found schedule */
    schedule=found->schedule;
  }
#endif


  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }

  return NBC_OK;
}

int ompi_coll_libnbc_iscatter_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype,
                                    void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                                    struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_0_0_t *module) {
    int rank, res, i, rsize;
    MPI_Aint sndext;
    NBC_Schedule *schedule;
    char *sbuf;
    NBC_Handle *handle;
    ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
    ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

    res = NBC_Init_handle(comm, coll_req, libnbc_module);
    if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
    handle = (*coll_req);
    res = MPI_Comm_rank(comm, &rank);
    if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
    if (MPI_ROOT == root) {
        res = MPI_Type_extent(sendtype, &sndext);
        if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }
    }
    res = MPI_Comm_remote_size (comm, &rsize);
    if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_remote_size() (%i)\n", res); return res; }

    handle->tmpbuf = NULL;

    schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
    if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

    res = NBC_Sched_create(schedule);
    if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

    /* receive from root */
    if (MPI_ROOT != root && MPI_PROC_NULL != root) {
        /* recv msg from remote root */
        res = NBC_Sched_recv(recvbuf, false, recvcount, recvtype, root, schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    } else if (MPI_ROOT == root) {
        for (i = 0 ; i < rsize ; ++i) {
            sbuf = ((char *)sendbuf) + (i * sendcount * sndext);
            /* root sends the right buffer to the right receiver */
            res = NBC_Sched_send(sbuf, false, sendcount, sendtype, i, schedule);
            if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
        }
    }

    res = NBC_Sched_commit(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

    res = NBC_Start(handle, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }

    return NBC_OK;
}
