/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
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
int NBC_Gather_args_compare(NBC_Gather_args *a, NBC_Gather_args *b, void *param) {

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

int ompi_coll_libnbc_igather(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                             MPI_Datatype recvtype, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_0_0_t *module) {
  int rank, p, res, i;
  MPI_Aint rcvext = 0;
  NBC_Schedule *schedule;
  char *rbuf, inplace;
#ifdef NBC_CACHE_SCHEDULE
  NBC_Gather_args *args, *found, search;
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
      res = MPI_Type_extent(recvtype, &rcvext);
      if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }
  }
  handle->tmpbuf = NULL;

  if (inplace) {
      sendcount = recvcount;
      sendtype = recvtype;
  } else if (rank == root) {
    rbuf = ((char *)recvbuf) + (rank*recvcount*rcvext);
    /* if I am the root - just copy the message (only without MPI_IN_PLACE) */
    res = NBC_Copy(sendbuf, sendcount, sendtype, rbuf, recvcount, recvtype, comm);
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
  found = (NBC_Gather_args*)hb_tree_search((hb_tree*)handle->comminfo->NBC_Dict[NBC_GATHER], &search);
  if(found == NULL) {
#endif
    schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
    if (NULL == schedule) { printf("Error in malloc() (%i)\n", res); return res; }

    res = NBC_Sched_create(schedule);
    if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

    /* send to root */
    if(rank != root) {
      /* send msg to root */
      res = NBC_Sched_send(sendbuf, false, sendcount, sendtype, root, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    } else {
      for(i=0;i<p;i++) {
        rbuf = ((char *)recvbuf) + (i*recvcount*rcvext);
        if(i != root) {
          /* root receives message to the right buffer */
          res = NBC_Sched_recv(rbuf, false, recvcount, recvtype, i, schedule);
          if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
        }
      }
    }

    res = NBC_Sched_commit(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

#ifdef NBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = (NBC_Gather_args*)malloc(sizeof(NBC_Gather_args));
    args->sendbuf=sendbuf;
    args->sendcount=sendcount;
    args->sendtype=sendtype;
    args->recvbuf=recvbuf;
    args->recvcount=recvcount;
    args->recvtype=recvtype;
    args->root=root;
    args->schedule=schedule;
          res = hb_tree_insert ((hb_tree*)handle->comminfo->NBC_Dict[NBC_GATHER], args, args, 0);
    if(res != 0) printf("error in dict_insert() (%i)\n", res);
    /* increase number of elements for A2A */
    if(++handle->comminfo->NBC_Dict_size[NBC_GATHER] > NBC_SCHED_DICT_UPPER) {
      NBC_SchedCache_dictwipe((hb_tree*)handle->comminfo->NBC_Dict[NBC_GATHER], &handle->comminfo->NBC_Dict_size[NBC_GATHER]);
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

int ompi_coll_libnbc_igather_inter (void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                    MPI_Datatype recvtype, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_0_0_t *module) {
    int rank, p, res, i, rsize;
    MPI_Aint rcvext = 0;
    NBC_Schedule *schedule;
    char *rbuf;
    NBC_Handle *handle;
    ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
    ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

    res = NBC_Init_handle(comm, coll_req, libnbc_module);
    if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
    handle = (*coll_req);
    res = MPI_Comm_rank(comm, &rank);
    if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
    res = MPI_Comm_size(comm, &p);
    if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_size() (%i)\n", res); return res; }
    res = MPI_Comm_remote_size (comm, &rsize);
    if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_remote_size() (%i)\n", res); return res; }

    if (root == MPI_ROOT) {
        res = MPI_Type_extent(recvtype, &rcvext);
        if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }
    }
    handle->tmpbuf = NULL;

    schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
    if (NULL == schedule) { printf("Error in malloc() (%i)\n", res); return res; }

    res = NBC_Sched_create(schedule);
    if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

    /* send to root */
    if(root != MPI_ROOT && root != MPI_PROC_NULL) {
        /* send msg to root */
        res = NBC_Sched_send(sendbuf, false, sendcount, sendtype, root, schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    } else if (MPI_ROOT == root) {
        for (i = 0 ; i < rsize ; ++i) {
            rbuf = ((char *)recvbuf) + (i * recvcount * rcvext);
            /* root receives message to the right buffer */
            res = NBC_Sched_recv(rbuf, false, recvcount, recvtype, i, schedule);
            if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
        }
    }

    res = NBC_Sched_commit(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

    res = NBC_Start(handle, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }

    return NBC_OK;
}
