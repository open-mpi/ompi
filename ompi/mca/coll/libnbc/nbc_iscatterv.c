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

/* a scatterv schedule can not be cached easily because the contents
 * ot the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* simple linear MPI_Iscatterv */
int ompi_coll_libnbc_iscatterv(void* sendbuf, int *sendcounts, int *displs, MPI_Datatype sendtype,
                               void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                               struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_0_0_t *module) {
  int rank, p, res, i;
  MPI_Aint sndext;
  NBC_Schedule *schedule;
  char *sbuf, inplace;
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
  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

  handle->tmpbuf=NULL;

  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

  /* receive from root */
  if(rank != root) {
    /* recv msg from root */
    res = NBC_Sched_recv(recvbuf, false, recvcount, recvtype, root, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
  } else {
    for(i=0;i<p;i++) {
      sbuf = ((char *)sendbuf) + (displs[i]*sndext);
      if(i == root) {
        if(!inplace) {
          /* if I am the root - just copy the message */
          res = NBC_Copy(sbuf, sendcounts[i], sendtype, recvbuf, recvcount, recvtype, comm);
          if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
        }
      } else {
        /* root sends the right buffer to the right receiver */
        res = NBC_Sched_send(sbuf, false, sendcounts[i], sendtype, i, schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
      }
    }
  }

  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }

  return NBC_OK;
}

int ompi_coll_libnbc_iscatterv_inter (void* sendbuf, int *sendcounts, int *displs, MPI_Datatype sendtype,
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
    res = MPI_Comm_remote_size(comm, &rsize);
    if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_remote_size() (%i)\n", res); return res; }

    handle->tmpbuf = NULL;

    schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
    if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

    res = NBC_Sched_create(schedule);
    if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

    /* receive from root */
    if (MPI_ROOT != root && MPI_PROC_NULL != root) {
        /* recv msg from root */
        res = NBC_Sched_recv(recvbuf, false, recvcount, recvtype, root, schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    } else if (MPI_ROOT == root) {
        for (i = 0 ; i < rsize ; ++i) {
            sbuf = ((char *)sendbuf) + (displs[i] * sndext);
            /* root sends the right buffer to the right receiver */
            res = NBC_Sched_send(sbuf, false, sendcounts[i], sendtype, i, schedule);
            if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
        }
    }

    res = NBC_Sched_commit(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

    res = NBC_Start(handle, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }

    return NBC_OK;
}
